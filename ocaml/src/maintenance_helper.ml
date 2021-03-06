(*
Copyright (C) 2016 iNuron NV

This file is part of Open vStorage Open Source Edition (OSE), as available from


    http://www.openvstorage.org and
    http://www.openvstorage.com.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License v3 (GNU AGPLv3)
as published by the Free Software Foundation, in version 3 as it comes
in the <LICENSE.txt> file of the Open vStorage OSE distribution.

Open vStorage is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY of any kind.
*)

open Prelude
open Nsm_model
open Recovery_info
open Lwt.Infix

let choose_new_devices
      (osd_access : Osd_access_type.t)
      osds_info_cache'
      osds_to_keep
      no_fragments_to_be_repaired
  =
  (* update osds_info_cache' to make sure it contains all
   * osds that will be force chosen *)
  Lwt_list.iter_p
    (fun osd_id ->
     osd_access # get_osd_info ~osd_id >>= fun (osd_info, _, _) ->
     Hashtbl.replace osds_info_cache' osd_id osd_info;
     Lwt.return ())
    osds_to_keep >>= fun () ->

  let extra_devices =
    Choose.choose_extra_devices
      no_fragments_to_be_repaired
      osds_info_cache'
      osds_to_keep
  in
  Lwt.return extra_devices

let _upload_missing_fragments
      osd_access
      osds_info_cache'
      ok_fragments
      all_fragments
      fragments_to_be_repaired
      ~namespace_id
      manifest
      ~chunk_id
      ~version_id
      ~gc_epoch
      compression
      encryption
      fragment_checksum_algo
      ~is_replication
      ~n_chunks ~chunk_location
  =

  let ok_fragments' =
    List.map_filter_rev
      Std.id
      ok_fragments
  in

  choose_new_devices
    osd_access
    osds_info_cache'
    ok_fragments'
    (List.length fragments_to_be_repaired)
  >>= fun extra_devices ->

  let live_ones =
    let open Nsm_model in
    Hashtbl.fold
      (fun id (info:OsdInfo.t) acc ->
       if info.OsdInfo.decommissioned
       then acc else id :: acc
      )
      osds_info_cache' []
  in
  let extra_device_ids = List.map fst extra_devices in
  let show = [%show: int32 list] in
  Lwt_log.debug_f
    "extra_devices: live_ones:%s ok=%s extra:%s"
    (show live_ones)
    ([%show : int32 option list] ok_fragments)
    (show extra_device_ids)
  >>= fun () ->

  let () =
    (* sanity check *)
    List.iter
      (fun extra_id ->
       assert (not (List.mem extra_id ok_fragments'))
      )
      extra_device_ids
  in

  let to_be_repaireds =
    List.map2
      (fun fragment_id (osd_id, _) -> (fragment_id, osd_id))
      fragments_to_be_repaired
      extra_devices in

  let object_info_o =
    let is_last_chunk = chunk_id = n_chunks - 1 in
    if is_last_chunk
    then Some RecoveryInfo.({
                               storage_scheme = manifest.Manifest.storage_scheme;
                               size = manifest.Manifest.size;
                               checksum = manifest.Manifest.checksum;
                               timestamp = manifest.Manifest.timestamp;
                             })
    else None
  in

  let object_id = manifest.Manifest.object_id in
  RecoveryInfo.make
    manifest.Manifest.name
    object_id
    object_info_o
    encryption
    (List.nth_exn manifest.Manifest.chunk_sizes chunk_id)
    (List.nth_exn manifest.Manifest.fragment_packed_sizes chunk_id)
    (List.nth_exn manifest.Manifest.fragment_checksums chunk_id)
  >>= fun recovery_info_slice ->

  Lwt_list.map_p
    (fun ((fragment_id, checksum), chosen_osd_id) ->
     let fragment_ba = List.nth_exn all_fragments fragment_id in
     Fragment_helper.pack_fragment
       (Bigstring_slice.wrap_bigstring fragment_ba)
       ~object_id ~chunk_id ~fragment_id
       ~ignore_fragment_id:is_replication
       compression
       encryption
       fragment_checksum_algo
     >>= fun (packed_fragment, _, _, checksum') ->

     if checksum = checksum'
     then
       begin
         Alba_client_upload.upload_packed_fragment_data
           osd_access
           ~namespace_id
           ~osd_id:chosen_osd_id
           ~object_id ~version_id
           ~chunk_id ~fragment_id
           ~packed_fragment
           ~gc_epoch ~checksum
           ~recovery_info_blob:(Osd.Blob.Slice recovery_info_slice)
         >>= fun () ->
         Lwt.return (fragment_id, chosen_osd_id)
       end
     else
       begin
         let msg =
           Printf.sprintf
             "Error while repairing object (this should never happen): %s <> %s"
             (Checksum.show checksum)
             (Checksum.show checksum')
         in
         Lwt_log.warning msg >>= fun () ->
         Lwt.fail_with msg
       end)
    to_be_repaireds

let upload_missing_fragments
      osd_access
      osds_info_cache'
      ~namespace_id
      manifest
      ~chunk_id
      ~version_id
      ~gc_epoch
      compression
      encryption
      fragment_checksum_algo
      ~k
      ~problem_fragments ~problem_osds
      ~n_chunks ~chunk_location
      ~with_chunk_data
  =

  let problem_osds =
    (* always repair fragments of all disqualified osds too *)
    Hashtbl.fold
      (fun osd_id (_, state, _) acc ->
        if Osd_state.disqualified state
        then Int32Set.add osd_id acc
        else acc)
      (osd_access # osds_info_cache)
      problem_osds
  in

  let _, ok_fragments, fragments_to_be_repaired =
    List.fold_left
      (fun (fragment_id, ok_fragments, to_be_repaireds)
           ((fragment_osd_id_o, fragment_version_id), fragment_checksum) ->
        let ok_fragments', to_be_repaireds' =
          if List.mem (chunk_id, fragment_id) problem_fragments ||
               (match fragment_osd_id_o with
                | None -> (fragment_id < k) (* repair missing data fragments *)
                | Some osd_id -> Int32Set.mem osd_id problem_osds)
          then
            ok_fragments,
            (fragment_id, fragment_checksum) :: to_be_repaireds
          else
            fragment_osd_id_o :: ok_fragments,
            to_be_repaireds in
        fragment_id + 1, ok_fragments', to_be_repaireds')
      (0, [], [])
      chunk_location in

  if fragments_to_be_repaired = []
  then Lwt.return []
  else
    with_chunk_data
      (fun data_fragments coding_fragments ->
        let all_fragments = List.append data_fragments coding_fragments in
        _upload_missing_fragments
          osd_access
          osds_info_cache'
          ok_fragments
          all_fragments
          fragments_to_be_repaired
          ~namespace_id
          manifest
          ~chunk_id
          ~version_id
          ~gc_epoch
          compression
          encryption
          fragment_checksum_algo
          ~is_replication:(k=1)
          ~n_chunks ~chunk_location)
