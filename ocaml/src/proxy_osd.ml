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
open Slice
open Lwt.Infix
open Alba_based_osd

class t
        (proxy_client : Proxy_client.proxy_client)
        ~long_id
        ~prefix ~preset_name
        ~namespace_name_format
  =
  let to_namespace_name = to_namespace_name prefix namespace_name_format in
  let get_kvs ~consistent_read namespace =
    object(self :# Osd.key_value_storage)
      method get_option _prio name =
        let object_name = Slice.get_string_unsafe name in
        proxy_client # read_objects
                     ~namespace
                     ~object_names:[ object_name; ]
                     ~consistent_read
                     ~should_cache:(not consistent_read)
                     (fun (_, vs) ->
                       match vs with
                       | [ Some (mf, v); ] -> Lwt.return (Some (Bigstring_slice.extract_to_bigstring v))
                       | [ None; ] -> Lwt.return_none
                       | _ -> assert false)

      method get_exn prio name =
        self # get_option prio name >>= function
        | None -> Lwt.fail_with
                    (Printf.sprintf
                       "Could not get value for key %S"
                       (Slice.get_string_unsafe name))
        | Some v -> Lwt.return v

      method multi_get prio names =
        proxy_client # read_objects
                     ~namespace
                     ~object_names:(List.map
                                      Slice.get_string_unsafe
                                      names)
                     ~consistent_read
                     ~should_cache:(not consistent_read)
                     (fun (_, values) ->
                       List.map
                         (Option.map (fun (mf, v) -> Bigstring_slice.extract_to_bigstring v))
                         values
                       |> Lwt.return)

      method multi_exists _prio names =
        proxy_client # multi_exists
                     ~namespace
                     ~object_names:(List.map Slice.get_string_unsafe names)

      method range _prio ~first ~finc ~last ~reverse ~max =
        proxy_client # list_object
                     ~namespace
                     ~first:(Slice.get_string_unsafe first) ~finc
                     ~last:(Option.map
                              (fun (l, linc) -> Slice.get_string_unsafe l, linc)
                              last)
                     ~reverse ~max
        >>= fun ((cnt, names), has_more) ->
        Lwt.return ((cnt, List.map Slice.wrap_string names), has_more)

      method range_entries prio ~first ~finc ~last ~reverse ~max =
        self # range
             prio
             ~first ~finc ~last
             ~reverse ~max
        >>= fun ((cnt, object_names), has_more) ->
        proxy_client # read_objects
                     ~namespace
                     ~object_names:(List.map Slice.get_string_unsafe object_names)
                     ~consistent_read
                     ~should_cache:(not consistent_read)
                     (fun (_, vos) ->
                       Lwt_list.map_s
                         (function
                          | None -> Lwt.fail_with "object missing in range entries"
                          | Some (mf, v) ->
                             Lwt.return (Bigstring_slice.extract_to_bigstring v,
                                         mf.Nsm_model.Manifest.checksum))
                         vos) >>= fun res ->
        Lwt.return ((cnt,
                     List.map2
                       (fun object_name (v, cs) -> object_name, v, cs)
                       object_names
                       res),
                    has_more)

      method partial_get _prio name object_slices =
        Lwt.catch
          (fun () ->
            proxy_client # read_object_slices
                         ~namespace
                         ~object_slices:[ Slice.get_string_unsafe name,
                                          List.map
                                            (fun (offset, len, _, _) -> Int64.of_int offset, len)
                                            object_slices; ]
                         ~consistent_read >>= fun data ->
            let () =
              List.fold_left
                (fun total_offset (_, len, dst, dstoff) ->
                  Lwt_bytes.blit_from_bytes
                    data
                    total_offset
                    dst
                    dstoff
                    len;
                  total_offset + len)
                0
                object_slices
              |> ignore
            in
            Lwt.return Osd.Success)
          (let open Proxy_protocol.Protocol.Error in
           function
             | Exn (ObjectDoesNotExist, _) -> Lwt.return Osd.NotFound
             | exn -> Lwt.fail exn)

      method apply_sequence prio asserts updates =
        Lwt.catch
          (fun () ->
            proxy_client
              # read_objects
              ~namespace
              ~object_names:(List.map
                               (function
                                | Osd.Assert.Value (key, _) -> Slice.get_string_unsafe key)
                               asserts)
              ~consistent_read:true
              ~should_cache:false
              (fun (_, values) ->
                List.map2
                  (fun assert_ mf_v_o ->
                    let open Proxy_protocol.Protocol in
                    match assert_, mf_v_o with
                    | Osd.Assert.Value (key, None), None ->
                       Lwt.return (Assert.ObjectDoesNotExist (Slice.get_string_unsafe key))
                    | Osd.Assert.Value (key, None), Some _
                      | Osd.Assert.Value (key, Some _), None ->
                       Osd.Error.(lwt_fail (Assert_failed (Slice.get_string_unsafe key)))
                    | Osd.Assert.Value (key, Some v), Some (mf, v') ->
                       Lwt.return (Assert.ObjectHasId (Slice.get_string_unsafe key,
                                                       mf.Nsm_model.Manifest.object_id)))
                  asserts values
                |> Lwt_list.map_s Std.id
              )
            >>= fun asserts' ->

            proxy_client # apply_sequence
                         ~write_barrier:false
                         ~namespace
                         ~asserts:asserts'
                         ~updates:(List.map
                                     (let open Proxy_protocol.Protocol in
                                      function
                                      | Osd.Update.Set (key, None) ->
                                         Update.DeleteObject (Slice.get_string_unsafe key)
                                      | Osd.Update.Set (key, Some (blob, cs, _)) ->
                                         Update.UploadObject (Slice.get_string_unsafe key,
                                                              Osd.Blob.get_bigstring_slice blob,
                                                              Some cs)
                                     )
                                     updates)
            >>= fun (_, _) ->
            Lwt.return Osd.Ok)
          (function
           | Proxy_protocol.Protocol.Error.Exn (Proxy_protocol.Protocol.Error.AssertFailed, key) ->
              Lwt.return Osd.(Exn (Error.Assert_failed (Option.get_some key)))
           | exn ->
              Lwt.fail exn)
    end
  in
  object(self :# Osd.osd)

    method global_kvs =
      get_kvs ~consistent_read:true prefix

    method namespace_kvs namespace_id =
      get_kvs ~consistent_read:false (to_namespace_name namespace_id)

    method add_namespace namespace_id =
      let namespace = to_namespace_name namespace_id in
      proxy_client # list_namespaces
                   ~first:namespace ~finc:true
                   ~last:(Some (namespace, true))
                   ~max:1 ~reverse:false
      >>= fun ((_, x), _) ->
      match x with
      | [] -> proxy_client # create_namespace ~namespace ~preset_name
      | [ _; ] -> Lwt.return ()
      | _ -> assert false

    method delete_namespace namespace_id _ =
      let namespace = to_namespace_name namespace_id in
      proxy_client # list_namespaces
                   ~first:namespace ~finc:true
                   ~last:(Some (namespace, true))
                   ~max:1 ~reverse:false
      >>= fun ((_, x), _) ->
      match x with
      | [] ->
         Lwt.return_none
      | [ _; ] ->
         proxy_client # delete_namespace ~namespace >>= fun () ->
         Lwt.return_none
      | _ -> assert false

    method set_full _ = failwith "grmbl this method doesn't belong here."
    method get_version = proxy_client # get_version
    method get_long_id = long_id
    method get_disk_usage = Lwt.return
                              (1000L, 2000L)
    (* (failwith "TODO return sth based on asd disk usage") *)
    method capabilities = Lwt.return (0, [])
  end
