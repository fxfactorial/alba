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
open Nsm_model


module Osd_pool = struct
    module Osd' = Osd
    open Albamgr_protocol.Protocol
    open Nsm_model
    type osd_pool = (Osd'.osd * (unit -> unit Lwt.t)) Lwt_pool2.t

    type t = {
        pools : (Osd.id, osd_pool) Hashtbl.t;
        get_osd_kind : Osd.id -> OsdInfo.kind Lwt.t;
        pool_size : int;
        buffer_pool : Buffer_pool.t;
        tls_config: Tls.t option;
        make_alba_osd_client : Alba_arakoon.Config.t ref ->
                               tls_config:Tls.t Ppx_deriving_runtime.option ->
                               tcp_keepalive:Tcp_keepalive.t ->
                               prefix:string ->
                               preset_name:Albamgr_protocol.Protocol.Preset.name option ->
                               (Osd'.osd * (unit -> unit Lwt.t)) Lwt.t;
      }

    let make ~size ~get_osd_kind buffer_pool tls_config make_alba_osd_client =
      let pools = Hashtbl.create 0 in
      { pools;
        get_osd_kind;
        pool_size = size;
        buffer_pool;
        tls_config;
        make_alba_osd_client;
      }

    let factory tls_config buffer_pool make_alba_osd_client =
      function
      | OsdInfo.Asd (conn_info', asd_id) ->
         let () =
           Lwt_log.ign_debug_f
             "factory: conn_info':%s"
             ([%show :Nsm_model.OsdInfo.conn_info] conn_info')
         in
         let conn_info = Asd_client.conn_info_from ~tls_config conn_info' in

         Asd_client.make_client buffer_pool ~conn_info (Some asd_id)
         >>= fun (asd, closer) ->
         let key_value_osd = new Asd_client.asd_osd asd_id asd in
         let osd = new Osd'.osd_wrap_key_value_osd key_value_osd in
         Lwt.return (osd, closer)
      | OsdInfo.Kinetic (conn_info', kinetic_id) ->
         let conn_info = Asd_client.conn_info_from ~tls_config conn_info' in
         Kinetic_client.make_client buffer_pool ~conn_info kinetic_id >>= fun (client, closer) ->
         let osd = new Osd'.osd_wrap_key_value_osd client in
         Lwt.return (osd, closer)
      | OsdInfo.Alba { Nsm_model.OsdInfo.cfg; id; prefix; preset; } ->
         make_alba_osd_client
           (ref cfg)
           ~tls_config
           ~tcp_keepalive:Tcp_keepalive2.default (* TODO *)
           ~prefix ~preset_name:(Some preset)

    let use_osd t ~(osd_id:int32) f =
      let get_pool () =
        try Hashtbl.find t.pools osd_id |> Lwt.return
        with Not_found ->
          begin
            t.get_osd_kind osd_id >>= fun kind ->
            let factory () =
              factory t.tls_config t.buffer_pool t.make_alba_osd_client kind
            in
            let p =
              Lwt_pool2.create
                t.pool_size
                ~check:(fun _ exn ->
                        (* TODO some exns shouldn't invalidate the connection *)
                        false)
                ~factory
                ~cleanup:(fun (_, closer) -> closer ())
            in
            Hashtbl.add t.pools osd_id p;
            Lwt.return p
          end
      in
      get_pool () >>= fun pool ->
      Lwt_pool2.use
        pool
        (fun p -> f (fst p))

    let invalidate t ~osd_id =
      if Hashtbl.mem t.pools osd_id
      then begin
          Lwt_pool2.finalize (Hashtbl.find t.pools osd_id) |> Lwt.ignore_result;
          Hashtbl.remove t.pools osd_id
        end

    let invalidate_all t =
      List.iter
        (fun (osd_id, _) -> invalidate t ~osd_id)
        (Hashtbl.to_assoc_list t.pools)
  end

let osd_buffer_pool = Buffer_pool.osd_buffer_pool

class osd_access
        mgr_access
        ~osd_connection_pool_size
        ~osd_timeout
        ~default_osd_priority
        ~tls_config
        make_alba_osd_client
  =
  let () = Lwt_log.ign_debug_f "osd_access ... tls_config:%s" ([%show : Tls.t option] tls_config) in
  let osd_long_id_claim_info = ref StringMap.empty in

  let osds_info_cache =
    let open Albamgr_protocol.Protocol in
    (Hashtbl.create 3
     : (Osd.id,
        OsdInfo.t * Osd_state.t) Hashtbl.t) in
  let add_osd_info ~osd_id osd_info =
    if not (Hashtbl.mem osds_info_cache osd_id)
    then
      begin
        let open Albamgr_protocol.Protocol in
        let osd_state = Osd_state.make () in
        let info' = (osd_info, osd_state) in
        Hashtbl.replace osds_info_cache osd_id info';
        let long_id = OsdInfo.(get_long_id osd_info.kind) in
        osd_long_id_claim_info :=
          StringMap.add
            long_id (Osd.ClaimInfo.ThisAlba osd_id)
            !osd_long_id_claim_info;

        Lwt_extra2.run_forever
            "refresh_osd_total_used"
            (fun () ->
             let osd_info,osd_state = Hashtbl.find osds_info_cache osd_id in
             Osd_pool.factory
               tls_config
               osd_buffer_pool
               make_alba_osd_client
               osd_info.OsdInfo.kind
             >>= fun (osd, closer) ->

             Lwt.finalize
               (fun () ->
                osd # get_disk_usage >>= fun ((used,total ) as disk_usage) ->
                Osd_state.add_disk_usage osd_state disk_usage;
                let osd_info' = OsdInfo.{ osd_info with used;total } in
                let () = Hashtbl.replace osds_info_cache osd_id (osd_info', osd_state) in
                Lwt.return ())
               closer)
          60.
        |> Lwt.ignore_result
      end
  in
  let get_osd_info ~osd_id =
    try Lwt.return (Hashtbl.find osds_info_cache osd_id)
    with Not_found ->
      mgr_access # get_osd_by_osd_id ~osd_id >>= fun osd_o ->
      let osd_info = match osd_o with
        | None -> failwith (Printf.sprintf "could not find osd with id %li" osd_id)
        | Some info -> info
      in
      add_osd_info ~osd_id  osd_info;
      Lwt.return (Hashtbl.find osds_info_cache osd_id)
  in
  let osds_pool =
    Osd_pool.make
      ~size:osd_connection_pool_size
      ~get_osd_kind:(fun osd_id ->
                     get_osd_info ~osd_id >>= fun (osd_info, _) ->
                     let open Nsm_model.OsdInfo in
                     Lwt.return osd_info.kind)
      osd_buffer_pool
      tls_config
      make_alba_osd_client
  in

  let refresh_osd_info ~osd_id =
    mgr_access # get_osd_by_osd_id ~osd_id >>= function
    | None -> assert false
    | Some osd_info' ->
       let osd_info, osd_state = Hashtbl.find osds_info_cache osd_id in
       Hashtbl.replace osds_info_cache osd_id (osd_info', osd_state);
       let open Nsm_model.OsdInfo in
       Lwt.return (osd_info.kind <> osd_info'.kind)
  in

  let rec with_osd_from_pool
          : type a.
                 osd_id:Albamgr_protocol.Protocol.Osd.id ->
                        (Osd.osd -> a Lwt.t) -> a Lwt.t
  = fun ~osd_id f ->
  Lwt.catch
    (fun () ->
     Osd_pool.use_osd
       osds_pool
       ~osd_id
       f)
    (fun exn ->
     get_osd_info ~osd_id >>= fun (_, state) ->

     let add_to_errors =
       let open Asd_protocol.Protocol.Error in
       match exn with
       | End_of_file
       | Exn Assert_failed _ -> false
       | _ -> true
     in
     let should_invalidate_pool =
       match exn with
       | Asd_client.BadLongId _
       (* TODO need a similar detection + exception for the kinetics *)
       | _ when Networking2.is_connection_failure_exn exn ->
          (* don't want this osd to be chosen for now *)
          true
       | exn ->
          false
     in

     (if should_invalidate_pool
      then
        begin
          Osd_pool.invalidate osds_pool ~osd_id;
          refresh_osd_info ~osd_id
        end
      else
        Lwt.return false)
     >>= fun should_retry ->

     if should_retry
     then with_osd_from_pool ~osd_id f
     else
       (if add_to_errors || should_invalidate_pool
        then
          begin
            Osd_state.add_error state exn;
            disqualify ~osd_id ~exn
          end
        else Lwt.return ())
       >>= fun () ->
       Lwt_log.debug_f ~exn "Exception in with_osd_from_pool osd_id=%li" osd_id >>= fun () ->
       Lwt.fail exn)
  and disqualify ~osd_id ~exn =
    let rec inner delay =
      Lwt.catch
        (fun () ->
         mgr_access # get_osd_by_osd_id ~osd_id >>= function
         | None -> Lwt.return `OkAgain
         | Some osd_info ->
            with_osd_from_pool ~osd_id
                               (fun osd ->
                                with_timing_lwt
                                  (fun () ->
                                   osd # global_kvs # apply_sequence
                                       default_osd_priority
                                       []
                                       [ Osd.Update.set_string
                                           Osd_keys.test_key
                                           (Lazy.force Osd_access_type.large_value)
                                           Checksum.NoChecksum
                                           false ])
                                >>= function
                                | (t, Osd.Ok) ->
                                   if t < osd_timeout
                                   then Lwt.return `OkAgain
                                   else Lwt.return (`Continue Lwt_unix.Timeout)
                                | (_, Osd.Exn err) ->
                                   Lwt.return (`Continue (Osd.Error.Exn err))))
        (fun exn -> Lwt.return (`Continue exn))
      >>= function
      | `Continue exn ->
         Lwt_log.info_f
           ~exn
           "Could not yet requalify osd %li, trying again in %f seconds"
           osd_id delay >>= fun () ->
         Lwt_extra2.sleep_approx delay >>= fun () ->
         inner (min (delay *. 1.5) 60.)
      | `OkAgain ->
         Lwt.return ()
    in
    get_osd_info ~osd_id >>= fun (_, state) ->
    if state.Osd_state.disqualified
    then Lwt.return ()
    else begin
        Osd_state.disqualify state true;
        Lwt_log.info_f ~exn "Disqualifying osd %li" osd_id >>= fun () ->
        (* start loop to get it requalified... *)
        Lwt.async
          (fun () ->
           inner 1. >>= fun () ->
           Lwt_log.info_f "Requalified osd %li" osd_id >>= fun () ->
           Osd_state.disqualify state false;
           Osd_state.add_write state;
           Lwt.return ());
        Lwt.return ()
      end
  in

  object(self :# Osd_access_type.t)

    method finalize = Osd_pool.invalidate_all osds_pool

    method with_osd :
             'a. osd_id : Albamgr_protocol.Protocol.Osd.id ->
                          (Osd.osd -> 'a Lwt.t) -> 'a Lwt.t =
      fun ~osd_id f ->
      with_osd_from_pool ~osd_id f

    method get_osd_info = get_osd_info

    method populate_osds_info_cache : unit Lwt.t =
      let rec inner next_osd_id =
        Lwt.catch
          (fun () ->
           mgr_access # list_osds_by_osd_id
                      ~first:next_osd_id ~finc:true
                      ~last:None ~reverse:false
                      ~max:(-1) >>= fun ((cnt, osds), has_more) ->
           List.iter
             (fun (osd_id, osd_info) -> add_osd_info ~osd_id osd_info)
             osds;

           (if has_more
            then Lwt.return_unit
            else Lwt_extra2.sleep_approx 60.) >>= fun () ->

           let next_osd_id' =
             List.last osds
             |> Option.map (fun (osd_id, _) -> Int32.succ osd_id)
             |> Option.get_some_default next_osd_id
           in
           Lwt.return next_osd_id')
          (fun exn ->
           Lwt_log.info_f ~exn "Exception in populate_osds_info_cache" >>= fun () ->
           Lwt_extra2.sleep_approx 60. >>= fun () ->
           Lwt.return next_osd_id) >>= fun next_osd_id' ->
        inner next_osd_id'
      in
      inner 0l

    method get_default_osd_priority = default_osd_priority

    method osds_info_cache = osds_info_cache

    method get_osd_claim_info = !osd_long_id_claim_info
    method osd_timeout = osd_timeout

    method osd_factory osd_info =
      Osd_pool.factory
        tls_config
        osd_buffer_pool
        make_alba_osd_client
        osd_info.Nsm_model.OsdInfo.kind

    method osds_to_osds_info_cache osds =
      let res = Hashtbl.create 0 in
      Lwt_list.iter_p
        (fun osd_id ->
         let open Nsm_model.OsdInfo in
         get_osd_info ~osd_id >>= fun (osd_info, osd_state) ->
         let osd_ok =
           not osd_info.decommissioned
           && Osd_state.osd_ok osd_state
         in
         if osd_ok
         then Hashtbl.add
                res
                osd_id
                osd_info;
         Lwt.return ())
        osds >>= fun () ->
      Lwt.return res

    method propagate_osd_info ?(run_once=false) ?(delay=20.) () : unit Lwt.t =
      let open Nsm_model in
      let open Albamgr_protocol.Protocol in
      let make_update id (osd_info:OsdInfo.t) (osd_state:Osd_state.t) =
        let n = 10 in
        let most_recent xs =
          let my_compare x y = ~-(compare x y) in
          List.merge_head ~compare:my_compare [] xs n
        in
        let open Osd_state in
        let open Nsm_model.OsdInfo in
        let long_id = get_long_id osd_info.kind
        and ips'    = osd_state.ips
        and port'   = osd_state.port
        and total'  = osd_state.total
        and used'   = osd_state.used
        and seen'   = most_recent osd_state.seen
        and read'   = most_recent osd_state.read
        and write'  = most_recent osd_state.write
        and errors' = most_recent osd_state.errors
        and other'  = osd_state.json
        in
        let update = Osd.Update.make
                       ?ips' ?port'
                       ?total' ?used'
                       ?other'
                       ~seen' ~read' ~write' ~errors'
                       ()
        in
        (long_id, update)
      in
      let propagate () =
        let updates =
          Hashtbl.fold
            (fun k v acc ->
             let osd_info, osd_state = v in
             let acc' =
               (* TODO filter out those that accumulated no updates *)
               let update = make_update k osd_info osd_state in
               update :: acc
             in
             let () = Osd_state.reset osd_state in
             acc')
            osds_info_cache
            []
        in
        let n_updates = List.length updates in
        if n_updates > 0
        then
          Lwt_log.debug_f "propagate %i updates" n_updates >>= fun () ->
          mgr_access # update_osds updates
        else
          Lwt.return_unit
      in
      if run_once
      then propagate ()
      else Lwt_extra2.run_forever "propagate_osd_info" propagate delay


    method seen ?(check_claimed=fun id -> false) ?(check_claimed_delay=60.) =
      let open Discovery in
      let determine_conn_info ips port tlsPort use_rdma =
        match port, tlsPort with
        | None     , None  -> failwith "multicast anounced no port ?!"
        | Some port, None  -> (ips,port,false, use_rdma)
        | _        , Some tlsPort ->
           if use_rdma
           then failwith "multicast anounced tls and rdma ?!"
           else (ips,tlsPort, true, false)
      in
      function
      | Bad s ->
         Lwt_log.info_f "Got 'bad' broadcast message: %s" s
      | Good (json, { id; extras; ips; port; tlsPort ; useRdma }) ->

         let open Albamgr_protocol.Protocol in

         let claimed = StringMap.mem (id:string) !osd_long_id_claim_info in
         if claimed
         then
           begin
             let open Osd.ClaimInfo in
             match StringMap.find id !osd_long_id_claim_info with
             | AnotherAlba _
             | Available ->
                Lwt.return ()
             | ThisAlba osd_id ->
                begin
                  get_osd_info ~osd_id >>= fun (osd_info, osd_state) ->
                  Osd_state.add_ips_port osd_state ips port;
                  Osd_state.add_json osd_state json;
                  Osd_state.add_seen osd_state;
                  let conn_info = determine_conn_info ips port tlsPort useRdma in
                  let kind',used',total' =
                    let open OsdInfo in
                    match extras with
                    | None ->
                       Kinetic(conn_info, id), osd_info.used, osd_info.total
                    | Some { used; total; _ } ->
                       Asd(conn_info, id), used, total

                  in
                  let osd_info' =
                    OsdInfo.{ osd_info with
                              kind = kind';
                              total = total'; used = used'
                    }
                  in
                  let () = Osd_state.add_disk_usage osd_state (used', total') in
                  let () =
                    Hashtbl.replace
                      osds_info_cache osd_id
                      (osd_info', osd_state)
                  in
                  let conn_info'  =
                    let open OsdInfo in
                    get_conn_info osd_info.kind
                  in
                  let () =
                    if conn_info' <> conn_info
                    then
                      begin
                        Lwt_log.ign_info_f
                          "Asd (%li) now has new connection info -> invalidating connection pool"
                          osd_id;
                        Osd_pool.invalidate osds_pool ~osd_id
                      end
                  in
                  Lwt.return ()
                end
           end
         else if check_claimed id
         then
           begin
             Lwt_log.debug_f "check_claimed %s => true" id >>= fun () ->
             let conn_info = determine_conn_info ips port tlsPort useRdma in

             let open Nsm_model in
             (match extras with
              | None ->
                 let kind = OsdInfo.Kinetic (conn_info, id) in
                 Osd_pool.factory
                   tls_config
                   osd_buffer_pool
                   make_alba_osd_client
                   kind >>= fun (osd, closer) ->
                 Lwt.finalize
                   (fun () -> osd # get_disk_usage)
                   closer >>= fun (used, total) ->
                 Lwt.return
                   (id, kind,
                    used, total)
              | Some { node_id; version; total; used; } ->
                 Lwt.return
                   (node_id,
                    OsdInfo.Asd (conn_info, id),
                    used, total))
             >>= fun (node_id, kind, used, total) ->

             let osd_info =
               OsdInfo.({
                           kind;
                           decommissioned = false;
                           node_id;
                           other = json;
                           used; total;
                           seen = [ Unix.gettimeofday (); ];
                           read = [];
                           write = [];
                           errors = [];
                         })
             in

             Lwt.catch
               (fun () -> mgr_access # add_osd osd_info)
               (function
                 | Error.Albamgr_exn (Error.Osd_already_exists, _) -> Lwt.return ()
                 | exn -> Lwt.fail exn)
             >>= fun () ->

             let get_claim_info () =
               mgr_access # get_osd_by_long_id ~long_id:id >>= fun osd_o ->
               let claim_info, osd_info = Option.get_some osd_o in
               Lwt.return claim_info
             in

             get_claim_info () >>= fun claim_info ->

             let open Osd.ClaimInfo in
             match claim_info with
             | ThisAlba _
             | AnotherAlba _ ->
                osd_long_id_claim_info := StringMap.add id claim_info !osd_long_id_claim_info;
                Lwt.return ()
             | Available ->
                if not (StringMap.mem id !osd_long_id_claim_info)
                then begin
                    osd_long_id_claim_info := StringMap.add id claim_info !osd_long_id_claim_info;
                    Lwt.ignore_result begin
                        (* start a thread to track it's claimed status
                           and update in the albamgr if needed *)
                        let rec inner () =
                          Lwt.catch
                            (fun () ->
                             Osd_pool.factory
                               tls_config
                               osd_buffer_pool
                               make_alba_osd_client
                               kind >>= fun (osd, closer) ->
                             Lwt.finalize
                               (fun () ->
                                osd # global_kvs # get_option
                                    default_osd_priority
                                    (Slice.wrap_string
                                       (Osd_keys.AlbaInstanceRegistration.instance_log_key 0l)))
                               closer >>= function
                             | None -> Lwt.return `Continue
                             | Some alba_id' ->
                                let alba_id' = Lwt_bytes.to_string alba_id' in
                                mgr_access # get_alba_id >>= fun alba_id ->

                                Lwt.catch
                                  (fun () ->
                                   if alba_id' = alba_id
                                   then begin
                                       mgr_access # mark_osd_claimed ~long_id:id >>= fun _ ->
                                       Lwt.return ()
                                     end else
                                     mgr_access # mark_osd_claimed_by_other
                                                ~long_id:id
                                                ~alba_id:alba_id')
                                  (function
                                    | Error.Albamgr_exn (Error.Osd_already_claimed, _) ->
                                       Lwt.return ()
                                    | exn -> Lwt.fail exn) >>= fun () ->

                                (* get osd_claim_info and store it... *)
                                get_claim_info () >>= fun claim_info ->
                                osd_long_id_claim_info :=
                                  StringMap.add id claim_info !osd_long_id_claim_info;

                                Lwt.return `Stop)
                            (fun exn ->
                             Lwt_log.debug_f
                               ~exn
                               "Error during check osd claimed thread (for %s)"
                               id >>= fun () ->
                             Lwt.return `StopAndRemove) >>= function
                          | `Continue ->
                             Lwt_extra2.sleep_approx check_claimed_delay >>= fun () ->
                             if check_claimed id
                             then inner ()
                             else
                               begin
                                 (* forget we ever saw this osd (if needed it will be picked up again) *)
                                 osd_long_id_claim_info := StringMap.remove id !osd_long_id_claim_info;
                                 Lwt.return ()
                               end

                          | `Stop ->
                             Lwt.return ()
                          | `StopAndRemove ->
                             (* forget we ever saw this osd (if needed it will be picked up again) *)
                             osd_long_id_claim_info := StringMap.remove id !osd_long_id_claim_info;
                             Lwt.return ()
                        in
                        inner ()
                      end;
                    Lwt.return ()
                  end else
                  Lwt.return ()
           end else
             Lwt.return ()

  end
