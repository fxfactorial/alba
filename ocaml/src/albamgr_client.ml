(*
Copyright 2015 Open vStorage NV

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open Prelude
open Lwt
open Albamgr_protocol
open Protocol

class type basic_client = object
  method query : 'i 'o.
                 ?consistency : Consistency.t ->
                 ('i, 'o) query -> 'i -> 'o Lwt.t

  method update : 'i 'o.
                  ('i, 'o) update -> 'i -> 'o Lwt.t
end

class client (client : basic_client) =
  object(self)

    method list_nsm_hosts ~first ~finc ~last ~max ~reverse =
      client # query
        ListNsmHosts
        RangeQueryArgs.{ first; finc; last; max; reverse; }

    method get_nsm_host ~nsm_host_id =
      self # list_nsm_hosts
        ~first:nsm_host_id ~finc:true
        ~last:(Some (nsm_host_id, true))
        ~max:1 ~reverse:false >>= fun ((_, nsm_hosts), _) ->
      Lwt.return (List.hd nsm_hosts)

    method list_all_nsm_hosts () =
      list_all_x
        ~first:""
        (fun (id, _, _) -> id)
        (self # list_nsm_hosts ~last:None ~max:(-1) ~reverse:false)

    method list_namespaces_by_id ~first ~finc ~last ~max=
      let args = RangeQueryArgs.{
                   first;
                   finc;
                   last;
                   reverse = false;
                   max;
                 }
      in
      client # query ListNamespacesById args

    method add_nsm_host ~nsm_host_id ~nsm_host_info =
      client # update
        AddNsmHost
        (nsm_host_id, nsm_host_info)

    method update_nsm_host ~nsm_host_id ~nsm_host_info =
      client # update
        UpdateNsmHost
        (nsm_host_id, nsm_host_info)

    method create_namespace ~namespace ~preset_name ?nsm_host_id () =
      begin match nsm_host_id with
        | None ->
          self # list_all_nsm_hosts () >>= fun (count,r) ->

          let active_nsm_hosts =
            List.filter
              (fun (_, info, _) -> not info.Nsm_host.lost)
              r
          in

          if active_nsm_hosts = [] then failwith "0 active nsm hosts";

          let id, _, _ =
            List.fold_left
              (fun ((_, _, bc) as best) ((_, _, cc) as cur) ->
                 if cc < bc
                 then cur
                 else best)
              (List.hd_exn active_nsm_hosts)
              (List.tl_exn active_nsm_hosts)
          in

          Lwt.return id
        | Some nsm_host_id ->
          Lwt.return nsm_host_id
      end >>= fun nsm_host_id ->
      client # update
        CreateNamespace
        (namespace, nsm_host_id, preset_name)

    method delete_namespace ~namespace =
      client # update DeleteNamespace namespace

    method list_namespaces ~first ~finc ~last ~max ~reverse =
      client # query
        ListNamespaces
        RangeQueryArgs.{ first; finc; last; max; reverse; }

    method get_namespace ~namespace =
      self # list_namespaces
        ~first:namespace ~finc:true ~last:(Some (namespace, true))
        ~max:1 ~reverse:false >>= fun ((_, namespaces), _) ->
      Lwt.return (List.hd namespaces)

    method get_namespace_by_id ~namespace_id =
      self # list_namespaces_by_id
        ~first:namespace_id
        ~finc:true
        ~last:(Some (namespace_id,true))
        ~max:1 >>= fun ((_, namespaces),_) ->
      match namespaces with
      | [] -> Error.(failwith_lwt Error.Namespace_does_not_exist)
      | [x] -> Lwt.return x
      | _ -> Lwt.fail_with "nou moe"


    method list_all_namespaces =
      list_all_x
        ~first:""
        fst
        (self # list_namespaces
           ~last:None
           ~max:(-1) ~reverse:false)

    method list_namespace_osds ~namespace_id ~first ~finc ~last ~max ~reverse =
      client # query
        ListNamespaceOsds
        (namespace_id,
         RangeQueryArgs.({
             first; finc; last;
             max; reverse;
           }))

    method list_all_namespace_osds ~namespace_id =
      list_all_x
        ~first:0l
        fst
        (self # list_namespace_osds
           ~namespace_id
           ~last:None
           ~max:(-1) ~reverse:false)

    method create_preset preset_name preset =
      client # update
        CreatePreset
        (preset_name, preset)

    method set_default_preset preset_name =
      client # update
        SetDefaultPreset
        preset_name

    method delete_preset preset_name =
      client # update DeletePreset preset_name

    method list_presets ~first ~finc ~last ~reverse ~max =
      client # query
        ListPresets
        RangeQueryArgs.({ first; finc; last; reverse; max; })

    method list_all_presets () =
      list_all_x
        ~first:""
        (fun (name, _, _, _) -> name)
        (self # list_presets
           ~last:None
           ~reverse:false ~max:(-1))

    method get_preset ~preset_name =
      self # list_presets
        ~first:preset_name ~finc:true ~last:(Some(preset_name, true))
        ~max:1 ~reverse:false >>= fun ((_, presets), _) ->
      Lwt.return (List.hd presets)

    method add_osds_to_preset ~preset_name ~osd_ids =
      client # update
        AddOsdsToPreset
        (preset_name, (List.length osd_ids, osd_ids))

    method list_osds_by_osd_id ~first ~finc ~last ~reverse ~max =
      client # query
        ListOsdsByOsdId
        RangeQueryArgs.({ first; finc; last; reverse; max; })

    method get_osd_by_osd_id ~osd_id =
      self # list_osds_by_osd_id
        ~first:osd_id ~finc:true
        ~last:(Some(osd_id, true))
        ~max:1 ~reverse:false >>= fun ((_, osds), _) ->
      Lwt.return (Option.map snd (List.hd osds))

    method list_osds_by_long_id ~first ~finc ~last ~reverse ~max =
      client # query
        ListOsdsByLongId
        RangeQueryArgs.({ first; finc; last; reverse; max; })

    method get_osd_by_long_id ~long_id =
      self # list_osds_by_long_id
        ~first:long_id ~finc:true ~last:(Some(long_id, true))
        ~max:1 ~reverse:false >>= fun ((_, osds), _) ->
      Lwt.return (List.hd osds)

    method list_all_osds =
      list_all_x
        ~first:""
        (fun (_, osd_info) -> Osd.get_long_id osd_info.Osd.kind)
        (self # list_osds_by_long_id
           ~last:None
           ~reverse:false ~max:(-1))

    method list_all_claimed_osds =
      list_all_x
        ~first:0l
        fst
        (self # list_osds_by_osd_id
           ~last:None
           ~reverse:false ~max:(-1))

    method list_available_osds =
      client # query ListAvailableOsds ()

    method add_osd osd_info =
      client # update AddOsd osd_info

    method update_osd ~long_id changes =
      client # update UpdateOsd (long_id, changes)

    method decommission_osd ~long_id =
      client # update DecommissionOsd long_id

    method mark_osd_claimed ~long_id =
      client # update MarkOsdClaimed long_id

    method mark_osd_claimed_by_other ~long_id ~alba_id =
      client # update MarkOsdClaimedByOther (long_id, alba_id)

    method add_work_repair_fragment
      ~namespace_id ~object_id ~object_name
      ~chunk_id ~fragment_id ~version_id =
      client # update
        AddWork
        (1,
         [ Work.RepairBadFragment (namespace_id,
                                   object_id,
                                   object_name,
                                   chunk_id,
                                   fragment_id,
                                   version_id) ])

    method get_work ~first ~finc ~last ~max ~reverse =
      client # query
        GetWork
        GetWorkParams.({ first; finc; last;
                         max; reverse; })

    method mark_work_completed ~work_id =
      client # update MarkWorkCompleted work_id

    method store_client_config ccfg =
      client # update StoreClientConfig ccfg

    method get_client_config =
      client # query
        ~consistency:Consistency.No_guarantees
        GetClientConfig ()

    method get_next_msgs : type dest msg.
      (dest, msg) Msg_log.t -> dest ->
      (Albamgr_protocol.Protocol.Msg_log.id * msg) counted_list_more Lwt.t =
      fun t dest ->
        client # query (GetNextMsgs t) dest

    method mark_msg_delivered : type dest msg.
      (dest, msg) Msg_log.t -> dest ->
      Albamgr_protocol.Protocol.Msg_log.id -> unit Lwt.t =
      fun t dest msg_id ->
        client # update (MarkMsgDelivered t) (dest, msg_id)

    method get_alba_id =
      client # query GetAlbaId ()

    method recover_namespace ~namespace ~nsm_host_id =
      client # update
        RecoverNamespace
        (namespace, nsm_host_id)

    method get_version =
      client # query GetVersion ()

    method check_can_claim ~long_id =
      client # query CheckClaimOsd long_id

    method list_osd_namespaces ~osd_id ~first ~finc ~last ~reverse ~max =
      client # query
        ListOsdNamespaces
        (osd_id,
         RangeQueryArgs.({
             first; finc; last;
             max; reverse;
           }))

    method list_all_osd_namespaces ~osd_id =
      list_all_x
        ~first:0l
        Std.id
        (self # list_osd_namespaces
           ~osd_id
           ~last:None
           ~max:(-1) ~reverse:false)

    method list_decommissioning_osds ~first ~finc ~last ~reverse ~max =
      client # query
        ListDecommissioningOsds
        RangeQueryArgs.({
            first; finc; last;
            max; reverse;
          })

    method list_all_decommissioning_osds =
      list_all_x
        ~first:0l
        fst
        (self # list_decommissioning_osds
           ~last:None
           ~max:(-1) ~reverse:false)
  end

class single_connection_client (ic, oc) =
  let read_response deserializer =
    Llio.input_string ic >>= fun res_s ->
    let res_buf = Llio.make_buffer res_s 0 in
    match Llio.int_from res_buf with
    | 0 ->
      Lwt.return (deserializer res_buf)
    | ierr ->
      let err = Error.int2err ierr in
      Lwt_log.debug_f "albamgr operation failed: %i %s" ierr (Error.show err)
      >>= fun () ->
      let payload = Llio.string_from res_buf in
      Lwt.fail (Error.Albamgr_exn (err, payload))
  in
  (object
    method query : 'i 'o.
           ?consistency:Consistency.t ->
           ('i, 'o) Albamgr_protocol.Protocol.query -> 'i -> 'o Lwt.t =
      fun ?(consistency = Consistency.Consistent) command req ->
        let buf = Buffer.create 20 in
        let tag = command_to_tag (Wrap_q command) in
        Llio.int32_to buf tag;
        Consistency.to_buffer buf consistency;
        Lwt_log.debug_f "albamgr_client: %s" (tag_to_name tag) >>= fun ()->
        write_query_i command buf req;
        Lwt_extra2.llio_output_and_flush oc (Buffer.contents buf) >>= fun () ->
        read_response (read_query_o command)

    method update : 'i 'o. ('i, 'o) Albamgr_protocol.Protocol.update -> 'i -> 'o Lwt.t =
      fun command req ->
        let buf = Buffer.create 20 in
        let tag = command_to_tag (Wrap_u command) in
        Llio.int32_to buf tag;
        Lwt_log.debug_f "albamgr_client: %s" (tag_to_name tag) >>= fun ()->
        write_update_i command buf req;
        Lwt_extra2.llio_output_and_flush oc (Buffer.contents buf) >>= fun () ->
        read_response (read_update_o command)
  end : basic_client)

let wrap_around (ara_c:Arakoon_client.client) =
  ara_c # user_hook "albamgr" >>= fun (ic, oc) ->
  Llio.input_int32 ic
  >>= function
  | 0l -> begin
      Lwt_log.debug_f "user hook was found%!" >>= fun () ->
      let client = new single_connection_client (ic, oc) in
      Lwt.return client
    end
  | e ->
     let rc = Arakoon_exc.rc_of_int32 e in
     Llio.input_string ic >>= fun msg ->
     Lwt.fail (Arakoon_exc.Exception (rc, msg))

let wrap_around' ara_c =
  wrap_around ara_c >>= fun c ->
  Lwt.return (new client c)

let make_client (ccfg:Arakoon_client_config.t) =
  let open Client_helper in
  find_master' ~tls:None ccfg >>= function
  | MasterLookupResult.Found (m , ncfg) ->
     let open Arakoon_client_config in
     Networking2.first_connection ncfg.ips ncfg.port >>= fun conn ->
     let closer = Networking2.closer conn in
     Lwt.catch
       (fun () ->
          Arakoon_remote_client.make_remote_client
            ccfg.cluster_id
            conn >>= fun client ->
          wrap_around (client:Arakoon_client.client))
       (fun exn ->
          closer () >>= fun () ->
          Lwt.fail exn)>>= fun c ->
     Lwt.return (c,
                 m,
                 (fun () ->
                    Lwt_log.debug_f "closing albamgr" >>= fun () ->
                    Networking2.closer conn ()))
  | r -> Lwt.fail (Client_helper.MasterLookupResult.Error r)

let with_client cfg f =
  Lwt.catch
    (fun () ->
       Client_helper.with_master_client'
         ~tls:None
         (Albamgr_protocol.Protocol.Arakoon_config.to_arakoon_client_cfg cfg)
         (fun c ->
          wrap_around c
          >>= fun wc ->
          f wc)
    )
    (function
      | Protocol.Error.Albamgr_exn (err, _) as exn ->
        Lwt_log.debug_f
          "albamgr client failed with %s"
          (Protocol.Error.show err) >>= fun () ->
        Lwt.fail exn
      | Client_helper.MasterLookupResult.Error err as exn ->
        Lwt_log.debug_f
          "albamgr client failed with %s"
          (Client_helper.MasterLookupResult.to_string err) >>= fun () ->
        Lwt.fail exn
      | Arakoon_exc.Exception (rc, msg) as exn ->
        Lwt_log.debug_f
          "albamgr client failed with %s: %s"
          (Arakoon_exc.string_of_rc rc) msg >>= fun () ->
        Lwt.fail exn
      | exn ->
        Lwt_log.info_f ~exn "albamgr client failed with %s" (Printexc.to_string exn)
        >>= fun () ->
        Lwt.fail exn)

let with_client' cfg f =
  with_client
    cfg
    (fun c -> f (new client c))