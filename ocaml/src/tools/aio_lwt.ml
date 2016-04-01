(*
Copyright 2015 iNuron NV

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

open Lwt.Infix
open Lwt_bytes2

let make_context n = Aio.context n

let init context =
  let fd = Aio.fd context in
  let lwt_fd = Lwt_unix.of_unix_file_descr
                 ~blocking:false
                 ~set_flags:true
                 fd
  in
  let bs = Bytes.create 8 in
  let rec inner () =
    Lwt_unix.read lwt_fd bs 0 8 >>= fun _ ->
    let () = Aio.run context in
    inner ()
  in
  Lwt.ignore_result (inner ())

let page_size = Aio.Buffer.page_size ()

let cast_buffer_to_lwt_bytes (bs : Aio.Buffer.t) : Lwt_bytes.t =
  Obj.magic bs

let pread context fd offset length =
  let ufd = Lwt_unix.unix_file_descr fd in
  let t, u = Lwt.wait () in
  let start_page = offset / page_size in
  let start_page_offset = start_page * page_size in
  let start_offset = offset - start_page_offset in
  let end_offset = offset + length in
  let end_page = (end_offset - 1) / page_size in
  let pages = end_page - start_page + 1 in
  let buf = Aio.Buffer.create (pages * page_size) in
  let buf' = cast_buffer_to_lwt_bytes buf in
  Lwt.catch
    (fun () ->
     let () =
       Aio.read
         context
         ufd
         (Int64.of_int start_page_offset)
         buf
         (let open Aio in
          function
          | Result buf -> Lwt.wakeup u ()
          | Errno err -> Lwt.wakeup_exn u (Aio.Error err)
          | Partial (buf, len) ->
             if len = start_page_offset + length
             then Lwt.wakeup u () (* read up to the bytes we're interested in *)
             else Lwt.wakeup_exn u (Failure "aio pread partial error"))
     in
     t >>= fun () ->
     let res =
       Bigstring_slice.from_bigstring
         buf'
         start_offset
         length
     in
     Lwt.return res)
    (fun exn ->
     Lwt_bytes.unsafe_destroy buf';
     Lwt.fail exn)

let default_context =
  let r = make_context 65536 in
  let () = init r in
  r


let read_done result =
  let buffer = Aio.result result in
  let i64 = Aio.Buffer.get_int64 buffer 0 in
  let i32 = Aio.Buffer.get_int32 buffer 8 in
  let i31 = Aio.Buffer.get_int31 buffer 12 in
  let i16 = Aio.Buffer.get_int16 buffer 16 in
  let u16 = Aio.Buffer.get_uint16 buffer 18 in
  let i8 = Aio.Buffer.get_int8 buffer 20 in
  let u8 = Aio.Buffer.get_uint8 buffer 21
  in
    Printf.printf " int64 = %Lx\n" i64;
    Printf.printf " int32 = %lx\n" i32;
    Printf.printf " int31 = %x\n" i31;
    Printf.printf " int16 = %d\n" i16;
    Printf.printf "uint16 = %d\n" u16;
    Printf.printf " int8  = %d\n" i8;
    Printf.printf "uint8  = %d\n" u8;
    flush_all ()

let write_done ctx fd result =
  let buffer = Aio.result result
  in
    Printf.printf "write done\n";
    flush_all ();
    Aio.Buffer.clear buffer;
    Printf.printf "reading\n";
    flush_all ();
    Aio.read ctx fd Int64.zero buffer read_done

let () = at_exit Gc.full_major

let () =
  let fd = Unix.openfile "testfile" [Unix.O_RDWR; Unix.O_CREAT] 0o664 in
  let buffer = Aio.Buffer.create (Aio.Buffer.page_size ()) in
  Printf.printf "page_size = %i \n" (Aio.Buffer.page_size ());
  let ctx = Aio.context 32
  in
    Aio.Buffer.clear buffer;
    Aio.Buffer.set_int64 buffer 0 0x123456789abcdefL;
    Aio.Buffer.set_int32 buffer 8 0x12345678l;
    Aio.Buffer.set_int31 buffer 12 0x12345678;
    Aio.Buffer.set_int16 buffer 16 (-12345);
    Aio.Buffer.set_uint16 buffer 18 12345;
    Aio.Buffer.set_int8 buffer 20 (-23);
    Aio.Buffer.set_uint8 buffer 21 42;

    Printf.printf "writing\n";
    flush_all ();
    Aio.write ctx fd Int64.zero buffer (write_done ctx fd);
    Aio.run ctx;
    (* Create error of pending IO on exit *)
    Aio.read ctx fd Int64.zero buffer read_done
