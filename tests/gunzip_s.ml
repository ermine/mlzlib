open Mlzlib
open Unix

exception Error of string

let chunk = 1024

(* 1952  GZIP file format specification version 4.3 *)

let gunzip_string buf_in f =
   let strm = zlib_inflateInit2 (-max_wbits) in
   let len = String.length buf_in in
   let i = ref (-1) in
   let get_byte () =
      incr i;
      if !i < len then
	 Char.code buf_in.[!i]
      else
	 raise (Error "premature end of file")
   in
   let id1 = get_byte () in
   let id2 = get_byte () in
   let cm = get_byte () in
   let flags = get_byte () in
      if id1 <> 0x1F || id2 <> 0x8B then
	 raise (Error "bad magic number");
      if cm <> 8 then
	 raise (Error "unknown compression method");
      if flags land 0xE0 <> 0 then
	 raise (Error "bad flags, not a gzip file");
      for i = 1 to 6 do ignore (get_byte ()) done;
      if flags land 0x04 <> 0 then (
	 (* Skip extra data *)
	 let len1 = get_byte () in
	 let len2 = get_byte () in
	    for i = 1 to len1 + len2 lsl 8 do 
	       ignore (get_byte ())
	    done
      );
      if flags land 0x08 <> 0 then (
	 (* Skip original file name *)
	 while get_byte () <> 0 do () done
      );
      if flags land 0x10 <> 0 then (
	 (* Skip comment *)
	 while get_byte () <> 0 do () done
      );
      if flags land 0x02 <> 0 then (
	 (* Skip header CRC *)
	 ignore (get_byte ());
	 ignore (get_byte ())
      );
      let buf_out = String.create chunk in
      let rec aux_inf pos_in crc =
	 let stream_end, used_in, used_out =
            zlib_inflate strm Z_SYNC_FLUSH
               buf_in pos_in (len - pos_in) buf_out 0 chunk in
	 let newcrc = Mlzlib.crc32 crc buf_out used_out in
            f buf_out used_out;
	    i := !i + used_in;
	    if stream_end then (
	       i := !i - 1;
	       let b1 = Int32.of_int (get_byte ()) in
	       let b2 = Int32.of_int (get_byte ()) in
	       let b3 = Int32.of_int (get_byte ()) in
	       let b4 = Int32.of_int (get_byte ()) in
	       let crc = 
		  Int32.logor b1 
		     (Int32.logor (Int32.shift_left b2 8)
			 (Int32.logor (Int32.shift_left b3 16)
			     (Int32.shift_left b4 24))) in
		  Printf.printf "CRC %lu %lu\n" crc newcrc;
		  f "" 0
	    )
	    else if used_in = len - pos_in then
	       raise (Error "truncated file")
            else 
	       aux_inf (pos_in + used_in) newcrc
      in
	 incr i;
	 aux_inf !i Int32.zero

let _ =
   let fname = "test.xml" in
   let f_in = open_in_bin fname in
   let buf = Buffer.create chunk in
   let s = String.create chunk in
   let rec aux_read () =
      let size = input f_in s 0 chunk in
	 if size = 0 then
	    close_in f_in
	 else (
	    Buffer.add_string buf (String.sub s 0 size);
	    aux_read ()
	 )
   in
      aux_read ();
      let f buf len = Printf.printf "%s" (String.sub buf 0 len) in
	 gunzip_string (Buffer.contents buf) f
