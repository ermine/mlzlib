open Mlzlib
open Printf

let chunk = 16384

let rec writeb fd buf pos len =
   let size = Unix.write fd buf pos len in
      if size = len then
	 ()
      else
	 writeb fd buf (pos+size) (len-size)

let def source dest level =
   let strm = zlib_deflateInit level in

   let buf_in = String.create chunk in
   let buf_out = String.create chunk in

   let rec aux_read () =
      let size = Unix.read source buf_in 0 chunk in
      let flush = if size = 0 then Z_FINISH else Z_NO_FLUSH in
      let rec aux_def pos_in =
	 let stream_end, used_in, used_out = 
	    zlib_deflate strm flush
	       buf_in pos_in (size-pos_in) buf_out 0 chunk in
	    writeb dest buf_out 0 used_out;
	    eprintf "DEF: used_in %d, used_out = %d, stream_end %s\n" 
	       used_in used_out (if stream_end = true then "true" else "false");
	    if used_in = size - pos_in then
	       ()
	    else
	       aux_def (pos_in + used_in)
      in
	 aux_def 0;
	 if flush = Z_FINISH then 
	    () 
	 else 
	    aux_read ()
   in
      try
	 aux_read ();
	 zlib_deflateEnd strm
      with exn ->
	 zlib_deflateEnd strm

let inf source dest =
   let strm = zlib_inflateInit () in

   let buf_in = String.create chunk in
   let buf_out = String.create chunk in

   let rec aux_read () =
      let size = Unix.read source buf_in 0 chunk in
      let rec aux_inf pos_in =
	 let stream_end, used_in, used_out =
	    zlib_inflate strm Z_NO_FLUSH 
	       buf_in pos_in (size - pos_in) buf_out 0 chunk in
	    writeb dest buf_out 0 used_out;
	    eprintf "INF: used_in %d, used_out = %d, stream_end %s\n" 
	       used_in used_out (if stream_end = true then "true" else "false");
	    if used_in = size - pos_in then
	       stream_end
	    else 
	       aux_inf (pos_in+used_in)
      in
	 if aux_inf 0 then
	    ()
	 else
	    aux_read ()
   in
      try
	  aux_read ();
	 zlib_inflateEnd strm
       with _ ->
	  zlib_inflateEnd strm

let main () =
   if Array.length Sys.argv = 1 then
      def Unix.stdin Unix.stdout Z_DEFAULT_COMPRESSION
   else if Array.length Sys.argv = 2 && Sys.argv.(1) = "-d" then
      inf Unix.stdin Unix.stdout
   else
      eprintf "zpipe usage: zpipe [-d] < source > dest\n"

let _ =
   main ()
