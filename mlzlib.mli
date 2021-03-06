(*
 * (c) 2006-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

val max_wbits: int

type z_retcode =
   | Z_OK
   | Z_STREAM_END
   | Z_NEED_DICT
   | Z_ERRNO
   | Z_STREAM_ERROR
   | Z_DATA_ERROR
   | Z_MEM_ERROR
   | Z_BUF_ERROR
   | Z_VERSION_ERROR

exception Error of z_retcode * string * string

external zlib_version: unit -> string
   = "mlzlib_zlibVersion"

type z_stream

type z_level =
   | Z_NO_COMPRESSION
   | Z_BEST_SPEED
   | Z_BEST_COMPRESSION
   | Z_DEFAULT_COMPRESSION

external zlib_deflateInit: z_level -> z_stream
   = "mlzlib_deflateInit"

external zlib_inflateInit: unit -> z_stream
   = "mlzlib_inflateInit"

external zlib_inflateInit2: int -> z_stream
   = "mlzlib_inflateInit2"

external zlib_deflateEnd: z_stream -> unit
   = "mlzlib_deflateEnd"

external zlib_inflateEnd: z_stream -> unit
   = "mlzlib_inflateEnd"

type z_flush =
   | Z_NO_FLUSH
   | Z_SYNC_FLUSH
   | Z_FULL_FLUSH
   | Z_FINISH
   | Z_BLOCK

external zlib_inflate: z_stream -> z_flush -> 
   string -> int -> int ->
   string -> int -> int ->
   bool * int * int
   = "mlzlib_inflate_bc" "mlzlib_inflate_nc"

external zlib_deflate: z_stream -> z_flush -> 
   string -> int -> int ->
   string -> int -> int ->
   bool * int * int
   = "mlzlib_deflate_bc" "mlzlib_deflate_nc"

external crc32: int32 -> string -> int -> int32
   = "mlzlib_crc32"

external uncompress : string -> int -> string -> int -> unit
   = "mlzlib_uncompress"
