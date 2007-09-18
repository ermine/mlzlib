/*
 * (c) 2006 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <zlib.h>

#define Z_Val(v) ((z_stream *) (v))

static value *mlzlib_error_exn = NULL;

static int z_retcode_table[] = {
  Z_OK,
  Z_STREAM_END,
  Z_NEED_DICT,
  Z_ERRNO,
  Z_STREAM_ERROR,
  Z_DATA_ERROR,
  Z_MEM_ERROR,
  Z_BUF_ERROR,
  Z_VERSION_ERROR
};

value z_retcode_of_code (int retcode) {
  value err;

  int errconstr = cst_to_constr(retcode, 
				z_retcode_table, 
				sizeof(z_retcode_table)/sizeof(int),
				Z_DATA_ERROR);
  if (errconstr == Val_int(-1)) {
    err = alloc_small(1, 0);
    Field(err, 0) = Val_int(retcode);
  } else {
    err = errconstr;
  }
  return err;
}

static void mlzlib_error(char *fn, int ret, char *msg) {
  value res;
  value s1 = Val_unit, s2 = Val_unit, s3 = Val_unit;

  if (msg == NULL)
    msg = "";

  Begin_roots3(s1, s2, s3);
    if (mlzlib_error_exn == NULL) {
      mlzlib_error_exn = caml_named_value("Mlzlib.Error");
      if (mlzlib_error_exn == NULL)
	invalid_argument("Exception Mlzlib.Error not initialized");
    }
    s1 = z_retcode_of_code(ret);
    s2 = copy_string(fn);
    s3 = copy_string(msg);
    res = alloc_small(4, 0);
    Field(res, 0) = *mlzlib_error_exn;
    Field(res, 1) = s1;
    Field(res, 2) = s2;
    Field(res, 3) = s3;
  End_roots();
  mlraise(res);
}

value z_stream_new(void) {
  value res = alloc((sizeof(z_stream) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);

  Z_Val(res)->zalloc = Z_NULL;
  Z_Val(res)->zfree = Z_NULL;
  Z_Val(res)->opaque = Z_NULL;
  Z_Val(res)->next_in = Z_NULL;
  Z_Val(res)->next_out = Z_NULL;
  Z_Val(res)->avail_in = 0;

  return res;
}

static int level_table[] = {
  Z_NO_COMPRESSION,
  Z_BEST_SPEED,
  Z_BEST_COMPRESSION,
  Z_DEFAULT_COMPRESSION
};

CAMLprim value mlzlib_deflateInit(value vlevel) {
  value vz = z_stream_new();
  int ret = deflateInit(Z_Val(vz), level_table[Int_val(vlevel)]);
  if (ret != Z_OK)
    mlzlib_error("deflateInit", ret, Z_Val(vz)->msg);
  return vz;
}

CAMLprim value mlzlib_inflateInit(value unit) {
  value vz = z_stream_new();
  int ret = inflateInit(Z_Val(vz));
  if (ret != Z_OK)
    mlzlib_error("inflateInit", ret, Z_Val(vz)->msg);
  return vz;
}

CAMLprim value mlzlib_deflateEnd(value vz) {
  int ret = deflateEnd(Z_Val(vz));
  if (ret != Z_OK)
    mlzlib_error("deflateEnd", ret, Z_Val(vz)->msg);
  return Val_unit;
}

CAMLprim value mlzlib_inflateEnd(value vz) {
  int ret = inflateEnd(Z_Val(vz));
  if (ret != Z_OK)
    mlzlib_error("inflateEnd", ret, Z_Val(vz)->msg);
  return Val_unit;
}

static int flush_table[] = {
  Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH, Z_BLOCK
};

CAMLprim value mlzlib_inflate_nc(value vz, value vflush, 
				 value vsrc_buf, value vsrc_pos, value vsrc_len,
				 value vdst_buf, value vdst_pos, value vdst_len)
{
  z_stream *z = Z_Val(vz);
  int used_in;
  int used_out;
  int ret;
  value res;

  z->next_in = &Byte_u(vsrc_buf, Long_val(vsrc_pos));
  z->avail_in = Long_val(vsrc_len);
  z->next_out = &Byte_u(vdst_buf, Long_val(vdst_pos));
  z->avail_out = Long_val(vdst_len);

  ret = inflate(z, flush_table[Int_val(vflush)]);

  if (ret < 0 || ret == Z_NEED_DICT)
    mlzlib_error("inflate", ret, z->msg);

  used_in = Long_val(vsrc_len) - z->avail_in;
  used_out = Long_val(vdst_len) - z->avail_out;

  z->next_in = Z_NULL;
  z->next_out = Z_NULL;

  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(ret == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

CAMLprim value mlzlib_inflate_bc(value *arg, int nargs) {
  return mlzlib_inflate_nc(arg[0], arg[1], arg[2], arg[3],
			   arg[4], arg[5], arg[6], arg[7]);
}

CAMLprim value mlzlib_deflate_nc(value vz, value vflush,
				 value vsrc_buf, value vsrc_pos, value vsrc_len,
				 value vdst_buf, value vdst_pos, value vdst_len)
{
  z_stream *z = Z_Val(vz);
  int used_in;
  int used_out;
  int ret;
  value res;

  z->next_in = &Byte_u(vsrc_buf, Long_val(vsrc_pos));
  z->avail_in = Long_val(vsrc_len);
  z->next_out = &Byte_u(vdst_buf, Long_val(vdst_pos));
  z->avail_out = Long_val(vdst_len);

  ret = deflate(z, flush_table[Int_val(vflush)]);

  if (ret < 0)
    mlzlib_error("deflate", ret, z->msg);

  used_in = Long_val(vsrc_len) - z->avail_in;
  used_out = Long_val(vdst_len) - z->avail_out;

  z->next_in = Z_NULL;
  z->next_out = Z_NULL;

  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(ret == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

CAMLprim value mlzlib_deflate_bc(value *arg, int nargs) {
  return mlzlib_deflate_nc(arg[0], arg[1], arg[2], arg[3],
			   arg[4], arg[5], arg[6], arg[7]);
}

CAMLprim value mlzlib_zlibVersion(value unit) {
  value ret;
  const char *v = zlibVersion();

  if(v == NULL)
    failwith("zlibVersion returns null pointer");

  ret = caml_copy_string(v);
  return ret;
}

/*
TODO
const char * zError (int err);
int inflateSyncPoint (z_streamp z);
const uLongf * get_crc_table (void);
*/
