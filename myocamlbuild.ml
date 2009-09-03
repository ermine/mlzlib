open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      make_binding ~lib:"-lz" "mlzlib";

      install_lib "mlzlib" ["libmlzlib.a"; "dllmlzlib.so"]
  | _ ->
      ()
end
