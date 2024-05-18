let c_header =
  {|
#include <GL/glew.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
|}

let main () =
  let ml_out = open_out "generated.ml" in
  let c_out = open_out "stubs.c" in
  let c_constants_out = open_out "constants_generator.c" in
  let c_fmt = Format.formatter_of_out_channel c_out in
  let ml_fmt = Format.formatter_of_out_channel ml_out in
  let c_constants_fmt = Format.formatter_of_out_channel c_constants_out in
  Format.fprintf c_constants_fmt "%s@\n" c_header;
  Cstubs.Types.write_c c_constants_fmt (module Bindings.Constants);
  Format.fprintf c_fmt "%s@\n" c_header;
  Cstubs.write_c c_fmt ~prefix:"stubs_" (module Bindings.Bindings);
  Cstubs.write_ml ml_fmt ~prefix:"stubs_" (module Bindings.Bindings);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  Format.pp_print_flush c_constants_fmt ();
  close_out ml_out;
  close_out c_out;
  close_out c_constants_out

let () = main ()
