open Core.Error

let init glfw =
  let res = Stubs.Glew.init () in
  if Stubs.Glew.init () <> 0 then
    match res with
    | x when x = Stubs.Glew.ok -> Result.ok glfw
    | x when x = Stubs.Glew.no_error -> Result.ok glfw
    | x when x = Stubs.Glew.error_no_gl_version ->
        Result.error (Glew No_gl_version)
    | x when x = Stubs.Glew.error_gl_version_10_only ->
        Result.error (Glew No_gl_version_10_only)
    | x when x = Stubs.Glew.error_glx_version_11_only ->
        Result.error (Glew No_glx_version_11_only)
    | x when x = Stubs.Glew.error_no_glx_display ->
        Result.error (Glew No_glx_display)
    | x -> Result.error (Glew (Unknown x))
  else
    Result.ok glfw
