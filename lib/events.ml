include Glfw.Events

module Input = struct
  open Core.Syntax.Result

  let is_key_pressed key glfw =
    let+ action = (Glfw.get_key glfw key) in
    action = Actions.PRESS
end
