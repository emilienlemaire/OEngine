module Result = struct
  let ( let* ) = Result.bind
  let ( let+ ) res f = Result.map f res
  let ( let$ ) res f = Result.iter f res
  let ( >>= ) = Result.bind
  let ( *> ) res f = Result.map f res
  let ( <* ) = Result.map
end
