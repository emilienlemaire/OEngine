type lvl =
  | App
  | Error
  | Warning
  | Info
  | Debug
  | Trace
  [@@deriving show]

module Make (S : sig val fmt: Format.formatter val name : string end) = struct
  type level = lvl =
  | App
  | Error
  | Warning
  | Info
  | Debug
  | Trace
  [@@deriving show]

  let printf lvl (fmt: ('a, Format.formatter, unit, unit) format4) =
    Fmt.pf S.fmt "@[[%s] %a: " S.name pp_level lvl;
    (Fmt.kpf (fun fmt -> Fmt.pf fmt "@]") S.fmt fmt)
end

