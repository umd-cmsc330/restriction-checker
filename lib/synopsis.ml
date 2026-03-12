(** top-level synopsis fuctions, used for enforcing restrictions
    @author Nathan Ho ({:{https://github.com/ptrichr} ptrichr})
  *)

open ListLabels

module A = Alcotest

(* i feel like adding a function composition combinator would be nice here *)
(* reverse composition *)
let ( << ) f g x = g @@ f @@ x

(* empty synopsis *)
let (init: Utils.synopsis) = {modules=[]; definitions=[]}

(* gen synopsis from string *)
let read_string = 
  Lexing.from_string        (* lex string *)
  << Parse.implementation   (* generate AST *)
  << fold_left ~f:Utils.get_synopsis ~init

(* gen synopsis from file *)
let read_file =
  open_in
  << Lexing.from_channel
  << Parse.implementation
  << fold_left ~f:Utils.get_synopsis ~init

let module_check ~allowed (synops:Utils.synopsis list) = 
  let set = "+"::"-"::"*"::"/"::"~-"::"~+"::allowed in    (* floating point ops look like this *)
  let output = fold_left ~f:(fun a n -> a ^ " " ^ n) ~init:"illegal modules:" in
  let check (s:Utils.synopsis) =
     let illegal = 
      filter_map 
      ~f:(fun m -> match String.split_on_char '.' m with 
                   | prefix::_ when not (mem ~set prefix) -> Some(prefix)
                   | _ -> None) 
      s.modules
    in A.(string |> list |> check) (output illegal) [] illegal
  in iter ~f:check synops

(* they could just shadow ref, it's only an issue 
  if they use ref in conjunction with ! *)
let ref_check (synops:Utils.synopsis list) = 
  let check (s:Utils.synopsis) =
    concat_map ~f:snd s.definitions 
    |> (fun fs -> List.mem "ref" fs && List.mem "!" fs)
    |> A.(check bool) "" false
  in iter ~f:check synops