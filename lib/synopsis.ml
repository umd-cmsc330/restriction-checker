(** top-level synopsis fuctions, used for enforcing restrictions *)
open ListLabels

module A = Alcotest

(* i feel like adding a function composition combinator would be nice here *)
(* reverse composition *)
let (%%) f g x = g (f x)

(* empty synopsis *)
let (empty:Utils._synopsis) = {modules=[]; definitions=[]}

(* gen synopsis from string *)
let read_string = 
  Lexing.from_string        (* lex string *)
  %% Parse.implementation   (* generate AST *)
  %% fold_left ~f:Utils.get_synopsis ~init:empty

(* gen synopsis from file *)
let read_file =
  open_in
  %% Lexing.from_channel
  %% Parse.implementation
  %% fold_left ~f:Utils.get_synopsis ~init:empty

(* ok, but now this has brought back the float function quirk *)
(* perhaps at some point we can include the top-level binding in which it was found? *)
let module_check (synops:Utils._synopsis list) ~allowed = 
  let output = fold_left ~f:(fun a n -> a ^ " " ^ n) ~init:"illegal modules:" in
  let check (s:Utils._synopsis) =
     let illegal = 
      concat_map ~f:(String.split_on_char '.') s.modules
      |> filter ~f:(Fun.flip List.mem allowed %% not)
    in A.(string |> list |> check) (output illegal) [] illegal
  in iter ~f:check synops

let ref_check (synops:Utils._synopsis list) = 
  let check (s:Utils._synopsis) =
    concat_map ~f:snd s.definitions 
    |> List.mem "ref" 
    |> A.(check bool) "" false
  in iter ~f:check synops