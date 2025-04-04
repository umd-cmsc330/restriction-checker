(** top-level synopsis fuctions, used for enforcing restrictions *)

let read_string str = 
  let tree = Parse.implementation (Lexing.from_string str) in
  List.fold_left (Utils.get_synopsis) {modules = []; definitions = []} tree

let read_file src = 
  let inchan = open_in src in
  let parsetree = Parse.implementation (Lexing.from_channel inchan) in
  List.fold_left (Utils.get_synopsis) {modules = []; definitions = []} parsetree

let module_check (synops:Utils._synopsis list) ~allowed = 
  let open OUnit2 in
  let open List in
  iter (fun (s:Utils._synopsis) -> 
        (* break strings into modules and submodules *)
        map (String.split_on_char '.') s.modules 
        (* check if top module is allowed *)
        |> iter (fun deconstructed -> 
                  match deconstructed with
                  |[] -> ()
                  |prefix::_ -> assert_equal (mem prefix allowed) true ~msg:("disallowed module: " ^ prefix)))
  synops

let ref_check (synops:Utils._synopsis list) = 
  let open OUnit2 in
  let open List in
  iter (fun (s:Synopsis__Utils._synopsis) -> 
              (* check if any list of calls in a def has ref in it *)
              iter (fun d -> snd d |> mem "ref" |> assert_equal false) s.definitions)
  synops