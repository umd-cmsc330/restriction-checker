open Synopsis
open Synopsis__Utils
open ListLabels

module A = Alcotest

(* verifies some basic functionality of synopsis *)

(* comparator for alcotest *)
let syn = 
  (* for all intents and purposes, order doesn't matter, rather,
    just that the lists contain the same elements *)
  let eq (s: synopsis) (s': synopsis) =
    let cmp = Stdlib.compare in
    sort ~cmp s.modules = sort ~cmp s'.modules && (* we don't care about module ordering *)
    fold_left2 
    ~f:(fun a (b, c) (b', c') -> 
          sort ~cmp b = sort ~cmp b' &&           (* honestly i have no clue how it would order this *)
          sort ~cmp c = sort ~cmp c' &&           (* we don't care about function call ordering *)
          a)
    ~init:true
    s.definitions s'.definitions

  in A.testable pp_synopsis eq

let assert_syn expected strings =
  (* map strings of expressions into synopses
    and compare them to what's expected *)
  map ~f:read_string strings 
  |> iter2 ~f:(A.(syn |> check) "") expected

(* this is not too descriptive, just says 
  true when expects false for now *)
let assert_fails check expected strings = 
  map ~f:read_string strings
  |> map ~f:(fun syn -> try check [syn]; false with _ -> true)
  |> A.(bool |> list |> check) "" expected

let test_basic _ = 
  let test_strings = [
                      "let x = 3";
                      "let f x = x + 1";
                      "let (a::b) = [1;2]";
                      "let a,b = 1,2";
                      "let f (a,b) = a + b"
                     ] in
  (* each expression can have multiple parts, so has to be list of lists *)
  (* bindings have to be list of strings, can bind multiple identifiers at once *)
  let (expected: synopsis list) = 
    [
      {
        modules = [];           (* utilizes no modules *)
        definitions = [
          [(false, ["x"])], []; (* binds x as non-rec value, no function calls *)
        ]
      };
      {
        modules = [];
        definitions = [
          [(false, ["f"])], ["+"];
        ]
      };
      {
        modules = [];
        definitions = [
          [(false, ["a"; "b"])], [];
        ]
      };
      {
        modules = [];
        definitions = [
          [(false, ["a"; "b"])], [];
        ]
      };
      {
        modules = [];
        definitions = [
          [(false, ["f"])], ["+"];
        ]
      }
    ]
    (* [([(false, ["x"])],[])];          
    [([(false, ["f"])],["+"])];       (* binds f as non-rec function, calls + *)
    [([(false, ["b"; "a"])],[])];     (* binds a, b in non-rec constructor, no function calls *)
    [([(false, ["b"; "a"])],[])];     (* same as above *)
    [([(false, ["f"])],["+"])];       binds f as non-rec function, calls + *)
  in assert_syn expected test_strings 

let test_nested _ =
  let test_strings = [
    "let rec fold f a xs = match xs with
    | [] -> a
    | x :: xt -> fold f (f a x) xt
    let is_there lst x = 
      fold (fun acc n -> if n = x then true else acc) false lst";
    "let every_xth x lst = 
      let helper (ind, xth) n = 
        match ind mod x with
        0 -> (1, xth @ [n])
        | _ -> (ind + 1, xth)
      in let _, res = fold helper (1, []) lst in res";
    "let rec jumping_tuples_helper zipped left right ind =
      match zipped with
      [] -> left @ right
      | (a, _, _, d) :: rest ->
          let next = ind + 1 in
          if ind mod 2 = 1 then jumping_tuples_helper rest (left @ [a]) (right @ [d]) next
          else jumping_tuples_helper rest (left @ [d]) (right @ [a]) next

    let jumping_tuples lst1 lst2 = jumping_tuples_helper (zip lst1 lst2) [] [] 0"
  ] in
  let (expected: synopsis list) = [
    {
      modules = [];
      definitions = [
        [(true, ["fold"])], ["fold"; "f"];
        [(false, ["is_there"])], ["fold"; "="];
      ]
    };
    {
      modules = [];
      definitions = [
        [(false, ["every_xth"]); (false, ["helper"]); (false, ["res"])], ["mod"; "@"; "+"; "fold"]
      ]
    };
    {
      modules = [];
      definitions = [
        [(true, ["jumping_tuples_helper"]); (false, ["next"])], ["@"; "+"; "mod"; "="; "jumping_tuples_helper"];
        [(false, ["jumping_tuples"])], ["jumping_tuples_helper"; "zip"]
      ]
    };
  ] in 
  assert_syn expected test_strings

let test_ref _ =
  let test_strings = [
    "let x = 3";
    "let x = 1 in let y = ref x in !x";
    "let every_xth x lst = 
      let helper (ind, xth) n = 
        match ind mod x with
        0 -> (1, xth @ [n])
        | _ -> (ind + 1, xth)
      in let _, res = fold helper (1, []) lst in res";
    "let every_xth x lst = 
      let helper (ind, xth) n = 
        let g = ref n in
        match ind mod x with
        0 -> (1, xth @ [!n])
        | _ -> (ind + 1, xth)
      in let _, res = fold helper (1, []) lst in res"
  ] in
  let expected = [
    false;
    true;
    false;
    true;
  ]        
  in assert_fails (ref_check) expected test_strings

(* i mean, this kind of demonstrates there's a lot to be desired rn, but ultimately,
  the things that it's not catching aren't in the scope of the course *)
let test_modules _ = 
  let allowed = [
    "List";
  ] in
  let test_strings = [
    "open Hashtbl
      let deconstruct_binding_list rf vb_lst = 
        let deconstruct_binding {Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} =
          let sub_bindings, calls = get_bindings_calls expr in
          (* this callsite needs to make patterns unique (?) *)
          ((rf = Asttypes.Recursive, get_names_from_pattern bindee)::sub_bindings, calls)
        in 
        ListLabels.map ~f:deconstruct_binding vb_lst
        |> ListLabels.split
        |> fun (a, b) -> (ListLabels.concat a, ListLabels.concat b)";
  ] in
  let expected = [
    true;
  ] in
  assert_fails (module_check ~allowed) expected test_strings

let () =
  A.(run
  ~argv: [|
    "";
    "--show-errors";
    "--verbose";
  |]
  "Tests"
  [
    "Function", [
      test_case "simple bindings"           `Quick test_basic;
      test_case "nested/multiple bindings"  `Quick test_nested;
    ];
    "Checks", [
      test_case "ref check"                 `Quick test_ref;
      test_case "module check"              `Quick test_modules;
    ];
  ])