(** utilities for synopsis *)

module StringSet = Set.Make(String)

type _identifier = string
type _call = string
type _binding = bool * _identifier list
type _definition = _binding list * _call list
(* i elected for a record because the field names are readable *)
(* yeah, i think the next big refactor will be making the functions
   take a synopsis and update it, kind of like nfas from p3 *)
type _synopsis = {
    modules: _identifier list ;
    (* maybe add a list of imperative constructs used *)
    definitions: _definition list ;
   }

(* set union of multiple lists of strings *)
(* lowkey, just like combine lists and then uniq sort normally maybe *)
let union lsts = 
  let open StringSet in
  List.concat lsts |> of_list |> to_list

let get_exp_desc ({pexp_desc=d; _}:Parsetree.expression) = d

let rec get_names_from_lident (id:Longident.t) = 
  match id with
  |Lident(x) -> [x]
  |Ldot(_, _) -> [String.concat "." (Longident.flatten id)]
  |Lapply(a,b) -> (get_names_from_lident a) @ (get_names_from_lident b)

(* use for ppat open too *)
let parse_loc ({Asttypes.txt=id; _}:Longident.t Asttypes.loc) = get_names_from_lident id

(* gather modules used from function calls of the syntax: (M.)*f *)
let modules_from_calls calls = 
  let open List in
  (* gets all but last id (the function's id) in something like (module name.)*function *)
  let parse_dot names = 
    match rev names with
    |[_] -> []
    |_::t -> [String.concat "." (rev t)]
    |_ -> []
  in map (String.split_on_char '.') calls |> concat_map parse_dot

(* can combine a lot of these patterns into or-patterns *)
let rec get_names_from_pattern ({ppat_desc=desc; _}:Parsetree.pattern) = 
  match desc with
  |Ppat_var({Asttypes.txt=x;_}) -> [x]
  |Ppat_alias(pat,{Asttypes.txt=x;_}) -> union [get_names_from_pattern pat; [x]]
  |Ppat_tuple(p_lst) -> List.fold_left (fun a x -> union [a; get_names_from_pattern x]) [] p_lst
  |Ppat_construct(_, o) -> 
    (match o with
    |None -> []
    |Some(_ ,p) -> get_names_from_pattern p)
  |Ppat_variant(_,Some(x)) -> get_names_from_pattern x
  |Ppat_record(lst,_) -> 
    List.fold_left (fun a ({Asttypes.txt=x;_},p) -> 
      union [a; get_names_from_lident x; get_names_from_pattern p]) [] lst
  |Ppat_array(lst) -> List.fold_left (fun a x -> union [a; get_names_from_pattern x]) [] lst
  |Ppat_or(a,b) -> union [get_names_from_pattern a; get_names_from_pattern b]
  |Ppat_constraint(p, _) | Ppat_exception(p) -> get_names_from_pattern p
  |Ppat_any | Ppat_constant(_) | Ppat_interval(_) | Ppat_variant(_, None) -> []
  |Ppat_open(_, p) -> get_names_from_pattern p
  |_ -> raise (Failure "using something like ppat_type, lazy, unpack or extension")

(* are bindings guaranteed to be unique? if no, which ones do we care about? the most recent? *)
let rec get_bindings_calls ({pexp_desc=desc; _}:Parsetree.expression) =
  let parse_cases cs = 
    List.fold_left (fun (a, b) {Parsetree.pc_rhs=x; _} ->
                      let bindings, calls = get_bindings_calls x in
                      (a @ bindings, union [b; calls])) ([], []) cs
  in match desc with
  |Pexp_ident({Asttypes.txt=i; _}) -> 
    (match i with 
     | Longident.Ldot(_, _) -> ([], get_names_from_lident i) (* something like just Funs.id *)
     | _ -> ([], []))   (* we don't care about other random identifiers *)
  |Pexp_let(rf,vb_lst,e) -> 
    let bindings, calls = deconstruct_binding_list rf vb_lst in
    let bindings', calls' = get_bindings_calls e in
    (bindings @ bindings', union [calls; calls'])
  |Pexp_function(_, _, Pfunction_cases(case_lst,_,_)) -> parse_cases case_lst
  |Pexp_function(_, _, Pfunction_body(e)) -> get_bindings_calls e
  |Pexp_apply(e,lst) -> 
    (* gets the bindings and calls from the function applied *)
    let bindings, calls = 
      (match get_exp_desc e with
       |Pexp_ident({Asttypes.txt=i; _}) -> ([], get_names_from_lident i)
       |_ -> get_bindings_calls e)
    (* get the bindings and calls from the arguments *)
    in List.fold_left (fun (a,b) (_,x) -> 
        let (bindings', calls') = get_bindings_calls x in 
        (a @ bindings', union [b; calls'])) (bindings, calls) lst
  |Pexp_match(e, cs) -> 
    let bindings, calls = get_bindings_calls e in
    let bindings', calls' = parse_cases cs in
    (bindings @ bindings', union [calls; calls'])
  |Pexp_tuple(es) ->
    List.fold_left (fun (a, b) e -> let bindings, calls = get_bindings_calls e in
                                    (a @ bindings, union [b; calls])) ([], []) es
  |Pexp_record(cs, _) ->
    List.fold_left (fun (a, b) (_, ex) -> let bindings, calls = get_bindings_calls ex in
                                          (a @ bindings, union [b; calls])) ([], []) cs
  |Pexp_setfield(_, _, _) -> raise (Failure "Illegal use of mutable field")
  |Pexp_array(_) -> raise (Failure "Illegal use of array construct")
  |Pexp_ifthenelse(guard, t_branch, e_opt) ->
    let bindings, calls = get_bindings_calls guard in
    let bindings', calls' = get_bindings_calls t_branch in
    let bindings'', calls'' = 
      (match e_opt with
       |None -> ([], [])
       |Some(e_branch) -> get_bindings_calls e_branch)
    in (bindings @ bindings' @ bindings'', union [calls; calls'; calls''])
  |Pexp_sequence(e1, e2) -> let bindings, calls = get_bindings_calls e1 in
                            let bindings', calls' = get_bindings_calls e2 in
                            (bindings @ bindings', union [calls; calls'])
  |Pexp_while(_, _) -> raise (Failure "Illegal use of while loop construct")
  |Pexp_for(_, _, _, _, _) -> raise (Failure "Illegal use of for loop construct")
  (* actually something we can do here is add a dummy call (i.e Module_name.dummy)
     that works because we will get modules from calls and then we can just
     filter the dummy function calls out later in get_synopsis or something*)
  |Pexp_open(_, _) -> ([], [])
  |_ -> ([],[])

and deconstruct_binding_list rf vb_lst = 
  let deconstruct_binding ~acc:(a, b) ~binding:{Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} =
    let binding = (rf = Asttypes.Recursive, get_names_from_pattern bindee) in
    let sub_bindings, calls = get_bindings_calls expr in
    (binding::(sub_bindings @ a), union [b; calls])
  in List.fold_left (fun a n -> deconstruct_binding ~acc:a ~binding:n) ([], []) vb_lst

let rec get_synopsis {modules=m; definitions=d} ({pstr_desc=desc; _}:Parsetree.structure_item) = 
  (* for module bindings *)
  let destruct_pmb acc ({pmb_name={Asttypes.txt=opt; _}; pmb_expr=e; _}:Parsetree.module_binding) =
    match opt with
    |None -> from_mod_expr acc e   (* this is probably like the wildcard or something *)
    |Some(name) -> from_mod_expr {modules=union [[name]; acc.modules]; definitions=acc.definitions} e
  in
  let default = {modules = m; definitions = d} in
  match desc with
  |Pstr_open ({popen_expr=e; _}) -> from_mod_expr default e
  (* should deconstruct the "(module.)*function" function calls *)
  |Pstr_eval(e, _) -> 
    let (bindings', calls') = get_bindings_calls e in
    {modules = union [m; modules_from_calls calls']; definitions = d @ [(bindings', calls')]}
  |Pstr_value(rf,vb_lst) ->
    let (bindings', calls') = deconstruct_binding_list rf vb_lst in
    {modules = union [m; modules_from_calls calls']; definitions = d @ [(bindings', calls')]}
  |Pstr_module(pmb) -> destruct_pmb default pmb
  |Pstr_recmodule(lst) -> List.fold_left destruct_pmb default lst
  |_ -> default
  
and from_mod_expr {modules=m; definitions=d} e =
  match e.pmod_desc with
  |Pmod_ident(x) -> {modules=union [m; parse_loc x]; definitions=d}
  |Pmod_structure(s) -> 
    (* goes through structure recursively building new modules and definitions *)
    List.fold_left get_synopsis {modules=m; definitions=d} s
  |Pmod_functor(_, _) | Pmod_apply(_, _)
  |Pmod_apply_unit(_) | Pmod_constraint(_, _) 
  |Pmod_unpack(_) | Pmod_extension(_) -> raise (Failure "using weird module syntax")
