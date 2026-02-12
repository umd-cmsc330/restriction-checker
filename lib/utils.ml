(** utilities for synopsis *)

module StringSet = Set.Make(String)

type _identifier = string
type _call = string
type _binding = bool * _identifier list
type _definition = _binding list * _call list
type _synopsis = {
    modules: _identifier list ;
    (* maybe add a list of imperative constructs used *)
    definitions: _definition list ;
   }

(* set union of multiple lists of strings *)
let union lsts = 
  let open StringSet in
  List.concat lsts |> of_list |> to_list

(* make a list unique by converting it to a set and back *)
let uniq lst = 
  let open StringSet in
  of_list lst |> to_list

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

(* these aren't guaranteed to be unique anymore. callee must make unique *)
let rec get_names_from_pattern ({ppat_desc=desc; _}:Parsetree.pattern) = 
  match desc with
  |Ppat_var({Asttypes.txt=x;_}) -> [x]
  |Ppat_alias(pat,{Asttypes.txt=x;_}) -> x::(get_names_from_pattern pat)
  |Ppat_record(lst,_) -> 
    ListLabels.concat_map ~f:(fun ({Asttypes.txt=x;_},p) -> get_names_from_lident x @ get_names_from_pattern p) lst
  |Ppat_tuple(p_lst) | Ppat_array(p_lst) -> ListLabels.concat_map ~f:get_names_from_pattern p_lst
  |Ppat_or(a,b) -> get_names_from_pattern a @ get_names_from_pattern b
  |Ppat_variant(_, Some(p)) | Ppat_constraint(p, _) | Ppat_exception(p) 
  |Ppat_open(_, p) | Ppat_construct(_, Some(_, p)) -> get_names_from_pattern p
  |Ppat_any | Ppat_constant(_) | Ppat_interval(_) 
  |Ppat_variant(_, None) | Ppat_construct(_, None) -> []
  |_ -> raise (Failure "using something like ppat_type, lazy, unpack or extension")

(* call list is not guarateed to be unique. callee must make unique *)
let rec get_bindings_calls ({pexp_desc=desc; _}:Parsetree.expression) =
  let from_lst lst =
    ListLabels.map ~f:get_bindings_calls lst
    |> ListLabels.split
    |> fun (a, b) -> (ListLabels.concat a, ListLabels.concat b)
  in
  let parse_cases cs = ListLabels.map ~f:(fun {Parsetree.pc_rhs=x; _} -> x) cs |> from_lst
  in match desc with
  |Pexp_ident({Asttypes.txt=i; _}) -> 
    (match i with 
     |Longident.Ldot(_, _) -> ([], get_names_from_lident i) (* something like just Funs.id *)
     |_ -> ([], []))   (* we don't care about other random identifiers *)
  |Pexp_let(rf,vb_lst,e) -> 
    let bindings, calls = deconstruct_binding_list rf vb_lst in
    let bindings', calls' = get_bindings_calls e in
    (bindings @ bindings', calls @ calls')
  |Pexp_function(_, _, Pfunction_cases(case_lst,_,_)) -> parse_cases case_lst
  |Pexp_function(_, _, Pfunction_body(e)) -> get_bindings_calls e
  |Pexp_apply(e,lst) -> 
    (* get the bindings and calls from the arguments *)
    let bindings, calls = ListLabels.map ~f:snd lst |> from_lst in
    (* get the bindings and calls from the function applied *)
    (match get_exp_desc e with
      |Pexp_ident({Asttypes.txt=i; _}) -> (bindings, get_names_from_lident i @ calls)  (* if id add to calls list *)
      |_ -> 
        let bindings', calls' = get_bindings_calls e in 
        (bindings @ bindings', calls @ calls'))
  |Pexp_match(e, cs) -> 
    let bindings, calls = get_bindings_calls e in
    let bindings', calls' = parse_cases cs in
    (bindings @ bindings', calls @ calls')
  |Pexp_tuple(es) -> from_lst es
  |Pexp_record(cs, _) -> ListLabels.map ~f:snd cs |> from_lst
  |Pexp_setfield(_, _, _) -> raise (Failure "Illegal use of mutable field")
  |Pexp_array(_) -> raise (Failure "Illegal use of array construct")
  |Pexp_ifthenelse(guard, t_branch, e_opt) ->
    let bindings, calls = get_bindings_calls guard in
    let bindings', calls' = get_bindings_calls t_branch in
    let bindings'', calls'' = 
      (match e_opt with
       |None -> ([], [])
       |Some(e_branch) -> get_bindings_calls e_branch)
    in (bindings @ bindings' @ bindings'', calls @ calls' @ calls'')
  |Pexp_sequence(e1, e2) -> let bindings, calls = get_bindings_calls e1 in
                            let bindings', calls' = get_bindings_calls e2 in
                            (bindings @ bindings', calls @ calls')
  |Pexp_while(_, _) -> raise (Failure "Illegal use of while loop construct")
  |Pexp_for(_, _, _, _, _) -> raise (Failure "Illegal use of for loop construct")
  (* actually something we can do here is add a dummy call (i.e Module_name.dummy)
     that works because we will get modules from calls and then we can just
     filter the dummy function calls out later in get_synopsis or something*)
  |Pexp_open(_, _) -> ([], [])
  |_ -> ([],[])

and deconstruct_binding_list rf vb_lst = 
  let deconstruct_binding {Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} =
    let sub_bindings, calls = get_bindings_calls expr in
    (* this callsite needs to make patterns unique (?) *)
    ((rf = Asttypes.Recursive, get_names_from_pattern bindee)::sub_bindings, calls)
  in 
  ListLabels.map ~f:deconstruct_binding vb_lst
  |> ListLabels.split
  |> fun (a, b) -> (ListLabels.concat a, ListLabels.concat b)

let rec get_synopsis {modules=m; definitions=d} ({pstr_desc=desc; _}:Parsetree.structure_item) = 
  (* for module bindings *)
  let destruct_pmb acc ({pmb_name={Asttypes.txt=opt; _}; pmb_expr=e; _}:Parsetree.module_binding) =
    match opt with
    |None -> from_mod_expr acc e   (* this is probably like the wildcard or something *)
    |Some(name) -> from_mod_expr {acc with modules=uniq (name::acc.modules)} e
  in
  let default = {modules = m; definitions = d} in
  match desc with
  |Pstr_open ({popen_expr=e; _}) -> from_mod_expr default e
  (* should deconstruct the "(module.)*function" function calls *)
  |Pstr_eval(e, _) -> 
    (* this callsite needs to make function calls unique *)
    let (bindings', calls') = get_bindings_calls e in
    {modules = union [m; modules_from_calls calls']; definitions = d @ [(bindings', uniq calls')]}
  |Pstr_value(rf,vb_lst) ->
    (* this callsite needs to make function calls unique *)
    let (bindings', calls') = deconstruct_binding_list rf vb_lst in
    {modules = union [m; modules_from_calls calls']; definitions = d @ [(bindings', uniq calls')]}
  |Pstr_module(pmb) -> destruct_pmb default pmb
  |Pstr_recmodule(lst) -> List.fold_left destruct_pmb default lst
  |_ -> default
  
and from_mod_expr synop e =
  match e.pmod_desc with
  |Pmod_ident(x) -> {synop with modules=union [synop.modules; parse_loc x]}
  |Pmod_structure(s) -> List.fold_left get_synopsis synop s
  |Pmod_functor(_, _) | Pmod_apply(_, _)
  |Pmod_apply_unit(_) | Pmod_constraint(_, _) 
  |Pmod_unpack(_) | Pmod_extension(_) -> raise (Failure "using weird module syntax")
