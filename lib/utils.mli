type _identifier = string
type _call = string

type _def_type =
  Recursive | Non_recursive
(** wrapper for boolean flag that represents if a definition is recursive *)

type _binding = _def_type * _identifier list
(** defines a binding: is it recursive? what identifiers does it bind? *)

type _definition = _binding list * _call list
(** defines a definition: what does it bind? what functions are called in its body? *)

(** a type that decribes certain aspects of a program's parsetree. *)
type _synopsis = {
    modules: _identifier list ;     (** a list of identifiers of modules opened/used *)
    definitions: _definition list ; (** a list of top-level [_definitions] *)
   }

val insert: 'a list -> 'a list -> 'a list
val get_desc: item:Parsetree.structure_item -> Parsetree.structure_item_desc
val get_exp_desc: expression:Parsetree.expression -> Parsetree.expression_desc
val get_pattern_desc: pattern:Parsetree.pattern -> Parsetree.pattern_desc
val get_names_from_lident: Longident.t -> _identifier list
val get_module_ident: Parsetree.open_declaration -> _identifier list

val get_names_from_pattern: Parsetree.pattern -> _identifier list
(** parses a Parsetree.pattern, gathering all identifiers being bound
    @param p pattern that is binding identifiers
    @return list of identifiers bound by pattern
  *)

val get_bindings_calls: Parsetree.expression -> _definition
(** parses a Parsetree.expression into a [_definition], containing
    the bindings and function applications that occur
    @param exp expression being parsed
    @return [_definition] representing bindings and function calls
            happening inside the expression 
  *)

val deconstruct_binding_list: Asttypes.rec_flag -> Parsetree.value_binding list -> _definition
(** turns a Parsetree.value_binding list into a [_definition] for the items bound
    by the expression being parsed 
    @param rf whether or not the binding is recursive
    @param vb_lst the list of value bindings to convert
    @return [_definition] representing the value bindings
  *)

val get_synopsis: Parsetree.structure_item -> acc:_synopsis -> _synopsis
(** generates updates a synopsis with information via the structure item argument
    @param the item to deconstruct into a synopsis
    @param acc the [_synopsis] to accumulate information into
    @return new updated synopsis record
  *)