(** utilities for synopsis
    @author Nathan Ho ({:{https://github.com/ptrichr} ptrichr})
  *)

type identifier = string

type call = string

(** defines a binding: is it recursive? what identifiers does it bind? *)
type binding = bool * identifier list

(** defines a definition: what does it bind? what functions are called in its body? *)
type definition = binding list * call list

(** decribes certain aspects of a program's parsetree, 
    namely the modules opened, values bound, and functions 
    called in each binding. *)
type synopsis = {
  modules: identifier list ;
  definitions: definition list ;
  } [@@deriving show { with_path = false }]


(** generates updates a synopsis with information via the structure item argument
    @param acc the [synopsis] to accumulate information into
    @param item the item to deconstruct into a synopsis
    @return new updated synopsis record
  *)
val get_synopsis: synopsis -> Parsetree.structure_item -> synopsis

