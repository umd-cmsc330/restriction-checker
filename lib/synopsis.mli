(** top-level synopsis fuctions, used for enforcing restrictions
    @author Nathan Ho ({:{https://github.com/ptrichr} ptrichr})
  *)

(** generate a synopsis of the expressions contained in a string
    @param _ expression represented as a string 
    @return synopsis of ocaml expression 
  *)
val read_string: string -> Utils.synopsis

(** generate a synopsis of the top-level bindings in a file
    @param _ filepath to read from
    @return synopsis of file read
  *)
val read_file: string -> Utils.synopsis

(** determine if the modules that are used by this source code
    contains modules that are not permitted for use.
    @param allowed list of modules that are permitted for use
    @param synops list of synopsis to analyze
    @return fails if an illegal module usage is detected
  *)
val module_check: allowed:string list -> Utils.synopsis list -> unit

(** determine if there is any usage of refs in this source code
    @param synops list of synopsis to analyze
    @return fails if ref usage is detected
  *)
val ref_check: Utils.synopsis list -> unit