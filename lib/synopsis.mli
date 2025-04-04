val read_string: string -> Utils._synopsis
(** generate a synopsis from a string
    @param str ocaml expression represented as a string 
    @return synopsis of ocaml expression *)

val read_file: string -> Utils._synopsis
(** generate a synopsis from a file
    @param src filepath to read from
    @return synopsis of file read *)

val module_check: Utils._synopsis list -> allowed:string list -> unit
val ref_check: Utils._synopsis list -> unit