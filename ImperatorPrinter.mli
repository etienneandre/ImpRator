(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       16/03/2009
 * Last modified: 25/03/2009
 *
 ****************************************************************)

(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the instantiated program to a string*)
val string_of_instantiated_program : AbstractImperatorFile.abstract_program -> string

(* Convert the parametric program to a string *)
val string_of_parametric_program : AbstractImperatorFile.abstract_program -> string
