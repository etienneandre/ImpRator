(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       04/03/2009
 * Last modified: 05/03/2009
 *
 ****************************************************************)



(****************************************************************)
(** Fraction *)
(****************************************************************)

type fraction = (int * int) (* Numerator / Denominator *)

(* The '0' fraction *)
val zero : fraction

(****************************************************************)
(** Functions on fractions *)
(****************************************************************)
(* Reducts a fraction so that it is irreductible *)
val reduct :
  fraction -> fraction

(* Add two fractions and return an irreductible fraction *)
val add :
  fraction -> fraction -> fraction

(* Check if a fraction is equal to 1 *)
val equal_to_1 :
  fraction -> bool

(* Convert a int to a fraction *)
val fraction_of_int :
  int -> fraction

(* Convert a float to a fraction *)
val fraction_of_float :
  float -> fraction

(* Convert a fraction to a string "num / den" *)
val string_of_fraction :
  fraction -> string

(*
	

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbstractImperatorFile.list_of_states
*)