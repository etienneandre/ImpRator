(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       16/03/2009
 * Last modified: 18/03/2009
 *
 ****************************************************************)

(****************************************************************)
(** Functions *)
(****************************************************************)
(* Get the value (expected cost) of every state *)
val value_determination : int array -> Matrix.matrix -> Matrix.vector -> Matrix.vector
