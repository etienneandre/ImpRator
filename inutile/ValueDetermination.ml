(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from Imperator to OCaml
 *
 * This file prints an AbstractImperatorFile structure
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       16/03/2009
 * Last modified: 20/03/2009
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open Matrix

(****************************************************************)
(** Value determination *)
(****************************************************************)

(* Get the value (expected cost) of every state *)
let value_determination policy tm costs =
	(* We here solve the equation u = costs + tm * u, i.e., u = (I - tm)-1 * costs *)
	
	print_debug_message mode_STANDARD mode_HIGH_DEBUG "tm := " ;
	print_debug_message mode_STANDARD mode_HIGH_DEBUG (string_of_matrix tm) ;
	
	let iN = i_n (Array.length tm) in
	
	print_debug_message mode_STANDARD mode_HIGH_DEBUG "iN := " ;
	print_debug_message mode_STANDARD mode_HIGH_DEBUG (string_of_matrix iN) ;
	
	let iN_minus_tm = add iN (opposite tm) in
	
	print_debug_message mode_STANDARD mode_HIGH_DEBUG "iN - tm := " ;
	print_debug_message mode_STANDARD mode_HIGH_DEBUG (string_of_matrix iN_minus_tm) ;

	let invert = invert iN_minus_tm in
	mult_matrix_vector invert costs
	
