(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       24/03/2009
 * Last modified: 27/03/2009
 *
 ****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)


(****************************************************************)
(** Types *)
(****************************************************************)

(* A constant is a rational *)
type constant = Num.num

(* A coeff is a rational *)
type coeff = Num.num

(* A right / left member of an inequality is an array variable_index -> coeff, and a constant *)
type member = coeff array * constant

(* Operators *)
type op =
	(* Lower *)
	| Op_l
	(* Lower or equal *)
	| Op_le
	(* Equal *)
	| Op_eq
	(* Greater or equal *)
	| Op_ge
	(* Greater *)
	| Op_g

(* Operators for inequality *)
type op_ineq =
	(* Equal *)
	| Opi_eq
	(* Greater or equal *)
	| Opi_ge
	(* Greater *)
	| Opi_g


(* An inequality is a member and an operator (member op 0) *)
type inequality = member * op_ineq

(* A constraint is a list of inequalities *)
type constr = inequality list


(****************************************************************)
(** Functions on members *)
(****************************************************************)
(* Return a fresh new member as the product of a member by a scalar *)
val mult_member : member -> Num.num -> member

(* Return a fresh new member as the sum of two members (on the same set of variables) *)
val add_members : member -> member -> member

(* Instantiate a member with an array variable_index -> value *)
val instantiate_member : member -> Num.num array -> Num.num


(****************************************************************)
(** Functions on inequalities *)
(****************************************************************)
(* Convert an expression member op member into member' op' 0 *)
val make_inequality : member -> op -> member -> inequality


(****************************************************************)
(** Functions on constraints *)
(****************************************************************)
(* Add a constraint in head of a list of inequalities, if not already subsumed *)
val add_inequality : inequality -> constr -> constr


(****************************************************************)
(** Conversion to string *)
(****************************************************************)
(* Convert a member into a string *)
val string_of_exact_member : string array -> member -> string

(* Convert an inequality into a string *)
val string_of_exact_inequality : string array -> inequality -> string

(* Convert a constraint into a string *)
val string_of_exact_constraint : string array -> constr -> string

(* Convert a constraint into a string with approximate value *)
val string_of_approximate_constraint : string array -> constr -> string
