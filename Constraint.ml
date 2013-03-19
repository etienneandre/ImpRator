(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       24/03/2009
 * Last modified: 22/04/2009
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open Num


(****************************************************************)
(** Modules *)
(****************************************************************)
exception InvalidInequality


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
(** Operation on members *)
(****************************************************************)
(* Return the member equal to -member *)
(*
let opposite (monomial_array, constant) =
	(* Minus the monomials *)
	let monomial_array_opposite = Array.map Num.minus_num monomial_array in
	(* Minus the constant *)
	let constant_opposite = Num.minus_num constant in
	(* Return the monomials and the constant *)
	monomial_array_opposite, constant_opposite
*)

(* Return a fresh new member as the product of a member by a scalar *)
let mult_member (monomial_array, constant) scalar =
	(* Multiply the monomials *)
	let monomial_array_mult = Array.map (fun coeff -> coeff */ scalar) monomial_array in
	(* Multiply the constant *)
	let constant_mult = constant */ scalar in
	(* Return the monomials and the constant *)
	monomial_array_mult, constant_mult


(* Return a fresh new member as the sum of two members (on the same set of variables) *)
let add_members (monomial_array1, constant1) (monomial_array2, constant2) =
	let n = Array.length monomial_array1 in
	(* Check the size *)
	if n <> Array.length monomial_array2
		then raise (InternalError "Two members must have the same size to be added");
	(* Compute the sum of monomials *)
	let monomial_array = Array.make n (Num.num_of_int 0) in
	for i = 0 to n-1 do
		monomial_array.(i) <- monomial_array1.(i) +/ monomial_array2.(i);
	done;
	(* Compute the sum of constants *)
	let constant = constant1 +/ constant2 in
	(* Return the monomials and the constant *)
	monomial_array, constant


(* Return a fresh new member as the substraction of two members (on the same set of variables) *)
let sub_members (monomial_array1, constant1) (monomial_array2, constant2) =
	let n = Array.length monomial_array1 in
	(* Check the size *)
	if n <> Array.length monomial_array2
		then raise (InternalError "Two members must have the same size to be sub");
	(* Compute the sum of monomials *)
	let monomial_array = Array.make n (Num.num_of_int 0) in
	for i = 0 to n-1 do
		monomial_array.(i) <- monomial_array1.(i) -/ monomial_array2.(i);
	done;
	(* Compute the substraction of constants *)
	let constant = constant1 -/ constant2 in
	(* Return the monomials and the constant *)
	monomial_array, constant


(* Instantiate a member with an array variable_index -> value *)
let instantiate_member (monomial_array, constant) value =
	(* Start with constant *)
	let sum = ref constant in
	(* Iterate on monomials *)
	Array.iteri (fun variable_index coeff ->
		sum := !sum +/ value.(variable_index) */ coeff;
	) monomial_array;
	(* Return sum *)
	!sum


(****************************************************************)
(** Functions on inequalities *)
(****************************************************************)
(* Compute the real comparison function from a op_ineq *)
let real_op = function
	(* Equal *)
	| Opi_eq -> (=/)
	(* Greater or equal *)
	| Opi_ge -> (>=/)
	(* Greater *)
	| Opi_g -> (>/)


(* Convert an expression member op member into member' op' 0 *)
let make_inequality left op right =
	match op with
	(* left < right <=> -left + right > 0 *)
	| Op_l -> sub_members right left, Opi_g
	(* left <= right <=> -left + right >= 0 *)
	| Op_le -> sub_members right left, Opi_ge
	(* left = right <=> -left + right = 0 *)
	| Op_eq -> sub_members right left, Opi_eq
	(* left >= right <=> left - right >= 0 *)
	| Op_ge -> sub_members left right, Opi_ge
	(* left > right <=> left - right > 0 *)
	| Op_g -> sub_members left right, Opi_g


(* Return true if an inequality is constant, and raise InvalidInequality if constant op 0 is invalid *)
let constant_inequality ((monomial_array, constant), op) =
	(* Check if the monomial_array is empty *)
	let all_zero = List.for_all (fun elem -> elem = (Num.num_of_int 0)) (Array.to_list monomial_array) in
	(* If not constant : false *)
	if not all_zero then false
	(* Check validity *)
	else if not ((real_op op) constant (Num.num_of_int 0)) then
		raise InvalidInequality
	(* If constant and valid : true *)
	else true


(*
(* Return true if an inequality is weaker than another one *)
let is_weaker ((monomial_array1, constant1), op1) ((monomial_array2, constant2), op2) =
	(* Compare op *)
	op1
*)


let is_equal ((monomial_array1, constant1), op1) ((monomial_array2, constant2), op2) =
	(* Compare op *)
	op1 = op2 &
	(* Compare constants *)
	constant1 =/ constant2 &
	(* Compare arrays *)
	let equal = ref true in
	let i = ref 0 in
	while !i < (Array.length monomial_array1) do
		(* If different : stop *)
		if monomial_array1.(!i) <>/ monomial_array2.(!i) then(
			equal := false;
			i := Array.length monomial_array1;
		);
		i := !i + 1;
	done;
	!equal



(****************************************************************)
(** Functions on constraints *)
(****************************************************************)
(* Add a constraint in head of a list of inequalities, if not already subsumed *)
let add_inequality inequality constr =
	(* If constant (may raise InvalidInequality), do not add *)
	if constant_inequality inequality then constr else
	(* Name the inequality *)
(*	let ((monomial_array, constant), op) = inequality in*)
	(* If the inequality is equal to another one, do not add *)
	if List.exists (fun some_ineq -> is_equal some_ineq inequality) constr then constr else
	(* Else add in head *)
	inequality :: constr
	(* TO DO potentiellement faisable : verifier si une contrainte = une autre a un coeff pres 
	verifier si une autre est plus faible ou plus forte *)



(****************************************************************)
(** Conversion to string *)
(****************************************************************)
(* Type of constraint printing *)
type print_type =
	| ExactValue
	| ApproximateValue


(* Convert a Num.num to a string *)
let string_of_number print_type number =
	match print_type with
	| ExactValue -> Num.string_of_num number
	| ApproximateValue -> string_of_float (Num.float_of_num number)


(* Convert an operator into a string *)
let string_of_op = function
	(* Equal *)
	| Opi_eq -> "="
	(* Greater or equal *)
	| Opi_ge -> ">="
	(* Greater *)
	| Opi_g -> ">"


(* Convert a member into a string *)
let string_of_member print_type variable_names (monomial_array, constant) =
	(* First monomial ? *)
	let first = ref true in
	(* The result string *)
	let result = ref "" in
	(* Iterate on the variables *)
	for variable_index = 0 to Array.length monomial_array - 1 do
		let coeff = monomial_array.(variable_index) in
		(* Do not consider coeff equal to 0 *)
		if coeff <>/ (Num.num_of_int 0) then(
			(* Add a ' + ' to the result except if first monomial *)
			if !first then(
				first := false;
				(* Sign of the coeff if < 0 *)
				let sign = if coeff >=/ (Num.num_of_int 0) then "" else "-" in
				result := !result ^ sign ^ " ";
			)else(
				(* Sign of the coeff *)
				let sign = if coeff >=/ (Num.num_of_int 0) then "+" else "-" in
				result := !result ^ " " ^ sign ^ " ";
			);
			result := !result
				^ (if coeff = (Num.num_of_int 1) or coeff = (Num.num_of_int (-1)) then "" else (string_of_number print_type (Num.abs_num coeff) ^ " * "))
				^ variable_names.(variable_index);
		)
	done;
	(* Convert the constant, if non null OR no variables where printed *)
	if constant <>/ (Num.num_of_int 0) or !first then(
		(* Sign of the constant *)
		let sign = if constant >=/ (Num.num_of_int 0) then "+" else "-" in
		(* Add the sign if not first or < 0 *)
		if not !first or constant <=/ (Num.num_of_int 0) then (result := !result ^ " " ^ sign ^ " ");
		result := !result ^ (string_of_number print_type (Num.abs_num constant))
	);
	!result


(* Convert an inequality into a string *)
let string_of_inequality print_type variable_names (member, op) =
	(string_of_member print_type variable_names member)
	^ " " ^ (string_of_op op) ^ " 0"
(*	^ (string_of_member variable_names right)*)


(* Convert a constraint into a string *)
let string_of_constraint print_type variable_names c =
	let rec string_of_constraint_rec = function
		| [] -> ""
		| ineq :: list_of_ineq -> "\n & " ^ (string_of_inequality print_type variable_names ineq)
			^ (string_of_constraint_rec list_of_ineq)
	in
	match c with
	| [] -> ""
	| ineq :: list_of_ineq -> (string_of_inequality print_type variable_names ineq)
		^ (string_of_constraint_rec list_of_ineq)


(* Convert a member into a string with exact value*)
let string_of_exact_member =
	string_of_member ExactValue

(* Convert an inequality into a string with exact value*)
let string_of_exact_inequality =
	string_of_inequality ExactValue

(* Convert a constraint into a string with exact value *)
let string_of_exact_constraint =
	string_of_constraint ExactValue

(* Convert a constraint into a string with approximate value *)
let string_of_approximate_constraint =
	string_of_constraint ApproximateValue

