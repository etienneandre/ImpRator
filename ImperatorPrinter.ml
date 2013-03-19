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
 * Created:       03/03/2009
 * Last modified: 06/04/2009
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open AbstractImperatorFile
open Global
open Num


(****************************************************************)
(** Print Mode *)
(****************************************************************)
type print_mode =
	| InstantiatedPrint
	| ParametricPrint


(****************************************************************)
(** String of variables *)
(****************************************************************)

(* Convert a probability to a string *)
let string_of_probability = Num.string_of_num


(****************************************************************)
(** String of structures *)
(****************************************************************)

(* Convert the cost1 and the cost2 to a string *)
let string_of_costs state_index label_index abstract_program print_mode =
	match print_mode with
	(* Instantiated print : print the instantiated cost1 and cost2 *)
	| InstantiatedPrint ->
		  "[" ^ (Num.string_of_num abstract_program.instantiated_costs1.(state_index).(label_index)) ^ "]"
		^ "[" ^ (Num.string_of_num abstract_program.instantiated_costs2.(state_index).(label_index)) ^ "]"
	(* Parametric print : find the right labels for cost1 and cost2 *)
	| ParametricPrint ->
		let string_of_parametric_cost = function
			| ConstCost cost_value -> Num.string_of_num cost_value
			| ParametricCost cost_index -> abstract_program.cost_parameters.(cost_index)
		in
		"[" ^ (string_of_parametric_cost abstract_program.parametric_costs1.(state_index).(label_index)) ^ "]["
		^ (string_of_parametric_cost abstract_program.parametric_costs2.(state_index).(label_index)) ^ "]"


(* Make a transition with a proba and a dest_state_name *)
let make_transition proba dest_state_name =
	"\n\t\t->"
	(* Probability *)
	^ " (" ^ proba ^ ") "
	(* Dest state name *)
	^ dest_state_name ^ ";"


(* Convert to a string a transition to a dest_state_index with a proba *)
let string_of_transition dest_state_index proba abstract_program =
	(* Print transition only if proba > 0 *)
	if proba =/ (num_of_int 0) then "" else
	make_transition (string_of_probability proba) abstract_program.states.(dest_state_index)


(* Compute a transition of absorbing state if the sum of transitions from state_index to label_index is < 1 *)
let string_of_transition_to_absorbing_state abstract_program state_index label_index =
	(* Compute sum of probabilities *)
	let sum = sum_of_probabilities abstract_program state_index label_index in
		if sum </ (num_of_int 1)
		then make_transition (string_of_probability ((num_of_int 1) -/ sum)) "absorbing"
		else ""


(* Convert the transitions from state_index via label_index to a string *)
let string_of_transitions state_index label_index abstract_program =
	string_of_array_of_string
	(Array.mapi
		(fun dest_state_index proba ->
			string_of_transition dest_state_index proba abstract_program
		)
		abstract_program.transition_matrices.(label_index).(state_index)
	)
	(* Add the absorbing_state if the sum of probabilities is < 1 *)
	^ string_of_transition_to_absorbing_state abstract_program state_index label_index


(* Convert a state to a string *)
let string_of_state state_index abstract_program print_mode =
	(* State name *)
	"\nstate " ^ (abstract_program.states).(state_index) ^ ":" ^
	(* Transitions *)
	List.fold_left
		(fun the_string label_index ->
			the_string
			^ "\n\t"
			(* Label name *)
			^ abstract_program.labels.(label_index)
			(* Costs *)
			^ (string_of_costs state_index label_index abstract_program print_mode)
			(* Transitions *)
			^ (string_of_transitions state_index label_index abstract_program)
		)
		""
		(abstract_program.labels_per_state).(state_index)


(* Convert a program into a string *)
let string_of_program abstract_program print_mode =
	string_of_array_of_string (
		Array.mapi
		(fun state_index _ -> string_of_state state_index abstract_program print_mode)
		abstract_program.states
	)

(* Convert a program into a string *)
let string_of_instantiated_program abstract_program =
	string_of_program abstract_program InstantiatedPrint
	
(* Convert a program into a string *)
let string_of_parametric_program abstract_program =
	string_of_program abstract_program ParametricPrint
