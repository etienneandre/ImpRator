(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       18/03/2009
 * Last modified: 06/04/2009
 *
 ****************************************************************)



(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open Num


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram


(****************************************************************)
(** Variables and names *)
(****************************************************************)

(** Name of an identifier *)
type name = string

(* Index of a state *)
type state_index = int

(* Index of a transition label *)
type label_index = int

(* Cost of a state : we allow both rational constants and parameters *)
type tmp_cost =
	| TmpConstCost of Num.num
	| TmpParametricCost of string

(* Cost of a state : we allow both rational constants and parameters *)
type cost =
	| ConstCost of Num.num
	| ParametricCost of int (* index of the cost parameter *)


(** Probability of a transition : rational *)
type probability = Num.num


(****************************************************************)
(** States and transitions *)
(****************************************************************)

type destState =
	| NormalState of string
	| AbsorbingState
	
(*
(* A "label_and_transitions" is a label with its associated transition list *)
type label_and_transitions = name * tmp_cost * (probability * destState) list	
type state = name * label_and_transitions list
*)

(****************************************************************)
(** Costs definition *)
(****************************************************************)
type costs_definition = (name * Num.num) list


(****************************************************************)
(** Parsing structure *)
(****************************************************************)
(* Parsing structure : state_name * (label_name * cost1 * cost2 * (proba * dest_name) list) list *)
type parsing_structure = (name * (name * tmp_cost * tmp_cost * (probability * destState) list ) list ) list


(****************************************************************)
(** Abstract program *)
(****************************************************************)
type abstract_program = {
	(* Number of states *)
	nb_states : int;
	(* Number of labels *)
	nb_labels : int;
	(* Number of cost1 parameters *)
	nb_cost1_parameters : int;
	(* Number of cost2 parameters *)
	nb_cost2_parameters : int;
	(* Number of cost1 and cost2 parameters *)
	nb_parameters : int;
	(* Array of tm indexed by the labels *)
	transition_matrices : Matrix.matrix array;
	(* Array of states indexed by the state_index *)
	states : name array;
	(* Array of labels indexed by the label_index *)
	labels : name array;
	(* Array of cost names indexed by the cost1 and then cost2 index *)
	cost_parameters : name array;
	(* Array of cost values indexed by the cost1 and then cost2 index *)
	cost_values : Num.num array;
	(* Array of (array of parametric costs indexed by the label_index) index by the state_index *)
	parametric_costs1 : (cost array) array;
	(* Array of (array of instantiated costs (Num.num) indexed by the label_index) index by the state_index *)
	instantiated_costs1 : Matrix.matrix;
	(* Array of (array of parametric costs indexed by the label_index) index by the state_index *)
	parametric_costs2 : (cost array) array;
	(* Array of (array of instantiated costs (Num.num) indexed by the label_index) index by the state_index *)
	instantiated_costs2 : Matrix.matrix;
	(* Arrat of (List of label_index) indexed by the state_index *)
	labels_per_state : (label_index list) array;
}


(****************************************************************)
(** Functions to get the variable names from the parsing structure *)
(****************************************************************)

(* Compute the list of (possible identical) state names *)
let get_state_names parsing_structure =
	(* Get state name in a state*)
	let get_state_names state_names (state_name, _) =
		state_name :: state_names
	in
	(* Reverse list *)
	List.rev (List.fold_left get_state_names [] parsing_structure)


(* Compute the list of (all different) parametric cost1 and cost2 variables *)
let get_cost_parameter_names parsing_structure =
	(* Get cost parameters in a state *)
	let get_cost_parameters (list_of_names1, list_of_names2) (_, list_of_labels) =
		(* Get cost parameter in a label *)
		let get_cost_parameter (list_of_names1, list_of_names2) (_, cost1, cost2, _) =
			let extended_list_of_names1 =
			match cost1 with
				| TmpConstCost _ -> list_of_names1
				| TmpParametricCost p -> if List.mem p list_of_names1 then list_of_names1 else p :: list_of_names1
			in let extended_list_of_names2 =
			match cost2 with
				| TmpConstCost _ -> list_of_names2
				| TmpParametricCost p -> if List.mem p list_of_names2 then list_of_names2 else p :: list_of_names2
			in (extended_list_of_names1, extended_list_of_names2)
		in
		List.fold_left get_cost_parameter (list_of_names1, list_of_names2) list_of_labels
	in
	(* Reverse list to keep the right order *)
	let list_of_names1, list_of_names2 = List.fold_left get_cost_parameters ([], []) parsing_structure in
	List.rev list_of_names1, List.rev list_of_names2


(* Compute the list of (all different) transition labels of a program *)
let get_transition_labels parsing_structure =
	(* Get transition labels in a state *)
	let get_transition_labels list_of_transition_labels (_, list_of_label_and_transitions) =
		(* Get transition label in a label_and_transitions *)
		let get_transition_label list_of_transition_labels (label_name, _, _, _) =
			if List.mem label_name list_of_transition_labels then list_of_transition_labels
			else label_name :: list_of_transition_labels
		in
		List.fold_left get_transition_label list_of_transition_labels list_of_label_and_transitions
	in
	(* Reverse list *)
	List.rev (List.fold_left get_transition_labels [] parsing_structure)


(****************************************************************)
(** Verification functions *)
(****************************************************************)
exception AlreadyFound

(* Check that all the elements of a list are different *)
let all_different l =
	let res =
	try(
	let _ = List.fold_left
		(fun list_of_elements elem ->
			if List.mem elem list_of_elements then raise AlreadyFound;
			elem :: list_of_elements)
		[]
		l
	in true
	) with AlreadyFound -> false
	in
	res


(* Check that all the state names used in the parsing structure have been defined ; if not, print error messages *)
let check_existence_of_state_names parsing_structure defined_state_names =
	(* Get all destination state names in a state *)
	let get_dest_states list_of_dest_states (_, list_of_labels) =
		(* Get all destination state names in 'label_and_transitions' *)
		let get_dest_states list_of_dest_states (_, _, _, list_of_prob_and_names) =
			(* Get all destination state names in prob_and_name *)
			let get_dest_states list_of_dest_states (_, dest_state) =
				match dest_state with
				| AbsorbingState -> list_of_dest_states
				| NormalState s ->
					if List.mem s list_of_dest_states then list_of_dest_states
					else s :: list_of_dest_states
			in List.fold_left get_dest_states list_of_dest_states list_of_prob_and_names
		in List.fold_left get_dest_states list_of_dest_states list_of_labels
	in
	let dest_state_names = List.fold_left get_dest_states [] parsing_structure in
	(* Check that all states are defined *)
	let is_state_defined result state_name =
		if List.mem state_name defined_state_names then result
		else (print_debug_message mode_STANDARD mode_ERROR ("State name " ^ state_name ^ " is used but was not defined."); false)
	in List.fold_left is_state_defined true dest_state_names


(* Check that the absorbing state exists somewhere *)
let check_existence_of_absorbing_state parsing_structure =
	try(
		(* Check in every state *)
		List.iter (fun (_, list_of_labels) ->
			(* Check in 'label_and_transitions' *)
			List.iter (fun (_, _, _, list_of_prob_and_dest_states) ->
				(* Check in 'prob_and_dest_states' *)
				List.iter (fun (_, dest_state) ->
					match dest_state with
					| AbsorbingState -> raise AlreadyFound
					| NormalState _ -> ()
				) list_of_prob_and_dest_states
			) list_of_labels
		) parsing_structure;
		false
	) with AlreadyFound -> true


(* Check that the sum of the probabilities to leave a state taking a specific action are equal to 1 *)
let check_probabilities parsing_structure =
	(* Check in every state *)
	List.fold_left (fun all_proba_ok (state_name, list_of_labels) ->
		(* Check in 'label_and_transitions' *)
		List.fold_left (fun all_proba_ok (transition_label, _, _, list_of_prob_and_dest_states) ->
			(* Compute sum of probabilities *)
			let compute_sum list_of_prob_and_dest_states =
				List.fold_left (fun i (proba, _) -> Num.add_num i proba) (Num.num_of_int 0) list_of_prob_and_dest_states
			in
			(* Check that the sum is equal to 1 *)
			let sum = compute_sum list_of_prob_and_dest_states in
			if sum =/ (Num.num_of_int 1) then all_proba_ok
			else (print_debug_message mode_STANDARD mode_ERROR  ("Sum of probabilities from " ^ state_name ^ " through transition '" ^ transition_label ^ "' should be equal to 1, here " ^ (Num.string_of_num sum) ^ "."); false)
		) all_proba_ok list_of_labels
	) true parsing_structure


(* Check that the sets of costs1 and costs2 names are disjoint *)
let disjoint_cost_names list_of_cost1_names list_of_cost2_names =
	List.fold_left (fun disjoint cost1_name ->
		if List.mem cost1_name list_of_cost2_names then(
			print_debug_message mode_STANDARD mode_ERROR (
				"Parameter name " ^ cost1_name ^ " is both a cost1 and a cost2 name."); false)
		else disjoint
		) true list_of_cost1_names


(* Check that all parameters in the program are given a value (and warns if the cost parameteres in C0 are not all different, or if some parameters are not used) *)
let check_cost_parameters cost_parameter_names costs_definition =
	(* Check that all the defined cost parameters are different (if not, only warns) *)
	let distinct_names = List.fold_left
		(fun list_of_names (cost_name, _) ->
			if List.mem cost_name list_of_names then(
				print_warning ("The cost parameter " ^ cost_name ^ " is defined twice."); list_of_names
			) else cost_name :: list_of_names )
		[]
		costs_definition
	in
	(* Check that all the parameters of the program are given a value (if not, return false) *)
	List.fold_left
		(fun all_ok cost_name ->
			if not (List.mem cost_name distinct_names) then(
				print_debug_message mode_STANDARD mode_ERROR ("The cost parameter " ^ cost_name ^ " is used but is not given an instantiated value.");
				false
			) else all_ok
		)
		true
		cost_parameter_names

(* Check that all the defined cost parameters represent a parameter of the program (if not, only warns) *)
let warn_if_unused_cost_parameters list_of_cost1_names list_of_cost2_names costs_definition =
	List.iter
	(fun (cost_name, _) ->
		if not (List.mem cost_name list_of_cost1_names)
		&& not (List.mem cost_name list_of_cost2_names)
		then print_warning ("The cost parameter " ^ cost_name ^ " does not appear in the program.")
	)
	costs_definition


(****************************************************************)
(** Creation functions *)
(****************************************************************)

(* Create the transition matrices, the costs array, and the array of labels per state *)
let make_matrices parsing_structure index_of_states index_of_labels index_of_cost_parameters cost_values =
	(* Numbers *)
	let nb_states = Hashtbl.length index_of_states in
	let nb_labels = Hashtbl.length index_of_labels in
	(* Create an empty array of matrices *)
	let transition_matrices = Array.map
		(fun _ -> Matrix.make_matrix nb_states)
		(Array.create nb_labels 0)
	in
	(* Create the matrix of parametric costs1 : Costs1[s][l] gives the cost1 for leaving state s firing transition l *)
	let parametric_costs1 = Array.make_matrix nb_states nb_labels (ConstCost (Num.num_of_int 0)) in
	(* Create the matrix of instantiated costs1 : Costs1[s][l] gives the cost1 for leaving state s firing transition l *)
	let instantiated_costs1 = Array.make_matrix nb_states nb_labels (Num.num_of_int 0) in
	
	(* Create the matrix of parametric costs2 : Costs2[s][l] gives the cost2 for leaving state s firing transition l *)
	let parametric_costs2 = Array.make_matrix nb_states nb_labels (ConstCost (Num.num_of_int 0)) in
	(* Create the matrix of instantiated costs2 : Costs2[s][l] gives the cost2 for leaving state s firing transition l *)
	let instantiated_costs2 = Array.make_matrix nb_states nb_labels (Num.num_of_int 0) in

	(* Create the array of labels per state *)
	let labels_per_state = Array.make (Hashtbl.length index_of_states) [] in
	(* Loop on states *)
	List.iter (fun (state_name_orig, list_of_labels) ->
		(* Loop on labels *)
		let state_orig_index = Hashtbl.find index_of_states state_name_orig in
		(* Reverse labels per state to keep the same order *)
		labels_per_state.(state_orig_index) <- List.rev (List.fold_left
			(fun list_of_labels (transition_label, cost1, cost2, list_of_transitions) -> 
				let label_index = Hashtbl.find index_of_labels transition_label in
				(* Update transition matrices *)
				List.iter (fun (proba, dest_state) ->
					match dest_state with
					(* Do not take absorbing states into account *)
					| AbsorbingState -> ()
					| NormalState dest_state_name ->
						(* Find the index of dest state *)
						let dest_state_index = Hashtbl.find index_of_states dest_state_name in
						(* Update transition matrix by adding this new value *)
						transition_matrices.(label_index).(state_orig_index).(dest_state_index)
							<- transition_matrices.(label_index).(state_orig_index).(dest_state_index) +/ proba
					) list_of_transitions;
				(* Update both cost1 matrices *)
				let _ = match cost1 with
					| TmpParametricCost cost_name ->
						let cost_index = Hashtbl.find index_of_cost_parameters cost_name in
						instantiated_costs1.(state_orig_index).(label_index) <- cost_values.(cost_index);
						parametric_costs1.(state_orig_index).(label_index) <- ParametricCost cost_index
					| TmpConstCost cost_value ->
						instantiated_costs1.(state_orig_index).(label_index) <- cost_value;
						parametric_costs1.(state_orig_index).(label_index) <- ConstCost cost_value
				in
				(* Update both cost2 matrices *)
				let _ = match cost2 with
					| TmpParametricCost cost_name ->
						let cost_index = Hashtbl.find index_of_cost_parameters cost_name in
						instantiated_costs2.(state_orig_index).(label_index) <- cost_values.(cost_index);
						parametric_costs2.(state_orig_index).(label_index) <- ParametricCost cost_index
					| TmpConstCost cost_value ->
						instantiated_costs2.(state_orig_index).(label_index) <- cost_value;
						parametric_costs2.(state_orig_index).(label_index) <- ConstCost cost_value
				in
				(* Update labels per state *)
				if List.mem label_index list_of_labels
					then list_of_labels
					else label_index :: list_of_labels
			) [] list_of_labels
		)
	) parsing_structure;
	(* Return the 3 structures *)
	(transition_matrices, parametric_costs1, instantiated_costs1, parametric_costs2, instantiated_costs2, labels_per_state)



(****************************************************************)
(** Main conversion function *)
(****************************************************************)

(* Convert the result of the parsing into a valid program ; raise InvalidProgram if invalid program*)
let make_AbstractImperatorFile (parsing_structure, costs_definition, beta) =
	(* First check that the program is non empty *)
	if parsing_structure = [] then (
		print_debug_message mode_STANDARD mode_ERROR  ("The program seems to be empty.");
		raise InvalidProgram);
	
	(* Get the defined state names *)
	let list_of_state_names = get_state_names parsing_structure in
	(* Get the transition labels *)
	let list_of_labels = get_transition_labels parsing_structure in
	(* Get all the (different) cost1 and cost2 parameter names *)
	let list_of_cost1_names, list_of_cost2_names = get_cost_parameter_names parsing_structure in
	
	(* Verify that all the defined state names are different *)
	let all_different = all_different list_of_state_names in
	(* Check that all destination state names are defined *)
	let all_states_defined = check_existence_of_state_names parsing_structure list_of_state_names in
	(* Check that the probabilities are ok *)
	let proba_ok = check_probabilities parsing_structure in
	(* Check that the costs1 and costs2 names are disjoint (not completely compulsory, but easier so) *)
	let disjoint_cost_names = disjoint_cost_names list_of_cost1_names list_of_cost2_names in
	
	(* Check that all parameters in the program are given a value (and warns if the cost parameteres in C0 are not all different) *)
	let costs1_ok = check_cost_parameters list_of_cost1_names costs_definition in
	let costs2_ok = check_cost_parameters list_of_cost2_names costs_definition in
	(* Warn if some parameters are not used *)
	warn_if_unused_cost_parameters list_of_cost1_names list_of_cost2_names costs_definition;
	
	(* Perform intersection and may raise exception *)
	if not (all_different && all_states_defined && proba_ok && disjoint_cost_names && costs1_ok && costs2_ok) then raise InvalidProgram;
	
	(* Numbers *)
	let nb_states = List.length list_of_state_names in
	let nb_labels = List.length list_of_labels in
	let nb_cost1_parameters = List.length list_of_cost1_names in
	let nb_cost2_parameters = List.length list_of_cost2_names in
	
	(* The array of state names ; index -> state name *)
	let states = Array.of_list list_of_state_names in
	(* A (constant) hash table state name -> index *)
	let index_of_states = Hashtbl.create nb_states in
	for i = 0 to nb_states - 1 do
		Hashtbl.add index_of_states states.(i) i;
	done;
	
	(* The array of transition label names ; index -> label name *)
	let labels = Array.of_list list_of_labels in
	(* A (constant) hash table state name -> index *)
	let index_of_labels = Hashtbl.create nb_labels in
	for i = 0 to nb_labels - 1 do
		Hashtbl.add index_of_labels labels.(i) i;
	done;
	
	(* The array of all cost parameter names : index -> parameter name *)
	let all_cost_parameters = list_of_cost1_names @ list_of_cost2_names in
	let cost_parameters = Array.of_list all_cost_parameters in
	(* A (constant) hash table parameter name -> cost_index *)
	let index_of_cost_parameters = Hashtbl.create (nb_cost1_parameters + nb_cost2_parameters) in
	for cost_index = 0 to (nb_cost1_parameters + nb_cost2_parameters) - 1 do
		Hashtbl.add index_of_cost_parameters cost_parameters.(cost_index) cost_index;
	done;
	
	(* The array of cost parameter values : index -> cost instantiated value *)
	let cost_values = Array.make (nb_cost1_parameters + nb_cost2_parameters) (Num.num_of_int 0) in
	List.iter (fun (cost_name, cost_value) ->
		(* Only consider the cost parameters used in the program *)
		if List.mem cost_name all_cost_parameters then(
			let cost_index = Hashtbl.find index_of_cost_parameters cost_name in
			cost_values.(cost_index) <- cost_value
		)
	) costs_definition;

	(* Create the transition matrices, the costs array, and the array of labels per state *)
	let transition_matrices, parametric_costs1, instantiated_costs1, parametric_costs2, instantiated_costs2, labels_per_state
		= make_matrices parsing_structure index_of_states index_of_labels index_of_cost_parameters cost_values in


	(* Check that the absorbing state exists *)
	let absorbing_present = check_existence_of_absorbing_state parsing_structure in
	(* If no absorbing state: apply beta discount *)
	if not absorbing_present then(
		let final_beta =
		(* If no beta defined: use default value *)
		if beta =/ (Num.num_of_int 0)
		then (
			print_warning ("The absorbing state was not found in the program, and no beta was defined."
			^ "\nThe default value beta = " ^ (string_of_num default_beta) ^ " will be used.");
			default_beta
		)else
		(* If beta is not in ]0..1[: use default value *)
		if beta </ (Num.num_of_int 0) or beta >=/ (Num.num_of_int 1)
		then(
			print_warning ("The absorbing state was not found in the program, and no beta was defined."
			^ "\nBy default, beta = " ^ (string_of_num default_beta) ^ ".");
			default_beta
		(* Else : keep the value of the program *)
		)else beta
		in
		(* Apply beta discount to all probabilities *)
		for label_index = 0 to Array.length labels - 1 do
			for state_orig_index = 0 to Array.length states - 1 do
				for state_dest_index = 0 to Array.length states - 1 do
					transition_matrices.(label_index).(state_orig_index).(state_dest_index)
						<- transition_matrices.(label_index).(state_orig_index).(state_dest_index) */ final_beta;
				done;
			done;
		done;
	
	(* If absorbing and beta, don't use beta *)
	)else if beta <>/ (Num.num_of_int 0) then
		print_warning ("As the absorbing state is used in the program, the value for beta will be ignored.");
	
	
	(* Make the structure *)
	{
	(* Number of states *)
	nb_states = Array.length states;
	(* Number of labels *)
	nb_labels = Array.length labels;
	(* Number of cost1 parameters *)
	nb_cost1_parameters = nb_cost1_parameters;
	(* Number of cost2 parameters *)
	nb_cost2_parameters = nb_cost2_parameters;
	(* Number of cost1 and cost2 parameters *)
	nb_parameters = nb_cost1_parameters + nb_cost2_parameters;
	(* Array of tm indexed by the labels *)
	transition_matrices = transition_matrices;
	(* Array of states indexed by the state_index *)
	states = states;
	(* Array of labels indexed by the label_index *)
	labels = labels;
	(* Array of cost names indexed by the cost1 and then cost2 index *)
	cost_parameters = cost_parameters;
	(* Array of cost values indexed by the cost1 and then cost2 index *)
	cost_values = cost_values;
	(* Array of (array of parametric costs indexed by the label_index) index by the state_index *)
	parametric_costs1 = parametric_costs1;
	(* Array of (array of instantiated costs (Num.num) indexed by the label_index) index by the state_index *)
	instantiated_costs1 = instantiated_costs1;
	(* Array of (array of parametric costs indexed by the label_index) index by the state_index *)
	parametric_costs2 = parametric_costs2;
	(* Array of (array of instantiated costs (Num.num) indexed by the label_index) index by the state_index *)
	instantiated_costs2 = instantiated_costs2;
	(* Arrat of (List of label_index) indexed by the state_index *)
	labels_per_state = labels_per_state;
	}


(****************************************************************)
(** Get functions *)
(****************************************************************)

(* Get the cost of leaving a state_index firing a label_index *)
(*
let get_cost1 state_index label_index abstract_program =
	abstract_program.instantiated_costs1.(state_index).(label_index)

let get_instantiated_cost1 state_index label_index abstract_program =
	abstract_program.instantiated_costs1.(state_index).(label_index)
*)
(* Compute the sum of probabilities from state_index via label_index *)
let sum_of_probabilities abstract_program state_index label_index =
	Array.fold_left
		add_num
		(Num.num_of_int 0)
		abstract_program.transition_matrices.(label_index).(state_index)








(****************************************************************)
(** Debugging functions *)
(****************************************************************)

(*
let dEBUG_print_labels_per_state abstract_program =
	print_newline();
	print_string "---Labels per state---";
	print_newline();
	Array.iteri (fun state list_of_labels ->
		print_string abstract_program.states.(state);
		print_string (" : [");
		List.iter (fun label -> print_string (abstract_program.labels.(label) ^ ", ")) list_of_labels;
		print_string "]";
		print_newline())


let dEBUG_print_costs states =
	print_newline();
	print_string "---Costs---";
	print_newline();
	Array.iteri (fun state cost ->
		print_string states.(state);
		print_string (" -> " ^ (string_of_num cost));
		print_newline())


let dEBUG_print_policy states labels =
	print_newline();
	print_string "---Policy---";
	print_newline();
	Array.iteri (fun state policy ->
		print_string (states.(state) ^ " -> " ^ labels.(policy));
		print_newline())


let dEBUG_print_array a=
	print_newline();
	print_string "---Array---";
	print_newline();
	Array.iteri (fun i s -> print_int i; print_string (" -> " ^ s); print_newline()) a


let dEBUG_print_transition_matrix tm states =
	Array.iteri (fun state_index_origin t ->
		Array.iteri (fun state_index_dest proba ->
			print_string ("\n    " ^ states.(state_index_origin) ^ 
			" -> (" ^ (Num.string_of_num proba) ^ ")" ^ states.(state_index_dest))
		) t
	) tm
	

let dEBUG_print_tmatrices transition_matrices states labels =
	print_newline();
	print_string "---Transition matrices---";
	print_newline();
	Array.iteri (fun label_index tm ->
		print_string ("  TM for Label " ^ labels.(label_index));
		(*
		Array.iteri (fun state_index_origin t ->
			Array.iteri (fun state_index_dest proba ->
				print_string ("\n    " ^ states.(state_index_origin) ^ 
				" -> (" ^ (Fractions.string_of_fraction proba) ^ ")" ^ states.(state_index_dest))
			) t
		) tm;*)
		dEBUG_print_transition_matrix tm states;
		print_newline()) transition_matrices
*)
