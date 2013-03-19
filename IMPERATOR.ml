(*****************************************************************
 *
 *                     IMPERATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       03/03/2009
 * Last modified: 22/04/2009
 *
 ****************************************************************)



(****************************************************************)
(** TO DO *)
(****************************************************************)


(**** a faire :
	- gestion du fichire d'entree et des paramètres !!!!!
*)


(**** optimisations :
	- algo instancie : ne pas chercher a optimiser une esperance deja nulle
	- creation d'inegalite : creer une fonction Constraint.minus : member -> member -> member
*)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractImperatorFile
open Num

(****************************************************************)
(** Exceptions *)
(****************************************************************)
(* Exception raised when the costs of a program are not fully instantiated *)
exception NonInstantiatedProgram of string




(****************************************************************)
(** Constants *)
(****************************************************************)
(* let fichier = "exCahier.imp" *)
(* let fichier = "exCahier2Couts.imp" *)
(* let fichier = "exCahier2CoutsSansBoucle.imp" *)
(* let fichier = "exRutgers.imp" *)
(* let fichier = "exSlidesMDP.imp" *)
(*  let fichier = "exMDP.imp" *)
(*  let fichier = "exDPM.imp" *)
(*  let fichier = "exToy.imp" *)
(* let fichier = "exRennesLSV.imp" *)
(* let fichier = "exParisLSV.imp" *)
(* let fichier = "exParisBologne.imp" *)
let fichier = "exINFINITY.imp"

let debug_mode = mode_STANDARD
(* let debug_mode = mode_LOW_DEBUG *)
(* let debug_mode = mode_HIGH_DEBUG *)

(****************************************************************)
(** Min/max *)
(****************************************************************)
let is_better value optimum = value </ optimum



(****************************************************************)
(** Useful function *)
(****************************************************************)
(* Print a message in function of debug mode *)
let print_message message debug_level =
	print_debug_message debug_mode debug_level message


(* Convert a Num.num into a string, under the form of "exact value (approx value)" *)
let string_float_of_num num =
	(string_of_num num)
	^ (
		if Num.is_integer_num num then ""
		else (" ( = " ^ (string_of_float (Num.float_of_num num)) ^ ")")
	)


(****************************************************************)
(** Computation functions for Policy Iteration *)
(****************************************************************)
(*
(* Compute the initial policy optimal wrt to the costs in abstract_program *)
let optimal_initial_policy abstract_program =
	(* Creation of an empty policy *)
	let policy = Array.make (Array.length abstract_program.states) 0 in
	(* Iteration on the states *)
	Array.iteri
	(fun state_index _ ->
		(* Get the current policy and esperance of this state_index *)
		let current_policy = policy.(state_index) in
		let current_esperance = Matrix.mult_vector_vector abstract_program.transition_matrices.(current_policy).(state) abstract_program.costs blublublublublu in

	)
	abstract_program.states
	;
	(* Return the optimal policy *)
	policy
	*)


(* Create an empty policy *)
let initial_policy nb_states labels_per_state =
	(* Creation of an empty policy *)
	let policy = Array.make nb_states 0 in
	(* Update to at least set a policy which exists for this state! *)
	Array.iteri
	(fun state_index list_of_labels ->
		(* Update utility *)
		match list_of_labels with
		| [] -> ()
		| label :: _ -> policy.(state_index) <- label
	) labels_per_state
	(* A CHANGER ABSOLUMENT surtout si la transition 0 n'existe pas pour l'etat *)
	;
	(* Return policy *)
	policy


(* Create the utility equal to the costs considering the politic *)
let initialize_utility nb_states instantiated_costs policy =
	(* Create an empty utility *)
	let utility = Array.make nb_states (Num.num_of_int 0) in
	(* Iteration on state and policy *)
	Array.iteri
	(fun state_index policy ->
		(* Update utility *)
		utility.(state_index) <- instantiated_costs.(state_index).(policy)
	)
	policy;
	(* Return the utility *)
	utility


(* Create a transition matrix, where each transition is conform to the policy of the state *)
let make_transition_matrix_from_policy transition_matrices policy =
	let nb_states = Array.length policy in
	(* Create an empty matrix *)
	let tm = Matrix.make_matrix nb_states in
	(* Fill in every transition *)
	for i = 0 to nb_states - 1 do
		for j = 0 to nb_states - 1 do
			tm.(i).(j) <- transition_matrices.(policy.(i)).(i).(j);
		done;	
	done;
(*	Array.iteri (fun state label ->
		tm.(state) <- transition_matrices.(label).(state)
	) policy;*)
	(* Return tm *)
	tm


(* Create an array of instantiated costs, wrt to the current policy *)
let make_current_instantiated_costs instantiated_costs policy =
	(* Create an empty array *)
	let current_costs = Array.make (Array.length policy) (Num.num_of_int 0) in
	(* Fill in every state *)
	Array.iteri (fun state_index label_index ->
		current_costs.(state_index) <- instantiated_costs.(state_index).(label_index)
	) policy;
	(* Return the costs *)
	current_costs


(* Determine the value (expected cost) of every state *)
let value_determination transition_matrices policy instantiated_costs =
	(* Compute the current transition matrix *)
	let tm = make_transition_matrix_from_policy transition_matrices policy in
	
	(* Compute the current costs wrt policy *)
	let current_instantiated_costs = make_current_instantiated_costs instantiated_costs policy in
	
	(* We here solve the equation u = costs + tm * u, i.e., u = (I - tm)-1 * costs *)
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG "tm := " ;
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG (Matrix.string_of_matrix tm) ;
	
(* 	let tm = Matrix.mult_matrix_scalar tm ((Num.num_of_int 8) // (Num.num_of_int 10)) in *)
	
	let iN = Matrix.i_n (Array.length tm) in
	
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG "iN := " ;
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG (Matrix.string_of_matrix iN) ;
	
	let iN_minus_tm = Matrix.add iN (Matrix.opposite tm) in
	
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG "iN - tm := " ;
	print_debug_message mode_STANDARD mode_TOTAL_DEBUG (Matrix.string_of_matrix iN_minus_tm) ;

	let invert = Matrix.invert iN_minus_tm in
	Matrix.mult_matrix_vector invert current_instantiated_costs


(* Compute the sum of the costs of the whole program, w.r.t. a policy *)
let compute_global_cost transition_matrices policy instantiated_costs =
	(* Compute the utility function : U := (I - TM)-1 * Costs *)
	let u = value_determination transition_matrices policy instantiated_costs in
	(* Compute the sum *)
	Array.fold_left Num.add_num (Num.num_of_int 0) u


(****************************************************************)
(** Computation functions for parametric algorithm *)
(****************************************************************)

(* Create an array of parametic costs, wrt to the current policy *)
let make_current_parametric_cost parametric_costs policy =
	(* Create an empty array *)
	let current_costs = Array.make (Array.length policy) (ConstCost (Num.num_of_int 0)) in
	(* Fill in every state *)
	Array.iteri (fun state_index label_index ->
		current_costs.(state_index) <- parametric_costs.(state_index).(label_index)
	) policy;
	(* Return the costs *)
	current_costs


(* Convert a AbstractImperatorFile.cost into a Constraint.member *)
let member_of_cost nb_parameters = function
	(* Constant cost : no array, and a constant *)
	| ConstCost c -> Array.make nb_parameters (Num.num_of_int 0), c
	(* Parametric cost : an updated array, and no constant *)
	| ParametricCost cost_index -> let monomials_array = Array.make nb_parameters (Num.num_of_int 0) in
		monomials_array.(cost_index) <- (Num.num_of_int 1);
		monomials_array, (Num.num_of_int 0)


(* Return a Constraint.member as the result of the vectorial product of an array of proba * an array of Constraint.member *)
let make_product_proba_array_member_array proba_array member_array nb_parameters =
	(* Size of the arrays *)
	let n = Array.length proba_array in
	if n <> Array.length member_array
		then raise (Matrix.DifferentSize "An array of proba and an array of members must have the same size to be multiplied.");
	(* Create an empty member *)
	let member = ref (Array.make nb_parameters (Num.num_of_int 0), (Num.num_of_int 0)) in
	(* Iterate on every member *)
	for i = 0 to n-1 do
		(* Multiply the member by the proba *)
		let mult = Constraint.mult_member member_array.(i) proba_array.(i) in
		(* Add to the former members *)
		member := (Constraint.add_members !member mult)
	done;
	(* Return the member *)
	!member


(* Return a Constraint.member as the result of a (V : Num.num array) * (array_of_costs : cost array)*)
let make_parametric_row v array_of_costs nb_parameters =
	(* Create an empty vector of variables *)
	let monomials = Array.make nb_parameters (Num.num_of_int 0) in
	(* Create an empty constant *)
	let constant = ref (Num.num_of_int 0) in
	for j = 0 to (Array.length v - 1) do
		(* Case : constant cost or variable ? *)
		match array_of_costs.(j) with
		| ConstCost c -> constant := !constant +/ c */ v.(j);
		| ParametricCost cost_index ->
			monomials.(cost_index) <- monomials.(cost_index) +/ v.(j)
	done;
	(* Return the member *)
	(monomials, !constant)


(* Return a vector of Constraint.member as the result of M * array_of_costs *)
let parametric_value_determination abstract_program policy (*m array_of_costs nb_parameters *) =
	(* Numbers *)
	let nb_states = abstract_program.nb_states in
	let nb_parameters = Array.length abstract_program.cost_parameters in
	(* Update current transition matrix with policy *)
	let tm = make_transition_matrix_from_policy abstract_program.transition_matrices policy in
	(* Compute (I - TM)^{-1} *)
	let iN = Matrix.i_n nb_states in
	let iN_minus_tm = Matrix.add iN (Matrix.opposite tm) in
	let invert = Matrix.invert iN_minus_tm in
	
	(* Compute the array of costs w.r.t. the policy *)
	let current_parametric_costs = make_current_parametric_cost abstract_program.parametric_costs1 policy in
	if nb_states <> Array.length current_parametric_costs
		then raise (InternalError "The TM and the array_of_costs have a different size in function parametric_value_determination.");
	
	(* Compute (I - TM)^{-1} * current_parametric_costs *)
	(* Create a fresh new vector of members *)
	let vector_of_members = Array.make nb_states (Array.make nb_parameters (Num.num_of_int 0), Num.num_of_int 0) in
	(* Fill it *)
	for state = 0 to nb_states - 1 do
		vector_of_members.(state) <- make_parametric_row invert.(state) current_parametric_costs nb_parameters;
	done;
	(* Return the vector of members *)
	vector_of_members


(****************************************************************)
(** Print functions *)
(****************************************************************)
(* Print the array of policy : state_index -> label_index *)
let print_policy debug_mode states labels =
	Array.iteri (fun state_index label_index ->
		print_message (
			states.(state_index)
			^ "-> "
			^ labels.(label_index)
		) debug_mode
	)

(* Print the array of utility : state_index -> cost *)
let print_utility debug_mode states =
(* 	print_message "---- Utility ----" debug_mode; *)
	Array.iteri (fun state_index cost ->
		print_message (
			states.(state_index)
			^ "-> "
			(* Exact value *)
			^ (string_float_of_num cost)
		) debug_mode
	)



(****************************************************************)
(** Algorithm Policy Iteration *)
(****************************************************************)

let policy_iteration states labels labels_per_state transition_matrices instantiated_costs =
	(* Some numbers *)
	let nb_states = Array.length states in
	
	(* Policy : array state_index -> label_index ; initially random *)
	let policy = initial_policy nb_states labels_per_state in
	print_message "---- Policy ----" mode_MEDIUM_DEBUG;
	print_policy mode_MEDIUM_DEBUG states labels policy;
	
	(* u : utility function, initally identical to costs wrt policy *)
	let u = ref (initialize_utility nb_states instantiated_costs policy) in
	print_message "---- Utility ----" mode_HIGH_DEBUG;
	print_utility mode_HIGH_DEBUG states !u;
	
	(* Loop condition *)
	let unchanged = ref false in
	let nb_iterations = ref 1 in
	while (not !unchanged) do
		print_message ("  Iteration " ^ (string_of_int !nb_iterations) ^ " of policy iteration") mode_STANDARD;
	
		print_message "---- Policy ----" mode_HIGH_DEBUG;
		print_policy mode_HIGH_DEBUG states labels policy;
			(*
			(* Update the current transition matrix *)
			let tm = make_transition_matrix_from_policy transition_matrices policy in
			
			(* Update the current costs wrt policy *)
			let current_instantiated_costs = make_current_instantiated_costs instantiated_costs policy in
			
			print_message "---- Current costs ----" mode_HIGH_DEBUG;
			print_utility mode_HIGH_DEBUG states current_instantiated_costs;
			*)
		(* Update the utility function : U := (I - TM)-1 * Costs *)
		u := value_determination transition_matrices policy instantiated_costs;
		
		print_message "---- Utility ----" mode_HIGH_DEBUG;
		print_utility mode_HIGH_DEBUG states !u;
		
		(* Start *)
		unchanged := true;
		
		(* Iterate on states *)
		for state = 0 to nb_states - 1 do
			print_message ("Considering state " ^ states.(state)) mode_LOW_DEBUG;
			(* Get the current policy of this state *)
			let current_policy = policy.(state) in
			let current_esperance = !u.(state) in
			
			print_message ("  Current esperance: " ^ (string_float_of_num current_esperance)) mode_HIGH_DEBUG;
			
			(* Compute the best policy for this state *)
			let optimal_policy = ref current_policy in
			let optimum = ref current_esperance in
			(* Iterate on the labels of the state ONLY *)
			List.iter (fun label ->
			(* Do not consider label_index = P[state_index] *)
			if label <> policy.(state) then(
				(* Debug message *)
				print_message ("Considering action " ^ labels.(label)) mode_MEDIUM_DEBUG;
				(* Compute Sigma_{j}M_{s, j}^{a} U[j] *)
				let esperance = Matrix.mult_vector_vector transition_matrices.(label).(state) !u
					+/ instantiated_costs.(state).(label) in
				print_message (
					"  Esperance for policy "
					^ labels.(label) ^ ": "
					^ (string_float_of_num esperance)
				) mode_HIGH_DEBUG;
				(* If Sigma_{j}M_{s, j}^{a} U[j] < U[s]*)
				if is_better esperance !optimum then(
					optimum := esperance;
					optimal_policy := label;
				);
			)
			) labels_per_state.(state);
			(* If optimum is better than current: update policy *)
			if is_better !optimum current_esperance then (
				print_message ("The state "
					^ states.(state)
					^ " has now policy "
					^ labels.(!optimal_policy)
					)mode_STANDARD;
				print_message ("New esperance: " ^ (string_float_of_num !optimum)) mode_HIGH_DEBUG;
				policy.(state) <- !optimal_policy;
				unchanged := false;
			);
		done;
		(* Increment *)
		nb_iterations := !nb_iterations + 1;
	done;
	
	(* Return the policy and the utility *)
	(!u, policy, (!nb_iterations - 1))




(****************************************************************)
(****************************************************************)
(** Hello world! *)
(****************************************************************)
(****************************************************************)
;;
print_message ("***********************************************") mode_STANDARD;
print_message ("*                 IMPERATOR                   *") mode_STANDARD;
print_message ("*                              Etienne ANDRE  *") mode_STANDARD;
print_message ("*                                       2009  *") mode_STANDARD;
print_message ("*  Laboratoire Specification et Verification  *") mode_STANDARD;
print_message ("*               ENS de Cachan & CNRS, France  *") mode_STANDARD;
print_message ("***********************************************") mode_STANDARD;


(****************************************************************)
(** Parsing and printing *)
(****************************************************************)


(* Lexing *)
let lexbuf = try(
	Lexing.from_channel (open_in fichier)
) with
	| Sys_error e -> print_message ("The file " ^ fichier ^ " could not be opened.\n" ^ e) mode_ERROR; abort_program (); exit(0)
	| Failure f -> print_message ("Lexing error: " ^ f) mode_ERROR; abort_program (); exit(0)
in
(* Parsing *)
let abstract_program = try(
	ImperatorParser.program ImperatorLexer.token lexbuf
) with
	| Parsing.Parse_error -> print_message "Unknown parsing error. Check your program." mode_ERROR; abort_program (); exit(0)
	| ParsingError e -> print_message e mode_ERROR; print_message "Parsing error" mode_ERROR; abort_program (); exit(0)
	| InvalidProgram -> print_message "The input file contains errors. Please check it again." mode_ERROR; abort_program (); exit(0)
in


(****************************************************************)
(** Debug information *)
(****************************************************************)

(* Print parametric program *)
print_message "\nParametric program:" mode_HIGH_DEBUG;
print_message (ImperatorPrinter.string_of_parametric_program abstract_program) mode_HIGH_DEBUG;

(* Print instantiated program *)
print_message "\nInstantiated program:" mode_HIGH_DEBUG;
print_message (ImperatorPrinter.string_of_instantiated_program abstract_program) mode_HIGH_DEBUG;

(* Print state indexes *)
print_message "\nState indexes:" mode_HIGH_DEBUG;
Array.iteri (fun label_index label_name ->
	print_message ("  State " ^ (string_of_int label_index) ^ ": " ^ label_name) mode_HIGH_DEBUG;
) abstract_program.states;

(* Print label indexes *)
print_message "\nLabel indexes:" mode_HIGH_DEBUG;
Array.iteri (fun index name ->
	print_message ("  Label " ^ (string_of_int index) ^ ": " ^ name) mode_HIGH_DEBUG;
) abstract_program.labels;



(****************************************************************)
(****************************************************************)
(** Algorithm Policy Iteration *)
(****************************************************************)
(****************************************************************)

print_message ("*********************") mode_STANDARD;
print_message "ALGORITHM policy Iteration STARTED" mode_STANDARD;
print_message ("*********************") mode_STANDARD;

(* Call the policy iteration function *)
let u, policy, nb_iterations =
	policy_iteration
		abstract_program.states
		abstract_program.labels
		abstract_program.labels_per_state
		abstract_program.transition_matrices
		abstract_program.instantiated_costs1
in

print_message ("*********************") mode_STANDARD;
print_message ("Algorithm policy iteration ended after " ^ (string_of_int nb_iterations) ^ " iterations") mode_STANDARD;
print_message ("*********************") mode_STANDARD;

print_message "---- Final utility ----" mode_STANDARD;
print_utility mode_STANDARD abstract_program.states u;
print_message "---- Final policy ----" mode_STANDARD;
print_policy mode_STANDARD abstract_program.states abstract_program.labels policy;





(****************************************************************)
(****************************************************************)
(** Algorithm IMPERATOR *)
(****************************************************************)
(****************************************************************)

print_message ("\n*********************") mode_STANDARD;
print_message "ALGORITHM IMPERATOR STARTED" mode_STANDARD;
print_message ("*********************") mode_STANDARD;

(* Some useful information *)
let nb_parameters = Array.length abstract_program.cost_parameters in

(* Parameterized 'U' array *)
let u_param = parametric_value_determination abstract_program policy in

(* Print U_param ! *)
print_message "Parametric U for optimal policy:" mode_HIGH_DEBUG;
for i = 0 to abstract_program.nb_states - 1 do
	print_message (
		"State " ^ abstract_program.states.(i) ^ " : "
		^ (Constraint.string_of_exact_member abstract_program.cost_parameters u_param.(i))
		^ " ( = " ^ (string_float_of_num (Constraint.instantiate_member u_param.(i) abstract_program.cost_values) ^ ")")
	) mode_HIGH_DEBUG;
done;

(* Our result ! *)
let my_constraint = ref [] in

(* Iterate on states *)
for state_index = 0 to abstract_program.nb_states - 1 do
	print_message ("Considering state " ^ abstract_program.states.(state_index)) mode_STANDARD;
	
	(* Perform \Sigma_{j}M_{i, j}^{P[state]}U[j] *)
	let ref_member =
		u_param.(state_index)
	in
	(* Instantiate it *)
	let inst_ref_member = Constraint.instantiate_member ref_member abstract_program.cost_values in
	(* Print ref_member for debug *)
	print_message ("U[" ^ abstract_program.states.(state_index) ^ "] = "
		^ (Constraint.string_of_exact_member abstract_program.cost_parameters ref_member)
		^ " ( = " ^ (string_float_of_num inst_ref_member) ^ ")")
	mode_HIGH_DEBUG;
	
	(* Iterate on the labels of the state ONLY *)
	List.iter (fun label_index ->
		(* Do not consider label_index = P[state_index] *)
		if label_index <> policy.(state_index) then(
		
		(* Debug message *)
		print_message ("Considering action " ^ abstract_program.labels.(label_index)) mode_LOW_DEBUG;
		
		(* Perform \Sigma_{j}M_{i, j}^{label_index}U[j] + Costs_{label_index} ( state_index ) *)
		let current_member =
			Constraint.add_members
			(* Sigma_{j}M_{i, j}^{label_index}U[j] *)
			(make_product_proba_array_member_array
				abstract_program.transition_matrices.(label_index).(state_index)
				u_param
				nb_parameters
			)
			(* Costs_{label_index} ( state_index ) *)
			(member_of_cost
				nb_parameters
				abstract_program.parametric_costs1.(state_index).(label_index)
			)
		in
		(* Print current_member for debug *)
		print_message ("Sigma_{j}M_{" ^ abstract_program.states.(state_index) ^ ", j}^{"
			^ abstract_program.labels.(label_index) ^ "}U[j]"
			^ " + Costs_{" ^ abstract_program.labels.(label_index) ^ "}("
			^ abstract_program.states.(state_index) ^ ") = "
			^ (Constraint.string_of_exact_member abstract_program.cost_parameters current_member)
			^ " ( = " ^ (string_float_of_num inst_ref_member) ^ ")")
		mode_HIGH_DEBUG;
		
		(* Instantiate the second member *)
		let inst_current_member = Constraint.instantiate_member current_member abstract_program.cost_values in
		(* Make comparison : case '<' *)
		if inst_ref_member </ inst_current_member then
		(* Add inequality to the constraint *)
		let new_inequality = Constraint.make_inequality ref_member Constraint.Op_l current_member in
		print_message ("Adding constraint : "
			^ (Constraint.string_of_exact_inequality abstract_program.cost_parameters new_inequality)) mode_STANDARD;
		my_constraint := Constraint.add_inequality new_inequality !my_constraint;
		(* Make comparison : case '=' *)
		else if inst_ref_member =/ inst_current_member then
		(* Add inequality to the constraint *)
		let new_inequality = Constraint.make_inequality ref_member Constraint.Op_le current_member in
		print_message ("Adding constraint : "
			^ (Constraint.string_of_exact_inequality abstract_program.cost_parameters new_inequality)) mode_LOW_DEBUG;
		my_constraint := Constraint.add_inequality new_inequality !my_constraint;
		(* The policy is not optimal *)
		else raise (InternalError ("The policy is not optimal, as " ^ (string_float_of_num inst_ref_member) ^ " > " ^ (string_float_of_num inst_current_member)))
		)
	) abstract_program.labels_per_state.(state_index);

done;

print_message ("*********************") mode_STANDARD;
print_message ("Final constraint (" ^ (string_of_int  (List.length !my_constraint)) ^ " inequalities)") mode_STANDARD;
print_message ("*********************") mode_STANDARD;
print_message ("   With exact value:") mode_STANDARD;
print_message (Constraint.string_of_exact_constraint abstract_program.cost_parameters (List.rev !my_constraint)) mode_NODEBUG;
print_message ("*********************") mode_STANDARD;
print_message ("   With approximate value:") mode_STANDARD;
print_message (Constraint.string_of_approximate_constraint abstract_program.cost_parameters (List.rev !my_constraint)) mode_NODEBUG;
print_message ("*********************") mode_STANDARD;

print_message ("Algorithm IMPERATOR ended successfully (after " ^ (string_of_float (get_time())) ^ " seconds)") mode_STANDARD;
print_message ("*********************") mode_STANDARD;


(*

(****************************************************************)
(****************************************************************)
(** Algorithm IMPERATOR II : ' w ' *)
(****************************************************************)
(****************************************************************)

print_message ("\n*********************") mode_STANDARD;
print_message "ALGORITHM IMPERATOR II 'w' STARTED" mode_STANDARD;
print_message ("*********************") mode_STANDARD;

(* Some useful information *)
let nb_states = abstract_program.nb_states in
let nb_labels = abstract_program.nb_labels in

(* Min and max for dichotomy *)
let policy = ref (initial_policy nb_states abstract_program.labels_per_state) in

(* Min and max for dichotomy *)
let w_min = ref (Num.num_of_int 0) in
let w_max = ref (Num.num_of_int 1) in

let w = ref (Num.div_num (Num.num_of_int 1) (Num.num_of_int 2)) in

(* Loop condition *)
let close_enough = ref false in
let nb_iterations = ref 1 in
while (not !close_enough) do
	print_message ("\n---------------------") mode_STANDARD;
	print_message ("Iteration " ^ (string_of_int !nb_iterations) ^ " of IMPERATOR 'w'") mode_STANDARD;
	print_message ("---------------------") mode_STANDARD;
	
	(* Create the ' (1-w) c1 + w * c2 ' vector *)
	let costs = Array.make_matrix nb_states nb_labels (Num.num_of_int 0) in
	for state_index = 0 to nb_states - 1 do
(* 		let current_policy = !policy.(state_index) in *)
(* 		for label_index = 0 to nb_labels - 1 do *)

		(* Iterate on label per state *)
		List.iter (fun label_index ->
			let cost = ((Num.num_of_int 1) -/ !w) */ abstract_program.instantiated_costs1.(state_index).(label_index)
				+/ !w */ abstract_program.instantiated_costs2.(state_index).(label_index) in
(* 		for label_index = 0 to nb_labels - 1 do *)
			costs.(state_index).(label_index) <- cost;
			print_message ("costs[" ^ abstract_program.states.(state_index) ^ "][" ^ abstract_program.labels.(label_index) ^ "] := "
				^ (string_float_of_num cost)) mode_HIGH_DEBUG;
(* 		done; *)
		) abstract_program.labels_per_state.(state_index);
	done;
	
	(* Call the policy iteration function *)
	let u, new_policy, nb_it =
		policy_iteration
			abstract_program.states
			abstract_program.labels
			abstract_program.labels_per_state
			abstract_program.transition_matrices
			costs
	in
	
	(* Update policy *)
	policy := new_policy;
	
	print_message ("---- Current policy at iteration " ^ (string_of_int !nb_iterations) ^ " ----") mode_LOW_DEBUG;
	print_policy mode_LOW_DEBUG abstract_program.states abstract_program.labels !policy;
	
	(* Compute global cost1 *)
	let cost1 = compute_global_cost abstract_program.transition_matrices !policy abstract_program.instantiated_costs1 in
	
	print_message ("Sigma costs1 := " ^ (string_float_of_num cost1)) mode_STANDARD;
	
	(* Compute global cost2 *)
	let cost2 = compute_global_cost abstract_program.transition_matrices !policy abstract_program.instantiated_costs2 in
	
	print_message ("Sigma costs2 := " ^ (string_float_of_num cost2)) mode_STANDARD;
	
	(* BLUBLU : a changer !! *)
	let max_value = (Num.num_of_int 4) // (Num.num_of_int 1) in
	let interval = (Num.num_of_int 1) in
	
	(* Compare cost2 with max value *)
	if cost2 >/ max_value +/ interval then(
		(* Optimize w *)
		w_max := !w;
		w := (!w +/ !w_min) // (Num.num_of_int 2);
	) else if cost2 </ max_value -/ interval then(
		(* Optimize w *)
		w_min := !w;
		w := (!w +/ !w_max) // (Num.num_of_int 2);
	) else(
		print_message ("w is now optimal as " ^ (Num.string_of_num cost2) ^ " ~ " ^ (Num.string_of_num max_value)) mode_STANDARD;
		terminate_program();
	);
	
	print_message ("w := " ^ (string_float_of_num !w) ^ "") mode_STANDARD;
	
	(* BLUBLU : on ne va pas trop loin... *)
	if !nb_iterations > 3 then close_enough := true;
	
	(* Increment iterations *)
	nb_iterations := !nb_iterations + 1;

done;

(*print_message "---- Final utility ----" mode_STANDARD;
print_utility mode_STANDARD abstract_program.states u;*)
print_message "---- Final policy ----" mode_STANDARD;
print_policy mode_STANDARD abstract_program.states abstract_program.labels !policy;



print_message ("\n*********************") mode_STANDARD;
print_message ("Algorithm IMPERATOR II successfully ended after " ^ (string_of_int !nb_iterations) ^ " iterations") mode_STANDARD;
print_message ("*********************") mode_STANDARD;



(*



(****************************************************************)
(** Algorithm IMPERATOR III : ' stochastic policy ' *)
(****************************************************************)

print_message ("\n*********************") mode_STANDARD;
print_message "ALGORITHM IMPERATOR III 'stochastic policy' STARTED" mode_STANDARD;
print_message ("*********************") mode_STANDARD;

(* Some useful information *)
let nb_parameters = Array.length abstract_program.cost_parameters in
let nb_labels = Array.length abstract_program.labels in



(* Probabilistic policy : array[state_index][label_index] -> proba *)
let pp = Array.make_matrix abstract_program.nb_states (Array.length abstract_program.labels) (Num.num_of_int 0) in
pp.(0).(0) <- (Num.div_num (Num.num_of_int 774) (Num.num_of_int 1000));
pp.(0).(1) <- (Num.div_num (Num.num_of_int 226) (Num.num_of_int 1000));
pp.(1).(0) <- (Num.num_of_int 1);
pp.(1).(1) <- (Num.num_of_int 0);
pp.(2).(0) <- (Num.num_of_int 1);
pp.(2).(1) <- (Num.num_of_int 0);
pp.(3).(0) <- (Num.num_of_int 1);
pp.(3).(1) <- (Num.num_of_int 0);
pp.(4).(0) <- (Num.num_of_int 0);
pp.(4).(1) <- (Num.num_of_int 1);
pp.(5).(0) <- (Num.div_num (Num.num_of_int 395) (Num.num_of_int 1000));
pp.(5).(1) <- (Num.div_num (Num.num_of_int 605) (Num.num_of_int 1000));
pp.(6).(0) <- (Num.num_of_int 0);
pp.(6).(1) <- (Num.num_of_int 1);
pp.(7).(0) <- (Num.num_of_int 0);
pp.(7).(1) <- (Num.num_of_int 1);
(* Print stochastic policy *)
print_message ("\nStochastic Policy") mode_HIGH_DEBUG;
for state_index = 0 to abstract_program.nb_states - 1 do
	for label_index = 0 to (Array.length abstract_program.labels) - 1 do
		print_message (
			"  P[" ^ abstract_program.states.(state_index) ^ "]["
			^ abstract_program.labels.(label_index) ^ "] = "
			^ (string_float_of_num pp.(state_index).(label_index))
		) mode_HIGH_DEBUG;
	done;
done;

(* Create prob[state][label][state'] -> proba *)
let prob = Array.make_matrix abstract_program.nb_states (Array.length abstract_program.labels) (Array.make abstract_program.nb_states (Num.num_of_int 0)) in
(* Fill it from transition_matrices *)
for state_index = 0 to abstract_program.nb_states - 1 do
	for label_index = 0 to (Array.length abstract_program.labels) - 1 do
		for state_dest_index = 0 to abstract_program.nb_states - 1 do
			prob.(state_index).(label_index).(state_dest_index) <- abstract_program.transition_matrices.(label_index).(state_index).(state_dest_index);
		done;
	done;
done;


(* Parameterized 'U' array *)
let u_param =
	(* Numbers *)
	let nb_states = abstract_program.nb_states in
	let nb_parameters = Array.length abstract_program.cost_parameters in
	(* Create the M matrix *)
	let m = Matrix.make_matrix nb_states in
	for state_index = 0 to abstract_program.nb_states - 1 do
		for label_index = 0 to nb_labels - 1 do
			for state_dest_index = 0 to abstract_program.nb_states - 1 do
				(* Proba of leaving state_index through label_index to state_dest_index *)
				m.(state_index).(state_dest_index) <- m.(state_index).(state_dest_index) +/
					pp.(state_index).(label_index) */ prob.(state_index).(label_index).(state_dest_index);
			done;
		done;
	done;
	
	(* Compute (I - M)^{-1} *)
	let iN = Matrix.i_n nb_states in
	let iN_minus_tm = Matrix.add iN (Matrix.opposite m) in
	let invert = Matrix.invert iN_minus_tm in
	
	(* Compute the array of costs w.r.t. the policy *)
	let the_costs = make_current_parametric_cost abstract_program.parametric_costs1 policy in
	if nb_states <> Array.length the_costs
		then raise (InternalError "The TM and the array_of_costs have a different size in function parametric_value_determination.");
	
	(* Compute (I - TM)^{-1} * current_parametric_costs *)
	(* Create a fresh new vector of members *)
	let vector_of_members = Array.make nb_states (Array.make nb_parameters (Num.num_of_int 0), Num.num_of_int 0) in
	(* Fill it *)
	for state = 0 to nb_states - 1 do
		vector_of_members.(state) <- make_parametric_row invert.(state) the_costs nb_parameters;
	done;
	(* Return the vector of members *)
	vector_of_members
in



(* Print U_param ! *)
print_message "Parametric U for optimal policy:" mode_HIGH_DEBUG;
for i = 0 to abstract_program.nb_states - 1 do
	print_message (
		"State " ^ abstract_program.states.(i) ^ " : "
		^ (Constraint.string_of_exact_member abstract_program.cost_parameters u_param.(i))
		^ " ( = " ^ (string_float_of_num (Constraint.instantiate_member u_param.(i) abstract_program.cost_values) ^ ")")
	) mode_HIGH_DEBUG;
done;

(* Our result ! *)
let my_constraint = ref [] in

(* Iterate on states *)
for state_index = 0 to abstract_program.nb_states - 1 do
	print_message ("Considering state " ^ abstract_program.states.(state_index)) mode_STANDARD;
	
	(* Perform \Sigma_{j}M_{i, j}^{P[state]}U[j] *)
	let ref_member =
		u_param.(state_index)
	in
	(* Instantiate it *)
	let inst_ref_member = Constraint.instantiate_member ref_member abstract_program.cost_values in
	(* Print ref_member for debug *)
	print_message ("U[" ^ abstract_program.states.(state_index) ^ "] = "
		^ (Constraint.string_of_exact_member abstract_program.cost_parameters ref_member)
		^ " ( = " ^ (string_float_of_num inst_ref_member) ^ ")")
	mode_HIGH_DEBUG;
	
	(* Iterate on the labels of the state ONLY *)
	List.iter (fun label_index ->
		(* Do not consider label_index = P[state_index] *)
		if label_index <> policy.(state_index) then(
		
		(* Debug message *)
		print_message ("Considering action " ^ abstract_program.labels.(label_index)) mode_LOW_DEBUG;
		
		(* Perform \Sigma_{j}M_{i, j}^{label_index}U[j] + Costs_{label_index} ( state_index ) *)
		let current_member =
			Constraint.add_members
			(* Sigma_{j}M_{i, j}^{label_index}U[j] *)
			(make_product_proba_array_member_array
				abstract_program.transition_matrices.(label_index).(state_index)
				u_param
				nb_parameters
			)
			(* Costs_{label_index} ( state_index ) *)
			(member_of_cost
				nb_parameters
				abstract_program.parametric_costs1.(state_index).(label_index)
			)
		in
		(* Print current_member for debug *)
		print_message ("Sigma_{j}M_{" ^ abstract_program.states.(state_index) ^ ", j}^{"
			^ abstract_program.labels.(label_index) ^ "}U[j]"
			^ " + Costs_{" ^ abstract_program.labels.(label_index) ^ "}("
			^ abstract_program.states.(state_index) ^ ") = "
			^ (Constraint.string_of_exact_member abstract_program.cost_parameters current_member)
			^ " ( = " ^ (string_float_of_num inst_ref_member) ^ ")")
		mode_HIGH_DEBUG;
		
		(* Instantiate the second member *)
		let inst_current_member = Constraint.instantiate_member current_member abstract_program.cost_values in
		(* Make comparison : case '<' *)
		if inst_ref_member </ inst_current_member then
		(* Add inequality to the constraint *)
		let new_inequality = Constraint.make_inequality ref_member Constraint.Op_l current_member in
		print_message ("Adding constraint : "
			^ (Constraint.string_of_exact_inequality abstract_program.cost_parameters new_inequality)) mode_STANDARD;
		my_constraint := Constraint.add_inequality new_inequality !my_constraint;
		(* Make comparison : case '=' *)
		else if inst_ref_member =/ inst_current_member then
		(* Add inequality to the constraint *)
		let new_inequality = Constraint.make_inequality ref_member Constraint.Op_le current_member in
		print_message ("Adding constraint : "
			^ (Constraint.string_of_exact_inequality abstract_program.cost_parameters new_inequality)) mode_LOW_DEBUG;
		my_constraint := Constraint.add_inequality new_inequality !my_constraint;
		(* The policy is not optimal *)
		else raise (InternalError ("The policy is not optimal, as " ^ (string_float_of_num inst_ref_member) ^ " > " ^ (string_float_of_num inst_current_member)))
		)
	) abstract_program.labels_per_state.(state_index);

done;
*)


*)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Goodbye, world, goodbye! *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
terminate_program ()

