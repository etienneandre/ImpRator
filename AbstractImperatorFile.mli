(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       03/03/2009
 * Last modified: 06/04/2009
 *
 ****************************************************************)



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
(** Creation function *)
(****************************************************************)
(* Convert the result of the parsing into a valid program ; raise VerificationFailure if invalid program*)
val make_AbstractImperatorFile : parsing_structure * costs_definition * Num.num -> abstract_program


(****************************************************************)
(** Get functions *)
(****************************************************************)

(* Get the instantiated cost of leaving a state_index firing a label_index *)
(*val get_instantiated_cost1 : state_index -> label_index -> abstract_program -> Num.num
*)
(* Compute the sum of probabilities from state_index via label_index *)
val sum_of_probabilities : abstract_program -> state_index -> label_index -> probability


