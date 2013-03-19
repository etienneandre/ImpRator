(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       17/03/2009
 * Last modified: 09/04/2009
 *
 ****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception SingularMatrix
exception DifferentSize of string


(****************************************************************)
(** Types *)
(****************************************************************)
type vector = Num.num array

type matrix = Num.num array array


(****************************************************************)
(** Creation of matrix *)
(****************************************************************)

(* Return a fresh vector n initialized to Num.num_of_int 0 *)
val make_vector : int -> vector

(* Return a fresh new matrix n * n instantiated to 0 *)
val make_matrix : int -> matrix

(* Return a fresh new identity matrix "I" *)
val i_n : int -> matrix


(****************************************************************)
(** Operation on a single matrix *)
(****************************************************************)

(* Return a fresh new matrix M' equal to the opposite of M, i.e., M'[i][j] = -M[i][j] *)
val opposite : matrix -> matrix


(****************************************************************)
(** Inversion of matrix *)
(****************************************************************)

(* Invert a matrix and return a fresh new matrix, or raise SingularMatrix if it is singular *)
val invert : matrix -> matrix


(****************************************************************)
(** Multiplications *)
(****************************************************************)

(* Return V1 * V2 *)
val mult_vector_vector : vector -> vector -> Num.num

(* Return a fresh matrix as the result of M * c *)
val mult_matrix_scalar : matrix -> Num.num -> matrix

(* Return a fresh vector as the result of M * V *)
val mult_matrix_vector : matrix -> vector -> vector

(* Return a fresh matrix as the result of M1 * M2 *)
val mult_matrices : matrix -> matrix -> matrix


(*
(* Return a list of members of inequality as the result of M * V where V is a vector of parameters (integers) *)
val mult_matrix_parameters : matrix -> (int array) -> Constraint.member list
*)

(****************************************************************)
(** Operations between 2 matrices *)
(****************************************************************)

(* Return a fresh new matrix as the result of A + B *)
val add : matrix -> matrix -> matrix


(****************************************************************)
(** Conversion to string *)
(****************************************************************)

(* Convert a vector to a string *)
val string_of_vector : vector -> string

(* Convert a matrix to a string *)
val string_of_matrix : matrix -> string
