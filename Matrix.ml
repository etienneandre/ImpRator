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
 * Created:       17/03/2009
 * Last modified: 10/04/2009
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
let make_vector n =
	Array.make n (Num.num_of_int 0)

(* Return a fresh matrix n * n initialized to Num.num_of_int 0 *)
let make_matrix n =
	Array.make_matrix n n (Num.num_of_int 0)

(* Return a fresh new identity matrix "I" *)
let i_n n =
	(* Set 0 everywhere... *)
	let matrixI = make_matrix n in
	(* ... except on the diagonal *)
	for p = 0 to n-1 do
		matrixI.(p).(p) <- (Num.num_of_int 1)
	done;
	(* Return I *)
	matrixI


(****************************************************************)
(** Conversion to string *)
(****************************************************************)

(* Convert a vector to a simple string *)
let string_of_vector =
	Array.fold_left
		(fun s elem -> s ^ "\n| " ^ (string_of_num elem) ^ " |")
		"\n--Vector--"

(* Convert a matrix to a simple string *)
let string_of_matrix matrixM =
	Array.fold_left (fun s row ->
		s ^ (Array.fold_left
			(fun s elem -> s ^ "| " ^ (string_of_num elem) ^ " |")
			""
			row) ^ "\n--------------------------\n"
		) "\n---------Matrix-----------\n" matrixM


(****************************************************************)
(** Operation on a single matrix *)
(****************************************************************)

(* Size of a matrix or a vector *)
let size = Array.length

(* Return a fresh new matrix M' equal to the opposite of M, i.e., M'[i][j] = -M[i][j] *)
let opposite matrixM =
	let n = size matrixM in
	(* Create a fresh new matrix *)
	let matrixMprime = make_matrix n in
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			matrixMprime.(i).(j) <- (num_of_int 0) -/ (matrixM.(i).(j))
		done;
	done;
	(* Return M' *)
	matrixMprime


(****************************************************************)
(** Inversion of matrix *)
(****************************************************************)

(** Code Java trouve le 16/03/2009 sur ' http://fr.wikipedia.org/wiki/Matrice_inversible ', et transcrit en caml *)


(* To exchange two rows in a matrix *)
let exchange_row matrixM k l m n =
	if k <= 0 || l <= 0 || k > n || l > n || k = l
	then ()
	else(
		(* for (int j=0; j<n; j++) *)
		for j = 0 to n-1 do
			let tmp = matrixM.(k-1).(j) in
				matrixM.(k-1).(j) <- matrixM.(l-1).(j);
				matrixM.(l-1).(j) <- tmp;
			;
		done;
	)


(* Invert a matrix *)
let invert matrixM =
	(* Compute the size of matrixM *)
	let n = size matrixM in
	
	(* Pour stocker les lignes pour lesquels un pivot a deja ete trouve *)
	let vectI = ref [] in
	
	(* Pour stocker les colonnes pour lesquels un pivot a deja ete trouve *)
	let vectJ = ref [] in
	
	(* Pour calculer l'inverse de la matrice initiale *)
	let matrixA = make_matrix n in
	
	(* Our result *)
	let matrixB = make_matrix n in
	
	(* Copie de M dans A et Mise en forme de B : B=I *)
	for i = 0 to n - 1 do (
		for j = 0 to n - 1 do
			matrixA.(i).(j) <- matrixM.(i).(j);
			if i = j then matrixB.(i).(j) <- (num_of_int 1) else matrixB.(i).(j) <- (num_of_int 0);
		done)
	done;
	
	(* Parametres permettant l'arret premature des boucles ci-dessous si calcul impossible *)
	let bk = ref true in
	let bl = ref true in
 
	(* Parametres de controle pour la recherche de pivot *)
	let cnt_row = ref 0 in
	let cnt_col = ref 0 in
	
	(* parametres de stockage de coefficients *)
	let a = ref (num_of_int 0) in
	let tmp = ref (num_of_int 0) in
	
	(* for (int k=0; k<n && bk; k++) *)
	let k = ref 0 in
	while (!k < n && !bk) do
		if not (List.mem !k !vectI) then(
			vectI := (!k :: !vectI);
			cnt_row := !cnt_row + 1;
			bl := true;
			(* for (int l=0; l<n && bl; l++)  *)
			let l = ref 0 in
			while (!l < n && !bl) do
				if not (List.mem !l !vectJ) then(
					a := matrixA.(!k).(!l);
					if (!a <>/ (num_of_int 0)) then(
						vectJ := (!l :: !vectJ);
						cnt_col := !cnt_col + 1;
						bl := false; (* permet de sortir de la boucle car le pivot a ete trouve *)
						(* for (int p=0; p<n; p++) *)
						for p = 0 to n-1 do
							if p <> !k then (
								tmp := matrixA.(p).(!l);
								(* for (int q=0; q<n; q++) *)
								for q = 0 to n-1 do
									(* A[p][q] = A[p][q] - A[k][q]*(tmp/a); *)
									matrixA.(p).(q) <- matrixA.(p).(q) -/ matrixA.(!k).(q) */ (!tmp // !a);
									(* B[p][q] = B[p][q] - B[k][q]*(tmp/a); *)
									matrixB.(p).(q) <- matrixB.(p).(q) -/ matrixB.(!k).(q) */ (!tmp // !a);
								done;
							);
						done;
					)
				);
				l := !l+1;
			done;
			
			if !cnt_row <> !cnt_col then (
				(*//Matrix is singular 
				  //Pas de pivot possible, donc pas d'inverse possible! On sort de la boucle *)
				bk := false;
				k := n;
			);

			
		);
		k := !k + 1;
	done;
	
	if not !bk then(
		(* Le pivot n'a pas pu être trouve précédemment, ce qui a donne bk = false *)
		
		print_debug_message mode_STANDARD mode_ERROR (string_of_matrix matrixM);
		
		raise SingularMatrix;

	)else(
		(* Réorganisation des colonnes de sorte que A=I et B=Inv(M). Méthode de Gauss-Jordan *)
		for l = 0 to n-1 do
			(* for k = 0 to n-1 do *)
			let k = ref 0 in
			while !k < n do
				a := matrixA.(!k).(l);
				if !a <>/ (num_of_int 0) then(
					matrixA.(!k).(l) <- (num_of_int 1);
					for p = 0 to n-1 do
						(* B[k][p] = B[k][p]/a *)
						matrixB.(!k).(p) <- matrixB.(!k).(p) // !a;
					done;
					if !k <> l then(
						(* exchange_row(A,k+1,l+1,n,n) *)
						exchange_row matrixA (!k+1) (l+1) n n;
						(* exchange_row(B,k+1,l+1,n,n) *)
						exchange_row matrixB (!k+1) (l+1) n n;
					);
					(* k = n; //Pour sortir de la boucle car le coefficient non nul a ete trouve *)
					k := n;
				);
				k := !k+1;
			done;

		done;
		(* return true *)
	);
	(* Return the matrix *)
	matrixB


(****************************************************************)
(** Multiplications *)
(****************************************************************)

(* Return v1 * v2 *)
let mult_vector_vector v1 v2 =
	let n = size v1 in
	if n <> size v2 then raise (DifferentSize "Two vectors must have the same size to be multiplied.");
	let prod = ref (Num.num_of_int 0) in
	for i = 0 to n-1 do
		prod := !prod +/ v1.(i) */ v2.(i);
	done;
	(* Return the product *)
	!prod


(* Return a fresh matrix as the result of M * c *)
let mult_matrix_scalar m c =
	let n = size m in
	(* Create a fresh new matrix *)
	let prod = make_matrix n in
	(* Fill it *)
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			prod.(i).(j) <- m.(i).(j) */ c;
		done;
	done;
	(* Return the product vector *)
	prod


(* Return a fresh vector as the result of M * V *)
let mult_matrix_vector m v =
	let n = size m in
	if n <> size v then raise (DifferentSize "A matrix and a vector must have the same size to be multiplied.");
	(* Create a fresh new vector *)
	let prod = Array.make n (Num.num_of_int 0) in
	(* Fill it *)
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			prod.(i) <- prod.(i) +/ m.(i).(j) */ v.(j);	
		done;
	done;
	(* Return the product vector *)
	prod


(* Return a fresh matrix as the result of M1 * M2 *)
let mult_matrices m1 m2 =
	let n = size m1 in
	if n <> size m2 then raise (DifferentSize "Two matrices must have the same size to be multiplied.");
	let prod = make_matrix n in
	(* Fill it *)
	for row = 0 to n-1 do
		for column = 0 to n-1 do
			for i = 0 to n-1 do
				prod.(row).(column) <- prod.(row).(column) +/ m1.(row).(i) */ m2.(i).(column);
			done;
		done;
	done;
	(* Return the product matrix *)
	prod


(****************************************************************)
(** Operations between 2 matrices *)
(****************************************************************)

(* Return a fresh new matrix as the result of A + B *)
let add matrixA matrixB =
	let n = size matrixA in
	if n <> size matrixB then raise (DifferentSize "Two matrices must have the same size to be added.");
	(* Create a fresh new matrix *)
	let matrixSum = make_matrix n in
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			matrixSum.(i).(j) <- (matrixA.(i).(j)) +/ (matrixB.(i).(j))
		done;
	done;
	(* Return M' *)
	matrixSum


