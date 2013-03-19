(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       04/03/2009
 * Last modified: 05/03/2009
 *
 ****************************************************************)


(****************************************************************)
(** Fraction *)
(****************************************************************)

type fraction = (int * int) (* Numerator / Denominator *)


(****************************************************************)
(** Internal functions used in this file *)
(****************************************************************)
(* Compute the GCD of two integers *)
let rec gcd a b =
    if a = 0 then b
             else gcd (b mod a) a


(** Compute the LCM of two integers  -  functions found on the web *)

let rec decompose_aux u q x =
  let v = u / q in
  if v < q then 
    [ u ]
  else if u mod q = 0 then
    q::decompose_aux v q x
  else
    decompose_aux u (q + x)
      (if q < 5 then 2 else x mod 4 + 2)

let decompose u =
  if u < 1 then 
    failwith "decompose"
  else if u = 1 then
    []
  else 
    decompose_aux u 2 1

let rec ppcmf xs ys = match xs,ys with
| [],_ -> ys
| _,[] -> xs
| x::rx, y::ry ->
    if x < y then
      x::ppcmf rx ys
    else if y < x then
      y::ppcmf xs ry
    else (* x=y *)
      x::ppcmf rx ry

let rec produit xs = match xs with
| [] -> 1
| x::xs -> x*produit xs

let lcm u v =
  let us = decompose u and vs = decompose v in
  produit (ppcmf us vs)


(* Zero *)
let zero = (0, 1)


(****************************************************************)
(** Functions on fractions *)
(****************************************************************)
(* Reducts a fraction so that it is irreductible *)
let reduct (num, den) =
	let gcd = gcd num den in
	(num / gcd, den / gcd)


(* Add two fractions and return an irreductible fraction *)
let add (n1, d1) (n2, d2) =
  (* Compute LCM of d1 and d2 *)
  let lcm = lcm d1 d2 in
  let factor1 = lcm / d1 in
  let factor2 = lcm / d2 in
  (* Reducts the result *)
  reduct (n1 * factor1 + n2 * factor2, lcm)


(* Check if a fraction is equal to 1, i.e., if num = den *)
let equal_to_1 (num, den) =
	(num = den)

(* Convert a int to a fraction *)
let fraction_of_int i = (i, 1)

(* Convert a float to a fraction *)
let fraction_of_float f =
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = int_of_float integer in
	(* Case of an integer *)
	if fractional = 0.0 then (integer, 1)
	else(
(*		print_string(string_of_float fractional);
		print_newline();
		print_string(string_of_int integer); 
		print_newline();*)
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = int_of_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = int_of_string fractional in
		(* Create the fraction *)
		let f = (integer * denominator + fractional, denominator)
		in reduct f
	)


(* Convert a fraction to a string "num / den" *)
let string_of_fraction = function
	| num, 1 -> string_of_int num
	| 0, den -> "0"
	| num, den ->
		if num = den then "1"
		else (string_of_int num) ^ "/" ^ (string_of_int den)

