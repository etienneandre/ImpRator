(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       06/03/2009
 * Last modified: 06/03/2009
 *
 ****************************************************************)

type t = string

let compare x y = if x = y then 0 else if x < y then -1 else 1
