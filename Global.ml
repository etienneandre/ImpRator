(*****************************************************************
 *
 *                     IMPERATOR
 * 
 * Compiler from IMPERATOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       04/03/2009
 * Last modified: 27/03/2009
 *
 ****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception ParsingError of string
exception InternalError of string

(****************************************************************)
(** Debug modes *)
(****************************************************************)
let mode_ERROR		= 0
let mode_NODEBUG	= 0
let mode_STANDARD	= 1
let mode_LOW_DEBUG	= 2
let mode_MEDIUM_DEBUG	= 3
let mode_HIGH_DEBUG	= 4
let mode_TOTAL_DEBUG	= 5


(****************************************************************)
(** Global constants *)
(****************************************************************)
let default_beta = Num.div_num (Num.num_of_int 99) (Num.num_of_int 100)


(****************************************************************)
(** Global time counter *)
(****************************************************************)
let counter = ref (Unix.gettimeofday())

(** Get the value of the counter
    @return float the value of counter with 3 digits after "." *)
let get_time() =
  ((float_of_int) (int_of_float ((Unix.gettimeofday() -. (!counter)) *. 1000.0))) /. 1000.0


(****************************************************************)
(** Useful functions *)
(****************************************************************)
(* Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""


(****************************************************************)
(** Messages *)
(****************************************************************)
(* Print a message in function of debug_level *)
let print_debug_message debug_mode debug_level message =
	(* Only print the message if its debug_level is compatible with the debug_mode *)
	if debug_mode >= debug_level then
		(* Find number of blanks for indentation *)
		let nb_spaces = if debug_level-1 > 0 then debug_level-1 else 0 in
		(* Create blanks proportionnally to the debug_level (at least one space) *)
		let spaces = " " ^ string_of_array_of_string (Array.make nb_spaces "   ") in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
		(* Print message *)
		print_string (formatted_message ^ "\n")

(* Print a warning *)
let print_warning message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** Warning: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print message *)
	print_string (formatted_message ^ "\n")



(****************************************************************)
(** Terminating functions *)
(****************************************************************)
(* Abort program *)
let abort_program () =
	print_debug_message mode_TOTAL_DEBUG mode_ERROR ("Program aborted (after " ^ (string_of_float (get_time())) ^ " seconds)");
	print_newline();
	flush stdout;
	exit(0)

(* Terminate program *)
let terminate_program () =
	print_newline();
	print_debug_message mode_TOTAL_DEBUG mode_STANDARD  ("Program successfully terminated (after " ^ (string_of_float (get_time())) ^ " seconds)");
	print_newline();
	flush stdout;
	exit(0)
	
	



(*
let OrderedString =
   struct
     type t = string
     let compare x y = if x = y then Equal else if x < y then Less else Greater
   end
   

let StringSet = Set(OrderedString)


*)