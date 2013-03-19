/***********************************************
   Laboratoire Specification et Verification

   Etienne ANDRE

   Created       : 02/03/2009
   Last modified : 03/04/2009
***********************************************/

%{

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractImperatorFile



(****************************************************************)
(** Functions *)
(****************************************************************)

(* Convert a float to a Num.num *)
let num_of_float f =
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = int_of_float integer in
	(* Case of an integer *)
	if fractional = 0.0 then Num.num_of_int integer
	else(
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = int_of_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = int_of_string fractional in
		(* Create the fraction *)
		Num.div_num (Num.num_of_int (integer * denominator + fractional)) (Num.num_of_int (denominator))
	)

%}

%token <int> INT
%token <float> FLOAT
%token <string> NAME

/*%token OP_PLUS
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN*/

%token OP_EQ
%token OP_MULT OP_DIV OP_PLUS OP_MINUS

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
/*%token APOSTROPHE COLON COMMA PIPE SEMICOLON*/
%token AMPERSAND ARROW COLON COMMA DOT SEMICOLON

%token CT_ABSORBING CT_BETA CT_COSTS CT_STATE CT_STRUCTURE

%token EOF

%start program             /* the entry point */
%type <AbstractImperatorFile.abstract_program> program
%%



/**********************************************/
program:
	 structure costs beta EOF
	{
		make_AbstractImperatorFile ($1, $2, $3)
	}
;

/***********************************************
  STRUCTURE
***********************************************/

/**********************************************/

structure:
	CT_STRUCTURE state_list {$2}
;

/**********************************************/

state_list:
	state state_list { $1 :: $2 }
	| { [] }
;

/**********************************************/

state:
	/* at least one label per state */
	CT_STATE NAME COLON label_and_transitions labels {
		($2, ($4 :: $5))
	}
;


/**********************************************/

labels:
	| label_and_transitions labels { $1 :: $2 }
	| { [] }
;

/**********************************************/

label_and_transitions:
	/* at least one transition per label and state */
	NAME cost1 cost2 transition transitions { $1, $2, $3, $4 :: $5 }
;


/**********************************************/

transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/**********************************************/

transition:
	ARROW LPAREN arithm_exp RPAREN destination SEMICOLON { ($3, $5) }
;


/**********************************************/

destination:
	| CT_ABSORBING { AbsorbingState }
	| NAME { NormalState $1 }
;


/**********************************************/
cost1:
	| LSQBRA cost RSQBRA { $2 }
;

/**********************************************/
cost2:
	| LSQBRA cost RSQBRA { $2 }
	| { TmpConstCost (Num.num_of_int 0) }
;

/**********************************************/

cost:
	| arithm_exp { TmpConstCost $1 }
	| NAME { TmpParametricCost $1 }
;

/**********************************************/
arithm_exp:
	add_exp {$1}
;

/**********************************************/
add_exp:
	| add_exp OP_PLUS mult_exp {Num.add_num $1 $3}
	| add_exp OP_MINUS mult_exp {Num.sub_num $1 $3}
	| mult_exp {$1}
;

/**********************************************/
mult_exp:
	| mult_exp OP_MULT number {Num.mult_num $1 $3}
	| mult_exp OP_DIV number {Num.div_num $1 $3}
	| number {$1}
;

/**********************************************/

number:
	| pos_number {$1}
	| OP_MINUS pos_number {Num.minus_num $2}
;

/**********************************************/

pos_number:
	| INT { Num.num_of_int $1 }
	
	| FLOAT { (*Num.num_of_string (string_of_float $1) *) (num_of_float $1)}
	
/*	| CT_INFINITY { Num.num_of_string "1/0" (*Num.div_num (Num.num_of_int 1) (Num.num_of_int 0) *)}*/
;


/***********************************************
  COSTS
***********************************************/
costs:
	CT_COSTS cost_list { $2 }
;

/**********************************************/
cost_list:
	| and_opt cost_def cost_list2 {$2 :: $3}
	| { [] }
;

/**********************************************/
cost_list2:
	| AMPERSAND cost_def cost_list2 {$2 :: $3}
	| { [] }
;

/**********************************************/
and_opt:
	| AMPERSAND {}
	| {}
;

/**********************************************/
cost_def:
	NAME OP_EQ arithm_exp { ($1, $3) }
;


/**********************************************/
beta:
	| CT_BETA OP_EQ arithm_exp { $3 }
	| { Num.num_of_int 0 }
;
