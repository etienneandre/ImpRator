(***********************************************
   Laboratoire Specification et Verification

   Etienne ANDRE

   Created       : 02/03/2009
   Last modified : 27/03/2009
***********************************************)

{
open ImperatorParser        (* The type token is defined in ImperatorParser.mli *)
}
rule token = parse
	[' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
(*	| "(*" ([^'\n']|['\n'])* "*)"     { token lexbuf }     (* skip comments *)*)
	| "(*"([^'*']|'*'+[^'*'')'])*'*'+')' { token lexbuf }
	
(*	"(*"([^'*']|['\r''\n']|('*'+([^"*)"]|['\r''\n'])))*"*   )"+ { token lexbuf }     (* skip comments *)*)
(*	| "infinity"      { CT_INFINITY }*)

	| "absorbing"      { CT_ABSORBING }
	| "beta"           { CT_BETA }
	| "costs"          { CT_COSTS }
	| "state"          { CT_STATE }
	| "structure"      { CT_STRUCTURE }

	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
	| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
(*	| '"' [^'"']* '"' as lxm { STRING lxm } (* a string between double quotes *)*)

(*	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '>'              { OP_G }
	| ":="             { OP_ASSIGN }*)

	| '='              { OP_EQ }
	
	| '*'              { OP_MULT }
	| '/'              { OP_DIV }
	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	

	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

(*	| '\''             { APOSTROPHE }
	| '|'              { PIPE }*)
	
	| '&'              { AMPERSAND }
	| "->"             { ARROW }
	| ':'              { COLON }
	| ','              { COMMA }
	| '.'              { DOT }
	| ';'              { SEMICOLON }

	| eof              { EOF}


