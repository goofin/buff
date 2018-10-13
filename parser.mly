%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACKET RIGHT_BRACKET
%token TYPE ENUM STRUCT
%token DOT STAR SEMICOLON
%token <string> INTEGER IDENT
%token EOF

%start <(string * Ast.type_) list> main

%%

main: types = list(type_) EOF { types };

type_: TYPE name = IDENT lit = type_literal SEMICOLON? { (name, lit) };

type_literal:
    | STRUCT LEFT_BRACE entries = separated_list(SEMICOLON, struct_entry) RIGHT_BRACE { Ast.Struct entries }
    | ENUM LEFT_BRACE entries = separated_list(SEMICOLON, enum_entry) RIGHT_BRACE { Ast.Enum entries }
    | LEFT_BRACKET RIGHT_BRACKET sel = type_ref { Ast.Slice sel }
    | LEFT_BRACKET n = INTEGER RIGHT_BRACKET sel = type_ref { Ast.Array (n, sel) }
    | STAR sel = type_ref { Ast.Pointer sel }
    ;

type_ref:
    | lit = type_literal { Ast.Literal lit }
    | name = IDENT sel = type_sel { Ast.Selector (name, sel) }
    ;

type_sel:
    | DOT name = IDENT sel = type_sel { name :: sel }
    | { [] }
    ;

struct_entry:
    | TYPE name = IDENT lit = type_literal { Ast.StructNested (name, lit) }
    | name = IDENT ref = type_ref { Ast.StructField (name, ref) }
    ;

enum_entry:
    | TYPE name = IDENT lit = type_literal { Ast.EnumNested (name, lit) }
    | name = IDENT ref = option(type_ref) { Ast.EnumCase (name, ref) }
    ;
