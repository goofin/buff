%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACKET RIGHT_BRACKET
%token TYPE PACKAGE IMPORT
%token ENUM STRUCT
%token DOT SEMICOLON
%token <string> INTEGER IDENT STRING
%token EOF

%start <Ast.statement list> main

%%

main: statements = list(statement) EOF { statements };

statement:
    | PACKAGE name = IDENT SEMICOLON { Ast.Package name }
    | IMPORT name = option(IDENT) path = STRING SEMICOLON { Ast.Import (name, path) }
    | TYPE name = IDENT lit = type_literal SEMICOLON { Ast.Type (name, lit) }
    ;

type_literal:
    | STRUCT LEFT_BRACE entries = list(struct_entry) RIGHT_BRACE { Ast.Struct entries }
    | ENUM LEFT_BRACE entries = list(enum_entry) RIGHT_BRACE { Ast.Enum entries }
    | LEFT_BRACKET RIGHT_BRACKET sel = type_ref { Ast.Slice sel }
    | LEFT_BRACKET n = INTEGER RIGHT_BRACKET sel = type_ref { Ast.Array (n, sel) }
    ;

type_ref:
    | lit = type_literal { Ast.Literal lit }
    | ident = IDENT rem = option(type_sel) { Ast.Selector (ident, rem) }
    ;

type_sel: DOT ref = type_ref { ref }

struct_entry:
    | TYPE name = IDENT lit = type_literal SEMICOLON { Ast.StructNested (name, lit) }
    | name = IDENT ref = type_ref SEMICOLON { Ast.StructField (name, ref) }
    ;

enum_entry:
    | TYPE name = IDENT lit = type_literal SEMICOLON { Ast.EnumNested (name, lit) }
    | name = IDENT ref = option(type_ref) SEMICOLON { Ast.EnumCase (name, ref) }
    ;
