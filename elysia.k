module ELYSIA-SYNTAX
  imports STRING-SYNTAX
  imports INT-SYNTAX

  syntax #Layout ::= r"([\\ \\r\\t])" [token]

  syntax Expr ::=
      String
    | Null
    | InternalPrint

  syntax InternalPrint ::= "print" "(" Expr ")" [strict]
  syntax Null ::= "null" [token]

  syntax Newline ::= "\n" [token]
  syntax Exprs ::=
      Expr Newline Exprs [right]
    | Expr

endmodule

module ELYSIA
  imports ELYSIA-SYNTAX
  imports INT
  imports STRING
  imports BOOL
  imports LIST

  configuration <T>
    <k> $PGM:Exprs </k>
    <out stream="stdout"> .List </out>
  </T>

  syntax KResult ::= Value
  syntax Value ::= String | Null

  syntax Expr ::= ToString(Expr) [function]
  rule ToString(null:Null) => "null"
  rule ToString(Str:String) => Str

  syntax K ::= Drop(Expr) [function, strict]
  rule Drop(_) => .K

  rule E1:Expr _:Newline E2:Exprs => Drop(E1) ~> E2

  rule <k> print(Str:String) => null ... </k>
       <out> OUT => OUT ListItem(Str) ListItem("\n") </out>

  rule <k> print(E:Expr) => print(ToString(E)) ... </k> [owise]

endmodule