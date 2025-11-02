module ELYSIA-SYNTAX
  imports STRING-SYNTAX
  imports INT-SYNTAX
  imports ID-SYNTAX

  syntax #Layout ::= r"([\\ \\r\\t])" [token]

  syntax Expr ::=
    // TODO!: Support structural binding
      "let" Id "=" Expr [strict(2)]
    | "let" "mut" Id "=" Expr [strict(2)]
    | Closure
    | Id "=" Expr [strict(2)]
    | InternalPrint
    | Id
    | IntrinsicValue
    | "(" Expr ")" [bracket]

  syntax IntrinsicValue ::=
      String
    | Int
    | Null

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
  imports SET

  configuration <T>
    <k> $PGM:Exprs </k>
    <context>
      <variable>
        <values> .Map </values>
        <declared> .Set </declared>
        <mutability> .Set </mutability>
      </variable>
    </context>
    <closure-stack> .List </closure-stack>
    <out stream="stdout"> .List </out>
  </T>

  syntax Expr ::= Value
  syntax KResult ::= Value
  syntax Value ::= IntrinsicValue

  syntax Expr ::= ToString(Expr) [function, strict]
  rule ToString(null:Null) => "null"
  rule ToString(S:String) => S
  rule ToString(I:Int) => Int2String(I)

  syntax K ::= Drop(Expr) [function, strict]
  rule Drop(_) => .K

  rule E1:Expr _:Newline E2:Exprs => Drop(E1) ~> E2

  rule <k> let X:Id = V:Value => V ... </k>
       <values> VALUES => VALUES[X <- V] </values>
       <declared> DECLS => DECLS SetItem(X) </declared>
    requires notBool X in DECLS

  rule <k> let mut X:Id = V:Value => V ... </k>
       <values> VALUES => VALUES[X <- V] </values>
       <mutability> MUTS => MUTS SetItem(X) </mutability>
       <declared> DECLS => DECLS SetItem(X) </declared>
    requires notBool X in DECLS

  rule <k> X:Id = V:Value => V ... </k>
       <values> VALUES => VALUES[X <- V] </values>
       <mutability> MUTS </mutability>
       <declared> DECLS </declared>
    requires (X in MUTS) andBool (X in DECLS)

  rule <k> X:Id => V ... </k>
       <values> ... X |-> V </values>

  rule <k> print(Str:String) => null ... </k>
       <out> OUT => OUT ListItem(Str) ListItem("\n") </out>

  rule <k> print(E:Expr) => print(ToString(E)) ... </k> [owise]

endmodule