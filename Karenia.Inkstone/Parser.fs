namespace rec Karenia.Inkstone.Parser

open FParsec

type Literal =
    | Number of NumberLiteral
    | String of string
    | Symbol of string
    | Boolean of bool
    | Nil

type BinaryOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | BitAnd
    | BitOr
    | BitXor
    | BoolAnd
    | BoolOr

type UnaryOp =
    | Not
    | Neg

type BinaryExpr = { lhs: Expr; rhs: Expr; op: BinaryOp }

type UnaryExpr = { lhs: Expr; op: UnaryOp }

type IfExpr =
    {
        cond: Expr
        true_body: Expr
        false_body: Expr Option
    }

type WhileExpr = { cond: Expr; body: Expr }

type Expr =
    | BinaryExpr of BinaryExpr
    | UnaryExpr of UnaryExpr
    | LiteralExpr of Literal
    | IdentExpr of string
    | IfExpr of IfExpr
    | ReturnExpr of Expr Option
    | BreakExpr of Expr Option
    | ContinueExpr

type AstNode<'u> =
    {
        node: 'u
        offset: int64
        length: int64
    }

module Parser =
    let lf<'u> = newline
    let sp<'u> = anyOf " \t"
    let lfi<'u> in_p = if in_p then lf else sp

    let numLit<'u> =
        numberLiteral
            (NumberLiteralOptions.DefaultInteger
             ||| NumberLiteralOptions.DefaultFloat)
            "number literal"
        |>> Number

    let nilLit<'u> =
        pstring "nil" <?> "nil literal" |>> fun _ -> Nil

    let boolLit<'u> =
        (pstring "true" |>> fun _ -> Boolean true)
        <|> (pstring "false" |>> fun _ -> Boolean false)

    let lit<'u> = numLit <|> nilLit <|> boolLit

    let expr<'u> = lit
