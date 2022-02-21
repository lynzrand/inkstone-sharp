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
    | ParenExpr of Expr
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

type InternalState =
    {
        parenStack: bool list
    }
    static member Default = { parenStack = list.Empty }

module Parser =
    let lf<'u> = newline >>. preturn ()
    let sp<'u> = many1Chars (anyOf " \t") >>. preturn ()

    let lfi = getUserState

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

    let pSimpleEscape<'u> =
        pchar '\\' >>. anyOf "rntb\\\"'"
        |>> fun ch ->
                match ch with
                | 'r' -> '\r'
                | 'n' -> '\n'
                | 't' -> '\t'
                | 'b' -> '\b'
                | '\\' -> '\\'
                | '\"' -> '\"'
                | '\'' -> '\''
                | _ -> failwith "Unreachable!"

    let stringLitInternal<'u> =
        pchar '"'
        >>. many (pSimpleEscape <|> noneOf "\\\"")
        .>> pchar '"'
        |>> string

    let symbolLit<'u> =
        pchar ':'
        >>. (stringLitInternal
             <|> many1CharsTill (noneOf " \t\n") spaces)
        |>> Symbol

    let lit<'u> =
        numLit <|> nilLit <|> boolLit <|> symbolLit



    let expr<'u> = lit
