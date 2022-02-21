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

type St =
    {
        parenStack: bool list
    }
    static member Default = { parenStack = list.Empty }

module Parser =
    /// Get the current position in the char stream
    let currPos<'u> (charStream: CharStream<'u>) = Reply charStream.Index

    /// Wrap the current parsing result inside an <see>AstNode</see>
    let astNode<'ch, 'u> (parser: Parser<'ch, 'u>) =
        pipe3 currPos parser currPos (fun s v e ->
            ({ node = v; offset = s; length = e - s }))

    let lf = skipNewline
    let sp = skipAnyOf " \t"

    let lfi =
        getUserState<St>
        >>= (fun st ->
            if st.parenStack.Head then
                skipMany (lf >>. sp)
            else
                sp)

    let numLit<'u> =
        numberLiteral
            (NumberLiteralOptions.DefaultInteger
             ||| NumberLiteralOptions.DefaultFloat)
            "number literal"
        |>> Number

    let nilLit<'u> =
        pstring "nil" <?> "nil literal" |>> fun _ -> Nil

    let boolLit<'u> =
        ((pstring "true" |>> fun _ -> Boolean true)
         <|> (pstring "false" |>> fun _ -> Boolean false))
        <?> "boolean literal"

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

    let stringLit<'u> = stringLitInternal |>> String

    let lit<'u> =
        numLit
        <|> nilLit
        <|> boolLit
        <|> symbolLit
        <|> stringLit

    let expr<'u> = astNode (lit |>> LiteralExpr)
