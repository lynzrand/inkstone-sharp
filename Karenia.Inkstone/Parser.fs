namespace rec Karenia.Inkstone.Parser.Ast

type Literal =
    | Number of FParsec.CharParsers.NumberLiteral
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
    | Gt
    | Ge
    | Lt
    | Le
    | Eq
    | Neq

type UnaryOp =
    | Not
    | Pos
    | Neg

type BinaryExpr = { lhs: Expr; rhs: Expr; op: BinaryOp }

type UnaryExpr = { lhs: Expr; op: UnaryOp }

type AssignExpr = { lhs: LExpr; rhs: Expr }

type IfExpr =
    {
        cond: Expr
        true_body: Expr
        false_body: Expr Option
    }

type WhileExpr = { cond: Expr; body: Expr }

type LExpr = Ident of string

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

namespace Karenia.Inkstone.Parser

open FParsec
open Karenia.Inkstone.Parser.Ast

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

    /// Parse a line feed
    let lf<'u> = skipNewline

    /// Parse a non-linefeed space
    let sp (cs: CharStream<'u>) =
        while isAnyOf " \t\xa0" (cs.Peek()) do
            cs.Read() |> ignore

        Reply(())

    /// Parse a line-feed or non-line-feed based on parentheses stack
    let lfi =
        getUserState<St>
        >>= (fun st ->
            if st.parenStack.Head then
                spaces
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

    let mutable expr, exprRef =
        createParserForwardedToRef<Expr AstNode, St> ()

    let binOpParser<'u> =
        let parser = OperatorPrecedenceParser()

        let addBin op prec ass bop =
            parser.AddOperator(
                InfixOperator(
                    op,
                    expr,
                    prec,
                    ass,
                    fun l r -> BinaryExpr { lhs = l; rhs = r; op = bop }
                )
            )

        let addUnary op prec bop =
            parser.AddOperator(
                PrefixOperator(
                    op,
                    expr,
                    prec,
                    true,
                    fun l -> UnaryExpr { lhs = l; op = bop }
                )
            )

        addBin "." 120 Associativity.Left Pow

        addBin "**" 90 Associativity.Left Pow

        addBin "*" 80 Associativity.Left Mul
        addBin "/" 80 Associativity.Left Div
        addBin "%" 80 Associativity.Left Mod

        addBin "&" 70 Associativity.Left BitAnd
        addBin "|" 70 Associativity.Left BitOr
        addBin "^" 70 Associativity.Left BitXor

        addBin "+" 60 Associativity.Left Add
        addBin "-" 60 Associativity.Left Sub

        addBin ">" 50 Associativity.None Gt
        addBin "<" 50 Associativity.None Lt
        addBin "==" 50 Associativity.None Eq
        addBin "!=" 50 Associativity.None Neq
        addBin ">=" 50 Associativity.None Ge
        addBin "<=" 50 Associativity.None Le

        addBin "and" 40 Associativity.Left BoolAnd
        addBin "or" 30 Associativity.Left BoolOr

        addUnary "+" 110 Pos
        addUnary "-" 110 Neg
        addUnary "!" 110 Not

        addUnary "not" 20 Not

        parser

    do
        exprRef.Value <-
            astNode (
                choice [ lit |>> LiteralExpr
                         binOpParser ]
            )
