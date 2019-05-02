module CodeGen (run) where

import AST
import Data.List (intersperse, find)
import Data.Maybe (fromMaybe)

run :: AST -> String 
run (AST id_ exports imports declarations) =
    unlines $ map declaration_to_code declarations
    
declaration_to_code :: Declaration -> String 
declaration_to_code declaration =
    case declaration of 
        FuncDecl _ name args expr ->
            "const " ++ name ++ " = (" ++ (concat $ intersperse "," $ args) ++ ") => " ++ expr_to_code expr ++ "\n"

        TypeDecl name constructors ->
            "/* [TypeDecl] */"

        AliasDecl name type_ ->
            "/* alias " ++ name ++ " */\n"


operators :: [(String, String)]
operators = 
    [ ("+",  "soma")
    , ("-",  "minus")
    , ("*",  "mul")
    , ("/",  "div")
    , ("%",  "mod")
    , (">",  "gt")
    , ("<",  "ls")
    , ("=>", "gtOrEq")
    , ("<=", "lsOrEq")
    , ("==", "equals")
    ]

expr_to_code :: Expr -> String 
expr_to_code expr =
    case expr of
        ExprVal val ->
            case val of 
                ValInt n ->
                    show n

                ValChar c ->
                    c : []

                ValString str ->
                    "\"" ++ str ++ "\""

        ExprList exprs ->
            "[" ++ (concat . intersperse "," . map expr_to_code $ exprs) ++ "]"

        ExprVar var -> 
            var

        ExprRecord kvs -> -- fix
            "{ ... }"

        ExprFnCall name exprs ->
            let 
                renamedOp =
                    fromMaybe name $ fmap snd $ find (\(op, _) -> op == name) operators
            in
                renamedOp ++ "(" ++ (concat . intersperse "," . map expr_to_code $ exprs) ++ ")"

        ExprIf expr_if expr_then expr_else -> -- fix
            expr_to_code expr_if ++ " ? " ++ expr_to_code expr_then ++ " : " ++ expr_to_code expr_else ++ ")"

        ExprCase expr kvs -> -- fix
            "(function () { const expr____ = " ++ expr_to_code expr ++ " ;\n if (false){}\n" ++ (unlines $ map kv_to_code $ kvs) ++ " \n})()"
            where
                kv_to_code (expr1, expr2) =
                    if expr1 == ExprVar "_" then 
                        "else return " ++ expr_to_code expr2
                    else
                        "else if (expr____ ===" ++ expr_to_code expr1  ++ ")\n {\nreturn " ++ expr_to_code expr2 ++ ";\n}\n"

        ExprLet declarations expr ->
            "\n; () => " ++ (unlines . map ((++) "    " . declaration_to_code) $ declarations) ++ "\n    " ++ expr_to_code expr

        ExprIs expr str ->
            "[?]"

        ExprLambda args expr ->
            "(" ++ (concat $ intersperse ", " args) ++ ") => " ++ expr_to_code expr
