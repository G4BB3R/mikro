module Parser (run) where

import Text.Parsec as P
import Text.Parsec.String as PS
import Data.Maybe
{-| module Color
  ; out rgb strToColor
  ; use String.Extra
  ; decls...
  | -}
data AST
    = AST String [Export] [Import] [Declaration]
        deriving Show

-- out route (..)
data Export
    = Export String [String]
        deriving Show

-- use Color as C (rgb, strToRgb)
data Import
    = Import String String [String]
        deriving Show

data Declaration
    = FuncDecl (Maybe Type) String [String] Expr       -- [@ Int -> Int ] \n sum a b = a + b
    | TypeDecl String [TypeConstructor]        -- type PrimaryColors = Red | Green | Blue
    | AliasDecl String Type                    -- alias Model = { name : String }
        deriving Show

data TypeConstructor 
    = TypeConstructor String [Type]
        deriving Show

data Type
    = TypeInt
    -- | TypeBool
    | TypeChar
    | TypeString
    | TypeFloat
    | TypeCustom String
    | TypeTuple [Type]
    | TypeRecord [(String, Type)]   -- { name : String, age : Int }
    | TypeFn Type Type       -- Bool -> Int
    | TypeParens Type
        deriving Show

data Expr 
    = ExprVal Value
    | ExprVar String 
    | ExprRecord [(String, Expr)]     -- { x = if True then 1 else 2, y = 2 + 2, z = 7 }
    -- | ExprFnCall String 
    | ExprIf Expr Expr Expr           -- if b then a + 2 else let x = 1 in x
    -- | ExprCase Expr [()]                    -- case 1 of ; 1 -> True ; _ -> False
    | ExprLet [Declaration] Expr    -- let a = 1 ; b = 2 in a + b
    -- | ExprLambda
        deriving Show

data Value 
    = ValInt Int            -- 1337
    -- | ValBool Bool          -- True
    | ValChar Char          -- 'a'
    | ValString String      -- "rsrs"
        deriving Show


run :: String -> Either ParseError AST
run input =
    P.parse parse_ast "parse_ast" input

parse_ast :: Parser AST
parse_ast = do
    module_name  <- parse_module_header
    many_lines
    exports      <- parse_exports
    many_lines
    imports      <- parse_imports
    many_lines
    declarations <- many $ try $ parse_declaration <* spaces
    spaces
    eof
    return $ AST module_name exports imports declarations


parse_module_header :: Parser String
parse_module_header =
    string "module " *> identifier <* many_spaces


parse_exports :: Parser [Export]
parse_exports = do
    try $ string "out "
    first  <- identifier <* many_spaces
    others <- many $ try $ endOfLine *> string "    " *> identifier <* many_spaces
    return $ map (\id_ -> Export id_ [])  $ first : others


parse_imports :: Parser [Import]
parse_imports = do
    try $ string "use "
    first  <- import_
    others <- many $ try $ endOfLine *> string "    " *> import_
    return $ first : others
    where 
        import_ = do 
            id_ <- identifier
            many_spaces
            as_ <- optionMaybe $ (try (string "as") *> char ' ' *> many_spaces *> identifier)
            many_spaces
            qualifieds <- do
                try $ char '('
                init_ <- many $ try $ identifier <* many_spaces <* char ',' <* many_spaces
                last_ <- identifier
                char ')'
                many_spaces
                return $ init_ ++ [last_]

            return $ Import id_ (Data.Maybe.fromMaybe id_ as_) qualifieds


parse_declaration :: Parser Declaration
parse_declaration = do
        try parse_type_declaration
    <|> try parse_alias_declaration
    <|> try parse_function_declaration
    <?> "declaration"


parse_function_declaration :: Parser Declaration
parse_function_declaration = do 
    definition_ <- optionMaybe $ do 
        try $ char '@'
        many_spaces
        type_ <- parse_type
        many_spaces
        endOfLine
        return type_
    
    id_ <- identifier
    many_spaces
    params_ <- many $ identifier <* many_spaces
    char '='
    spaces
    expr_ <- parse_expr
    return $ FuncDecl definition_ id_ params_ expr_

parse_expr :: Parser Expr 
parse_expr = 
        try parse_expr_if
    <|> try parse_expr_let
    <|> try parse_expr_record
    <|> try (parse_expr_value >>= return . ExprVal)
    <|> try parse_expr_var
    <?> "expression"

parse_expr_var :: Parser Expr
parse_expr_var = do
    id_ <- identifier_L
    return $ ExprVar id_

parse_expr_value :: Parser Value 
parse_expr_value = do 
        try parse_int
    -- <|> try parse_bool
    <|> try parse_char
    <|> try parse_string 
    <?> "expression_value"

    where 
        parse_int :: Parser Value 
        parse_int = do 
            n <- many1 digit 
            return $ ValInt $ read n

        parse_char :: Parser Value 
        parse_char = do 
            char '\''
            c <- alphaNum
            char '\''
            return $ ValChar c

        parse_string :: Parser Value 
        parse_string = do 
            char '"'
            str <- many $ noneOf "\""
            char '"'
            return $ ValString $ str

parse_expr_record :: Parser Expr 
parse_expr_record = do 
    try $ char '{'
    spaces
    head_ <- optionMaybe $ try $ parse_name_and_value
    tail_ <- many $ try $ char ',' *> spaces *> parse_name_and_value
    spaces
    char '}'

    case head_ of 
        Nothing ->
            return $ ExprRecord $ tail_

        Just head_ -> 
            return $ ExprRecord $ head_ : tail_

    where 
        parse_name_and_value = do
            id_ <- identifier
            spaces
            char '='
            spaces
            expr_ <- parse_expr
            spaces
            return (id_, expr_)


parse_expr_if :: Parser Expr 
parse_expr_if = do
    try $ string "if"
    spaces
    expr1 <- parse_expr
    spaces
    string "then"
    spaces
    expr2 <- parse_expr
    spaces
    string "else"
    spaces
    expr3 <- parse_expr
    return $ ExprIf expr1 expr2 expr3

parse_expr_let :: Parser Expr 
parse_expr_let = do
    try $ string "let"
    spaces
    declarations <- many $ try $ parse_declaration <* spaces
    string "in"
    spaces
    expr <- parse_expr
    return $ ExprLet declarations expr


parse_type_declaration :: Parser Declaration
parse_type_declaration = do
    try $ string "type" 
    char ' '
    many_spaces
    id_ <- identifier
    spaces
    char '='
    spaces
    first_ <- type_constructor
    tail_  <- many $ try $ spaces *> char '|' *> many_spaces *> type_constructor
    return $ TypeDecl id_ $ first_ : tail_

    where 
        type_constructor :: Parser TypeConstructor
        type_constructor = do
            id_    <- identifier
            types_ <- many $ try $ char ' ' *> many_spaces *> parse_type
            return $ TypeConstructor id_ types_


parse_alias_declaration :: Parser Declaration
parse_alias_declaration = do
    try $ string "alias"
    char ' '
    many_spaces
    id_ <- identifier
    spaces
    char '='
    spaces
    type_ <- parse_type
    return $ AliasDecl id_ type_

parse_type :: Parser Type 
parse_type = do 

    type1 <- 

            (try (string "Int")    *> return TypeInt)
        <|> (try (string "Char")   *> return TypeChar)
        <|> (try (string "String") *> return TypeString)
        <|> (try (string "Float")  *> return TypeFloat)
        <|> (try identifier >>= return . TypeCustom)
        <|> try parse_tuple
        <|> try parse_record_type
        <|> try parse_type_parens
        <?> "Type!"

    arrow_ <- optionMaybe $ try $ many_spaces *> string "->" *> many_spaces >> return ()

    case arrow_ of 
        Nothing ->
            return $ type1

        Just () -> do
            type2 <- parse_type
            return $ TypeFn type1 type2


    where
        parse_tuple = do
            try $ char '('
            many_spaces
            head_ <- parse_type
            tail_ <- many1 $ try $ many_spaces *> char ',' *> many_spaces *> parse_type
            many_spaces
            char ')'
            many_spaces
            return $ TypeTuple $ head_ : tail_

        parse_record_type = do 
            try $ char '{'
            spaces
            head_ <- optionMaybe $ try $ parse_name_and_type
            tail_ <- many $ try $ char ',' *> spaces *> parse_name_and_type
            spaces
            char '}'

            case head_ of 
                Nothing ->
                    return $ TypeRecord $ tail_

                Just head_ -> 
                    return $ TypeRecord $ head_ : tail_

            where 
                parse_name_and_type = do
                    id_ <- identifier
                    spaces
                    char ':'
                    spaces
                    type_ <- parse_type
                    spaces
                    return (id_, type_)

        parse_type_parens = do 
            many_spaces
            char '('
            many_spaces
            type_ <- parse_type 
            many_spaces
            char ')'
            many_spaces
            return $ TypeParens type_



identifier :: Parser String
identifier = do
    x  <- letter
    xs <- many alphaNum
    return $ x : xs

identifier_L :: Parser String 
identifier_L = do 
    x  <- lower
    xs <- many alphaNum
    return $ x : xs    

identifier_U :: Parser String 
identifier_U = do 
    x  <- upper
    xs <- many alphaNum
    return $ x : xs    


many_spaces :: Parser ()
many_spaces =
    skipMany $ char ' ' *> return ()


many_lines :: Parser () 
many_lines =
    (try (endOfLine >> many_spaces >> many_lines)) <|> (endOfLine >> return ())


parser_free_spaces_but_keep_in_scope :: Parser ()
parser_free_spaces_but_keep_in_scope = do
    optionMaybe $ try $ do 
        spaces
        endOfLine
        char ' '
        many_spaces
    return ()
