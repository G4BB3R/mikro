module Parser (run) where

import Text.Parsec as P
import Text.Parsec.String as PS
import Data.Maybe

{-  (module Color
        (export rgb strToColor)
        (import String.Extra))
-}
data AST
    = AST String [Export] [Import] [Declaration]
        deriving Show

data Export
    = Export String [String]
        deriving Show

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
    | TypeChar
    | TypeString
    | TypeFloat
    | TypeCustom String
    -- | TypeTuple [Type]
    | TypeRecord [(String, Type)]  -- { name : String, age : Int }
    | TypeFn Type Type                -- Bool -> Int
    | TypeParens Type
        deriving Show

data Expr 
    = ExprVal Value
    | ExprVar String 
    | ExprRecord [(String, Expr)]     -- { x = if True then 1 else 2, y = 2 + 2, z = 7 }
    | ExprFnCall String [Expr]
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
    spaces 
    char '('
    spaces
    string "module"
    spaces
    module_name  <- identifier_U
    spaces
    exports      <- parse_exports
    spaces
    imports      <- parse_imports
    spaces
    char ')'
    spaces
    declarations <- many $ try $ parse_declaration <* spaces
    spaces
    eof
    return $ AST module_name exports imports declarations


parse_exports :: Parser [Export]
parse_exports = do
    try $ char '(' >> spaces >> string "export"
    spaces
    exports <- many $ try $ identifier <* spaces
    char ')'
    return $ map (\id_ -> Export id_ [])  $ exports


parse_imports :: Parser [Import]
parse_imports = do
    try $ char '(' >> string "import"
    spaces
    imports_ <- many $ try $ import_
    spaces
    char ')'
    return $ imports_
    where 
        import_ = do 
            try $ char '('
            id_ <- identifier_namespaced_U
            spaces
            as_ <- optionMaybe $ (try (string "as") *> spaces *> identifier_U)
            spaces
            qualifieds <- optionMaybe $ do
                try $ char '('
                spaces
                imports_ <- many $ try $ identifier <* spaces
                char ')'
                return imports_
            spaces
            char ')'
            spaces

            return $ Import id_ (Data.Maybe.fromMaybe id_ as_) (Data.Maybe.fromMaybe [] qualifieds)


parse_declaration :: Parser Declaration
parse_declaration = do
        parse_type_declaration
    <|> parse_alias_declaration
    <|> parse_function_declaration
    <?> "declaration"


parse_function_declaration :: Parser Declaration
parse_function_declaration = do 
    definition_ <- optionMaybe $ do 
        try $ char '(' >> spaces >> char '@'
        spaces
        type_ <- parse_type
        spaces
        char ')'
        spaces
        return type_
    
    defn <- try $ char '(' >> ((try $ string "defn") <|> string "def")
    spaces
    id_ <- identifier
    spaces
    params_ <- do
        if defn == "defn" then do 
            char '['
            spaces
            params_ <- many $ identifier <* spaces
            char ']'
            spaces
            return params_
        else
            return []

    expr_ <- parse_expr
    spaces
    char ')'
    return $ FuncDecl definition_ id_ params_ expr_

parse_expr :: Parser Expr 
parse_expr = 
        parse_expr_if
    <|> parse_expr_let
    <|> parse_expr_function_call
    <|> parse_expr_record
    <|> (parse_expr_value >>= return . ExprVal)
    <|> parse_expr_var
    <?> "expression"

parse_expr_function_call :: Parser Expr 
parse_expr_function_call = do 
    try $ char '('
    spaces
    id_ <- identifier_namespaced_L
    spaces
    args <- many $ try $ parse_expr <* spaces
    char ')'
    return $ ExprFnCall id_ args


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
    try $ char '(' >> string "if"
    spaces
    expr1 <- parse_expr
    spaces
    expr2 <- parse_expr
    spaces
    expr3 <- parse_expr
    spaces
    char ')'
    return $ ExprIf expr1 expr2 expr3


parse_expr_let :: Parser Expr
parse_expr_let = do
    try $ char '(' >> string "let"
    spaces
    char '['
    spaces
    declarations <- many $ try $ parse_declaration <* spaces
    spaces
    char ']'
    spaces
    expr <- parse_expr
    spaces
    char ')'
    return $ ExprLet declarations expr


parse_type_declaration :: Parser Declaration
parse_type_declaration = do
    try $ char '(' >> string "type"
    spaces
    id_ <- identifier
    spaces
    constructors  <- many $ try $ type_constructor <* spaces
    spaces
    char ')'
    return $ TypeDecl id_ $ constructors

    where 
        type_constructor :: Parser TypeConstructor
        type_constructor =
            (do try $ char '('
                id_    <- identifier_U
                types_ <- many $ try $ spaces *> parse_type
                spaces
                char ')'
                return $ TypeConstructor id_ types_)

            <|> (identifier_U >>= (\id_ -> return $ TypeConstructor id_ []))


parse_alias_declaration :: Parser Declaration
parse_alias_declaration = do
    try $ char '(' >> string "alias"
    spaces
    id_ <- identifier_U
    spaces
    type_ <- parse_type
    spaces
    char ')'
    return $ AliasDecl id_ type_

parse_type :: Parser Type 
parse_type = do 

    type1 <- 

            (try (string "Int")    *> return TypeInt)
        <|> (try (string "Char")   *> return TypeChar)
        <|> (try (string "String") *> return TypeString)
        <|> (try (string "Float")  *> return TypeFloat)
        <|> (try identifier >>= return . TypeCustom)
       -- <|> try parse_tuple
        <|> try parse_record_type
        <|> try parse_type_parens
        <?> "Type!"

    arrow_ <- optionMaybe $ try $ spaces *> string "->" *> spaces >> return ()

    case arrow_ of 
        Nothing ->
            return $ type1

        Just () -> do
            type2 <- parse_type
            return $ TypeFn type1 type2


    where
        -- parse_tuple = do
        --     try $ char '('
        --     spaces
        --     head_ <- parse_type
        --     tail_ <- many1 $ try $ spaces *> char ',' *> spaces *> parse_type
        --     spaces
        --     char ')'
        --     spaces
        --     return $ TypeTuple $ head_ : tail_

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
            spaces
            char '('
            spaces
            type_ <- parse_type 
            spaces
            char ')'
            spaces
            return $ TypeParens type_


identifier_operator :: Parser String 
identifier_operator =
    many $ oneOf "!@#$%^&*/=+-_.:?><"

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

identifier_namespaced_U :: Parser String 
identifier_namespaced_U = do
    m_ <- optionMaybe $ try $ do id_ <- identifier_U ; dot <- string "." ; return $ id_ ++ dot
    id_ <- identifier_U
    case m_ of 
        Nothing -> return id_
        Just namespace -> return $ namespace ++ id_

identifier_namespaced_L :: Parser String 
identifier_namespaced_L = do
    m_ <- optionMaybe $ try $ do id_ <- identifier_U ; dot <- string "." ; return $ id_ ++ dot
    id_ <- identifier_L <|> identifier_operator
    case m_ of 
        Nothing -> return id_
        Just namespace -> return $ namespace ++ id_
