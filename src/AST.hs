module AST (AST(..), Export(..), Import(..), Declaration(..), TypeConstructor(..), Type(..), Expr(..), Value(..)) where

{-  (module Color
        (export rgb strToColor)
        (import String.Extra))
-}
data AST
    = AST String [Export] [Import] [Declaration]
        deriving (Show, Eq)

data Export
    = Export String [String]
        deriving (Show, Eq)

data Import
    = Import String String [String]
        deriving (Show, Eq)

data Declaration
    = FuncDecl (Maybe Type) String [String] Expr       -- [@ Int -> Int ] \n sum a b = a + b
    | TypeDecl String [TypeConstructor]        -- type PrimaryColors = Red | Green | Blue
    | AliasDecl String Type                    -- alias Model = { name : String }
        deriving (Show, Eq)

data TypeConstructor 
    = TypeConstructor [Type]
        deriving (Show, Eq)

data Type
    = TypeInt
    | TypeChar
    | TypeString
    | TypeFloat
    | TypeList Type
    | TypeCustom [String]
    -- | TypeTuple [Type]
    | TypeRecord [(String, Type)]  -- { name : String, age : Int }
    | TypeFn Type Type                -- Bool -> Int
    | TypeParens Type
        deriving (Show, Eq)

data Expr 
    = ExprVal Value
    | ExprList [Expr]
    | ExprVar String 
    | ExprRecord [(String, Expr)]     -- { x = if True then 1 else 2, y = 2 + 2, z = 7 }
    | ExprFnCall String [Expr]
    | ExprIf Expr Expr Expr           -- if b then a + 2 else let x = 1 in x
    | ExprCase Expr [(Expr, Expr)]                    -- case 1 of ; 1 -> True ; _ -> False
    | ExprLet [Declaration] Expr    -- let a = 1 ; b = 2 in a + b
    | ExprIs Expr String              -- (is expr ExprIs)
    | ExprLambda [String] Expr
        deriving (Show, Eq)

data Value 
    = ValInt Int            -- 1337
    -- | ValBool Bool          -- True
    | ValChar Char          -- 'a'
    | ValString String      -- "rsrs"
        deriving (Show, Eq)
