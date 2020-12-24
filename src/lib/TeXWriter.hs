{-# LANGUAGE LambdaCase #-}
module TeXWriter (writeTeX) where
import Ast
import Data.List (intercalate)

cmd_ :: String -> String
cmd_ = ("\\" ++)

cmd :: String -> String
cmd = cmd_ . (++ " ")

cmdargs :: String -> [String] -> String
cmdargs c = concat . (cmd_ c:) . map (\a -> "{" ++ a ++ "}")

writeConst :: Constant -> String
writeConst = \case
    -- Operation symbols
    (GreekLetter s) -> cmd s
    (Letters s) -> s
    (Number n)  -> n
    (StdFun s)  -> cmd s
    Add         -> "+"
    Sub         -> "-"
    Mul         -> cmd "cdot"
    Mmul        -> cmd "ast"
    Mmmul       -> cmd "star"
    Sslash      -> "/"
    Bbslash     -> cmd "backslash"
    Times       -> cmd "times"
    Div         -> cmd "div"
    Comp        -> cmd "circ"
    Oplus       -> cmd "oplus"
    Otimes      -> cmd "otimes"
    Odot        -> cmd "odot"
    Sum         -> cmd "sum"
    Prod        -> cmd "prod"
    Wedge       -> cmd "wedge"
    Wwedge      -> cmd "bigwedge"
    Vv          -> cmd "vee"
    Vvv         -> cmd "bigvee"
    Nn          -> cmd "cap"
    Nnn         -> cmd "bigcap"
    Uu          -> cmd "cup"
    Uuu         -> cmd "bigcup"
    -- Miscellaneous symbols
    Inte        -> cmd "int"
    Oint        -> cmd "oint"
    Del         -> cmd "partial"
    Grad        -> cmd "nabla"
    Addsub      -> cmd "pm"
    Void        -> cmd "emptyset"
    Infty       -> cmd "infty"
    Aleph       -> cmd "aleph"
    Angle       -> cmd "angle"
    Therefore   -> cmd "therefore"
    Abs         -> "|"
    Cdots       -> cmd "cdots"
    Vdots       -> cmd "vdots"
    Ddots       -> cmd "ddots"
    Bslash      -> "\\"
    Comma       -> ","
    Quad        -> cmd "quad"
    Space       -> cmd_ " "
    Diamond     -> cmd "diamond"
    Square      -> cmd "square"
    Lfloor      -> cmd "lfloor"
    Rfloor      -> cmd "rfloor"
    Lceil       -> cmd "lceil"
    Rceil       -> cmd "rceil"
    Cc          -> cmdargs "mathbb" ["C"]
    Ensnn       -> cmdargs "mathbb" ["N"]
    Qq          -> cmdargs "mathbb" ["Q"]
    Rr          -> cmdargs "mathbb" ["R"]
    Zz          -> cmdargs "mathbb" ["Z"]
    -- Relation symbols
    Eq          -> "="
    Neq         -> cmd "neq"
    Lt          -> "<"
    Gt          -> ">"
    Le          -> cmd "leqslant"
    Ge          -> cmd "geqslant"
    Prec        -> cmd "prec"
    Succ        -> cmd "succ"
    In          -> cmd "in"
    Notin       -> cmd_ "not" ++ cmd "in"
    Subset      -> cmd "subset"
    Supset      -> cmd "supset"
    Subsete     -> cmd "subseteq"
    Supsete     -> cmd "supseteq"
    Mod         -> cmd "equiv"
    Congr       -> cmd "cong"
    Approx      -> cmd "approx"
    Prop        -> cmd "propto"
    -- Logical symbols
    And         -> cmdargs "textrm" ["and"]
    Or          -> cmdargs "textrm" ["or"]
    Not         -> cmd "neg"
    Implies     -> cmd "Rightarrow"
    If          -> cmdargs "textrm" ["if"]
    Iff         -> cmd "Leftrightarrow"
    Forall      -> cmd "forall"
    Exists      -> cmd "exists"
    Falsum      -> cmd "perp"
    Taut        -> cmd "top"
    Turnstile   -> cmd "vdash"
    Tturnstile  -> cmd "models"
    -- Arrows
    Uarr        -> cmd "uparrow"
    Darr        -> cmd "downarrow"
    Larr        -> cmd "leftarrow"
    To          -> cmd "to"
    Mapsto      -> cmd "mapsto"
    Harr        -> cmd "leftrightarrow"
    Llarr       -> cmd "Leftarrow"
    Dot         -> "."
    Semicolon   -> ";"
    Quote       -> "'"
    Facto       -> "!"

-- Writes a unary operator
writeUnaryOp :: UnaryOp -> String
writeUnaryOp = \case
    Usqrt  -> "sqrt"
    Utext  -> "textrm"
    Ubb    -> "boldsymbol"
    Ubbb   -> "mathbb"
    Ucc    -> "mathcal"
    Utt    -> "texttt"
    Ufr    -> "mathfrak"
    Usf    -> "mathsf"
    Utilde -> "tilde"
    Uhat   -> "hat"
    Ubar   -> "overline"
    Uul    -> "underline"
    Uvec   -> "vec"
    Udot   -> "dot"
    Uddot  -> "ddot"

-- Writes the delimiters
writeLBracket :: LBracket -> String
writeRBracket :: RBracket -> String
writeLBracket l = cmd_ "left" ++ case l of
    LPar     -> "("
    LCro     -> "["
    LBra     -> "\\{"
    LChe     -> cmd "langle"
    LBraCons -> "."
writeRBracket r = cmd_ "right" ++ case r of
    RPar     -> ")"
    RCro     -> "]"
    RBra     -> "\\}"
    RChe     -> cmd "rangle"
    RBraCons -> "."

-- Writes a simple expression
writeSimpleExpr :: SimpleExpr -> String
writeSimpleExpr = \case
    SEConst c -> writeConst c
    Matrix css ->
        let expr = intercalate " \\\\ " $
                    map (intercalate " & " . map writeTeX) css
         in cmdargs "begin" ["matrix"] ++ expr ++ cmdargs "end" ["matrix"]
    Delimited l e r ->
        writeLBracket l ++ writeCode e ++ writeRBracket r
    UnaryApp o e ->
        cmdargs (writeUnaryOp o) [writeSimpleExprND e]
    BinaryApp BFrac e1 e2 ->
        cmdargs "frac" [writeSimpleExprND e1, writeSimpleExprND e2]
    BinaryApp BRoot e1 e2 ->
        cmdargs ("sqrt[" ++ writeSimpleExpr e1 ++ "]") [writeSimpleExpr e2]
    BinaryApp BStackRel e1 e2 ->
        cmdargs "stackrel" [writeSimpleExpr e1, writeSimpleExpr e2]
    Raw s -> cmdargs "textrm" [s]

-- Writes a simple expression after removing the embracing delimiters if present
writeSimpleExprND :: SimpleExpr -> String
writeSimpleExprND = \case
    Delimited _ e _ -> writeCode e
    e               -> writeSimpleExpr e

-- Writes an expression
writeExpr :: Expr -> String
writeExpr = \case
    Simple se -> writeSimpleExpr se
    Frac e1 e2 ->
        cmdargs "frac" [writeSimpleExprND e1, writeSimpleExprND e2]
    Under e1 e2 ->
        writeSimpleExpr e1 ++ "_{" ++ writeSimpleExprND e2 ++ "}"
    Super e1 e2 ->
        writeSimpleExpr e1 ++ "^{" ++ writeSimpleExprND e2 ++ "}"
    SubSuper e1 e2 e3 ->
        writeSimpleExpr e1 ++
        "_{" ++ writeSimpleExprND e2 ++ "}" ++
        "^{" ++ writeSimpleExprND e3 ++ "}"

-- Writes a code block
writeCode :: Code -> String
writeCode = foldr (\e s -> writeExpr e ++ s) ""

-- The main writer
writeTeX :: Code -> String
writeTeX = writeCode
