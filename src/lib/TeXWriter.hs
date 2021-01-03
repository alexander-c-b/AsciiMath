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

writeConstant :: Constant -> String
writeConstant = \case
    -- Operation symbols
    GreekLetter s -> cmd s
    Letters s   -> s
    Diff s      -> cmdargs "text" ["d"] ++ s
    Number n    -> n
    StdFun s    -> cmd s
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
    SmallSpace  -> cmd_ ","
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
    And         -> cmdargs "text" ["and"]
    Or          -> cmdargs "text" ["or"]
    Not         -> cmd "neg"
    Implies     -> cmd "Rightarrow"
    If          -> cmdargs "text" ["if"]
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
    Utext  -> "text"
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
writeLeftDelim, writeRightDelim :: Delimiter -> String
writeLeftDelim l = cmd_ "left" ++ case l of
  Parenthesis  -> "("
  Bracket      -> "["
  Brace        -> "\\{"
  AngleBracket -> cmd "langle"
  Invisible    -> "."
writeRightDelim r = cmd_ "right" ++ case r of
  Parenthesis  -> ")"
  Bracket      -> "]"
  Brace        -> "\\}"
  AngleBracket -> cmd "rangle"
  Invisible    -> "."

writeSTerm :: STerm -> String
writeSTerm = \case
  Constant c      -> writeConstant c
  Delimited l e r -> writeLeftDelim l ++ writeCode e ++ writeRightDelim r
  Grouped code    -> "{" ++ writeCode code ++ "}"
  Text s          -> cmdargs "text" [s]

writeTerm :: Term -> String
writeTerm = \case
  STerm st -> writeSTerm st
  Under st1 st2 ->
      writeSTerm st1 ++ "_{" ++ writeSTerm st2 ++ "}"
  Super st1 st2 ->
      writeSTerm st1 ++ "^{" ++ writeSTerm st2 ++ "}"
  SubSuper st1 st2 st3 ->
      writeSTerm st1 ++
      "_{" ++ writeSTerm st2 ++ "}" ++
      "^{" ++ writeSTerm st3 ++ "}"

-- Writes a simple expression
writeSimple :: Simple -> String
writeSimple = \case
  Term t -> writeTerm t
  Unary o t ->
      cmdargs (writeUnaryOp o) [writeTerm t]
  Binary BFrac t1 t2 ->
      cmdargs "frac" [writeTerm t1, writeTerm t2]
  Binary BRoot t1 t2 ->
      cmdargs ("sqrt[" ++ writeTerm t1 ++ "]") [writeTerm t2]
  Binary BStackRel t1 t2 ->
      cmdargs "stackrel" [writeTerm t1, writeTerm t2]

-- Writes an expression
writeExpr :: Expr -> String
writeExpr = \case
  Simple s -> writeSimple s
  Frac s1 s2 -> cmdargs "frac" (map writeSimple [s1,s2])


writeExprs :: [Expr] -> String
writeExprs = concatMap writeExpr

-- The main writer
writeTeX, writeCode :: Code -> String
writeTeX  = writeCode
writeCode = \case
    Exprs es -> writeExprs es
    Matrix rows ->
        let expr = intercalate " \\\\ " $
                    map (intercalate " & " . map writeExprs) rows
         in cmdargs "begin" ["matrix"] ++ expr ++ cmdargs "end" ["matrix"]
