{-|
  Description: Tools to wrap “sudo” and “gksudo” using the "NixManager.Bash" module.
  -}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.AskPass
  ( sudoExpr
  , askPass
  )
where

import           Data.Text          (Text)
import           NixManager.Bash    (Arg (LiteralArg), Expr (Command), evalExpr)
import           NixManager.Process (ProcessData, noStdin, runProcess)
import           Prelude            hiding (readFile)


-- |Transform the expression, evaluating it inside a sudo expression
sudoExpr :: Expr -> Expr
sudoExpr e = Command
  "sudo"
  ["-H", "-S", "-u", "root", "--", "sh", "-c", LiteralArg (evalExpr e)]

-- |Expression to run “gksudo” with the specified description, printing the password on stdout
askPassExpr :: Text -> Expr
askPassExpr description =
  Command "gksudo" ["--description", LiteralArg description, "--print-pass"]

-- |Run “gksudo” with a fitting description, printing the password on stdout
askPass :: IO ProcessData
askPass = runProcess noStdin (askPassExpr "NixOS Manager")



