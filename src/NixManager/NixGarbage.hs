{-|
  Description: Expressions and commands relating to @nix-collect-garbage@. Uses the "NixManager.Bash" module
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixGarbage
  ( collectGarbage
  )
where

import           Data.Text.Encoding  (encodeUtf8)
import           NixManager.AskPass  (sudoExpr)
import           NixManager.Bash     (Expr (Command))
import           NixManager.Password (Password, getPassword)
import           NixManager.Process  (ProcessData, runProcess)
import           NixManager.Util     (mwhen)
import           Prelude             hiding (readFile)

-- | Bash expression for @nix-collect-garbage@
collectGarbageExpr :: Bool -> Expr
collectGarbageExpr olderGenerations =
  Command "nix-collect-garbage" (mwhen olderGenerations ["-d"])

-- | Run the collect garbage tool using the password and possibly deleting older generations
collectGarbage :: Bool -> Password -> IO ProcessData
collectGarbage olderGenerations password = runProcess
  (Just (encodeUtf8 (getPassword password)))
  (sudoExpr (collectGarbageExpr olderGenerations))

