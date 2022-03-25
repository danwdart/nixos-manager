{-|
  Description: Bash expressions for some POSIX tools

Bash expressions for some POSIX tools
-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.PosixTools
  ( mkdir
  , cp
  , mv
  , kill
  )
where

import           Data.Foldable      (toList)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (pack)
import           NixManager.Bash    (Arg (LiteralArg, RawArg), Expr (Command))
import           NixManager.Util    (mwhen, showText)
import           System.Process     (Pid)

-- | Wrapper for @mkdir@ (first parameter specifies recursion)
mkdir :: Bool -> NonEmpty FilePath -> Expr
mkdir recursive paths = Command
  "mkdir"
  (mwhen recursive ["-p"] <> toList (LiteralArg . pack <$> paths))

-- | Wrapper for @cp@
cp :: FilePath -> FilePath -> Expr
cp from to = Command "cp" (LiteralArg <$> [pack from, pack to])

-- | Wrapper for @mv@
mv :: FilePath -> FilePath -> Expr
mv from to = Command "mv" (LiteralArg <$> [pack from, pack to])

-- | Wrapper for @kill@ (currently only @-9@)
kill :: Pid -> Expr
kill pid = Command "kill" ["-9", RawArg (showText pid)]
