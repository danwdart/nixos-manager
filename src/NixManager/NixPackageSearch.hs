{-|
  Description: Functions that wrap @nix search@
Functions that wrap @nix search@
  -}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixPackageSearch
  ( searchPackages
  )
where

import           Control.Lens          (folded, to, (^.), (^?!))
import           Data.Monoid           (First (getFirst))
import           Data.Text             (Text)
import           Data.Validation       (Validation (Failure))
import           NixManager.Bash       (Arg (LiteralArg), Expr (Command))
import           NixManager.NixPackage (NixPackage, readPackagesJson)
import           NixManager.Process    (runProcessToFinish)
import           NixManager.Util       (TextualError, addToError, decodeUtf8,
                                        fromStrictBS, showText)
import           System.Exit           (ExitCode (ExitFailure, ExitSuccess))

-- | Expression to call @nix search --json <search-term>@
nixSearchExpr :: Text -> Expr
nixSearchExpr term = Command "nix" ["search", LiteralArg term, "--json"]

-- | Call @nix search@ with a search parameter, return the parsed result
searchPackages :: Text -> IO (TextualError [NixPackage])
searchPackages t = do
  po <- runProcessToFinish Nothing (nixSearchExpr t)
  let
    processedResult = addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (readPackagesJson (po ^. #stdout . fromStrictBS))
  case po ^?! #result . to getFirst . folded of
    ExitSuccess      -> pure processedResult
    ExitFailure 1    -> pure processedResult
    ExitFailure code -> pure
      (Failure
        (  "Error executing \"nix search\" command (exit code "
        <> showText code
        <> "): standard error output: "
        <> (po ^. #stderr . decodeUtf8)
        )
      )


