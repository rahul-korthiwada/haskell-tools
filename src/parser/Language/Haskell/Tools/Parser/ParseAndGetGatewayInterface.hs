{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}
module Language.Haskell.Tools.Parser.ParseAndGetGatewayInterface
where

import Debug.Trace
import Data.Data
import GHC hiding (loadModule)
import qualified GHC
import Outputable (Outputable(..), showSDocUnsafe, cat)
import GHC.Paths ( libdir )
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Extra (splitOn,trim,replace, cons)
import GHC.LanguageExtensions
import Control.Exception
import Data.Functor
import Data.Maybe
import System.Directory
import Debug.Trace (traceShowId)
import Shelly
import Control.Concurrent
import SrcLoc (noSrcSpan, combineSrcSpans)
import DynFlags
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.PrettyPrint.Prepare
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.BackendGHC.Exprs (trfText')
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfName)
import Language.Haskell.Tools.BackendGHC.Modules hiding (trfModuleHead)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.Parser.SplitModule
import qualified Data.Set as DS
import Data.Char (isUpper)
import qualified Data.Map as DM

isAllUppercase :: String -> Bool
isAllUppercase = all isUpper

main :: IO ()
main = do
    functionToCaseMatchStatements <- getCaseMatchStatements "/home/juspay/Documents/code/euler-api-gateway/src/" "Euler.API.Gateway.Gateway.Common"
    -- let functionsList = getFunctions parsedModule
    let gateways = filter isAllUppercase $  DS.toList $ DS.fromList $ concat $ map (\(a,b) -> b) functionToCaseMatchStatements
    let fCM = map (\(a,b) -> (a, DS.fromList b)) functionToCaseMatchStatements
    let stringOfValues = foldl' (\b (fN, fM) -> b <> "\n" <> fN <> ((intercalate "," ((\gw -> if DS.member gw fM then "True" else "False") <$> gateways))) ) ("," <> (intercalate "," (gateways))) fCM
    -- print 
    print(stringOfValues)
    pure ()

-- getASTFunctions :