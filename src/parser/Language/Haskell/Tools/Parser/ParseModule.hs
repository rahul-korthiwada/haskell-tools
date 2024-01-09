{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds #-}
module Language.Haskell.Tools.Parser.ParseModule where
  
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

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False True

initGhcFlags' :: Bool -> Bool -> Ghc ()
initGhcFlags' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ (if errorsSuppressed then flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ foldl' (\acc x -> xopt_set acc x) (dflags { importPaths = []
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }) [
                                BlockArguments
                                ,ConstraintKinds
                                ,DataKinds
                                ,DeriveAnyClass
                                ,DeriveDataTypeable
                                ,DeriveFoldable
                                ,DeriveFunctor
                                ,DeriveGeneric
                                ,DeriveTraversable
                                ,DerivingStrategies
                                ,DerivingVia
                                ,DuplicateRecordFields
                                ,EmptyCase
                                ,ExplicitForAll
                                ,ExplicitNamespaces
                                ,FlexibleContexts
                                ,FlexibleInstances
                                ,GADTs
                                ,GeneralizedNewtypeDeriving
                                ,ImplicitParams
                                ,ImplicitPrelude
                                ,InstanceSigs
                                ,KindSignatures
                                ,LambdaCase
                                ,MagicHash
                                ,MultiParamTypeClasses
                                ,MultiWayIf
                                ,OverloadedLabels
                                ,OverloadedStrings
                                ,PatternSynonyms
                                ,QuasiQuotes
                                ,RankNTypes
                                ,RecordWildCards
                                ,ScopedTypeVariables
                                ,TemplateHaskell
                                ,TupleSections
                                ,TypeApplications
                                ,TypeFamilies
                                ,TypeOperators
                                ,TypeSynonymInstances
                                ,UndecidableInstances
                                ,ViewPatterns
                                ,BangPatterns
                                ,AllowAmbiguousTypes
                                ,UnicodeSyntax
                                ,StandaloneDeriving
                                ,EmptyDataDecls
                                ,FunctionalDependencies
                                ,PartialTypeSignatures
                                -- ,NamedFieldPuns
                                -- ,NoImplicitPrelude
                                ,Strict
                                ,EmptyDataDeriving
                                ,PolyKinds
                                ,ExistentialQuantification 
                          ]

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = do initGhcFlags' True False
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags dfs  

loadModule :: FilePath -> String -> Ghc ModSummary
loadModule workingDir moduleName
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       void $ load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName         

foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

moduleParser :: String -> String -> IO ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage))
moduleParser modulePath moduleName = do
    dflags <- runGhc (Just libdir) getSessionDynFlags
    pp <- getCurrentDirectory
    print (replace (replace "." "/" moduleName) "" $ replace ".hs" "" modulePath)
    modSum <- runGhc (Just libdir) $ loadModule (replace (replace "." "/" moduleName) "" $ replace ".hs" "" modulePath) moduleName
    y <- runGhc (Just libdir) $ parseModule modSum
    let annots = pm_annotations y
    valsss <- runGhc (Just libdir) $ runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum (pm_parsed_source y)
    sourceOrigin <- return (fromJust $ ms_hspp_buf $ pm_mod_summary y)
    newAst <- runGhc (Just libdir) $ (prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
        <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum $ pm_parsed_source y)
    pure newAst

moduleParserGhc :: String -> String -> IO [(String,Int,Int)]
moduleParserGhc modulePath moduleName = do
    dflags <- runGhc (Just libdir) getSessionDynFlags
    pp <- getCurrentDirectory
    print (replace (replace "." "/" moduleName) "" $ replace ".hs" "" modulePath)
    modSum <- runGhc (Just libdir) $ loadModule (replace (replace "." "/" moduleName) "" $ replace ".hs" "" modulePath) moduleName
    y <- runGhc (Just libdir) $ parseModule modSum
    let annots = pm_annotations y
    runGhc (Just libdir) $ mapM getFunctionInfo $ getFunctions $ unLoc $ pm_parsed_source y
    where
      getFunctionInfo :: (Located (HsDecl GhcPs),String) -> Ghc (String,Int,Int)
      getFunctionInfo (decl,name) = do
        let startLine = maybe 0 srcLocLine $ extractRealSrcLoc $ (srcSpanStart (getLoc decl))
            endLine   = maybe 0 srcLocLine $ extractRealSrcLoc $ (srcSpanEnd (getLoc decl))
        liftIO $ putStrLn $ (trim name) <> " : Function at lines " ++ show startLine ++ " - " ++ show endLine
        pure $ (trim name,startLine,endLine)

      extractRealSrcLoc :: SrcLoc -> Maybe RealSrcLoc
      extractRealSrcLoc (RealSrcLoc x) = Just x
      extractRealSrcLoc _ = Nothing

      getFunctions :: (HsModule GhcPs) -> [(Located (HsDecl GhcPs),String)]
      getFunctions parsedModule = do
        -- let (functions, nonFunctions) = foldl' (\acc res -> do
        --         if isFunction res
        --           then ((fst acc Prelude.<> [(unLoc res)]),snd acc)
        --           else ((fst acc),snd acc Prelude.<> [(unLoc res)])
        --         ) ([],[]) (hsmodDecls $ parsedModule)
        --     groupedFunctions = groupBy (\a b -> isFunSig a && isFunVal b ) functions
        --     groupFunctionsMore = groupByUltimate groupedFunctions --unionBy (\a b -> getInfixlFun a b || getInfixlFun b a) (groupedFunctions)
        concat $ map extractFunctions $ hsmodDecls (parsedModule)

      extractFunctions :: LHsDecl GhcPs -> [(Located (HsDecl GhcPs),String)]
      extractFunctions funD@(L _ (ValD _ decl)) = [(funD,getFunctionName $ showSDocUnsafe $ ppr $ funD)]
      extractFunctions sigD@(L _ (SigD _ decl)) = [(sigD,getFunctionName $ showSDocUnsafe $ ppr $ sigD)]
      extractFunctions _             = []


isFunction :: _ -> Bool
isFunction (L _ (SigD _ sigDecls)) = True
isFunction (L _ (ValD _ valDecls)) = True
isFunction _ = False

isFunSig :: _ -> Bool
isFunSig (SigD _ sigDecls) = True
isFunSig _ = False

isFunVal :: _ -> Bool
isFunVal (ValD _ valDecls) = True
isFunVal _ = False

getFunctionName :: _ -> String
getFunctionName str = 
  if any (\x -> x `isInfixOf` str) ["infixl", "infixr", "infix", "INLINE", "NOINLINE"]
    then (splitOn " " $  replace "]" "" $ replace "[" "" str) !! 2
    else head . splitOn " " . replace "(" "" . replace ")" "" . replace "]" "" $ replace "[" "" $ str

groupByUltimate :: [[HsDecl GhcPs]] -> [(String,[[HsDecl GhcPs]])]
groupByUltimate = (HM.toList . foldl' (\acc x -> addToBucket acc x) HM.empty)
  where
    addToBucket :: HM.HashMap String [[HsDecl GhcPs]] -> [HsDecl GhcPs] -> HM.HashMap String [[HsDecl GhcPs]]
    addToBucket acc el = 
      let funcName = getFunctionName $ showSDocUnsafe $ ppr $ el
      in (HM.insert funcName $ 
              case HM.lookup funcName acc of
                Just x -> x ++ [el]
                _ -> [el]
          ) acc