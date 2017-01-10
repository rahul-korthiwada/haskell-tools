{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
-- | Functions that convert the value and function definitions of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Binds where

import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import BasicTypes as GHC
import ApiAnnotation as GHC
import Bag as GHC
import HsPat as GHC
import HsTypes as GHC

import Data.List

import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), AnnMaybeG(..), AnnListG(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfBind :: TransformName n r => Located (HsBind n) -> Trf (Ann AST.UValueBind (Dom r) RangeStage)
trfBind = trfLocNoSema trfBind'
  
trfBind' :: TransformName n r => HsBind n -> Trf (AST.UValueBind (Dom r) RangeStage)
-- a value binding (not a function)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_pats = [], m_grhss = GRHSs [L _ (GRHS [] expr)] (L _ locals) })]} }) 
  = AST.USimpleBind <$> copyAnnot AST.UVarPat (define $ trfName id)
                    <*> addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr))
                    <*> addToScope locals (trfWhereLocalBinds locals)
trfBind' (FunBind id (MG (unLoc -> matches) _ _ _) _ _ _) = AST.UFunBind <$> makeNonemptyIndentedList (mapM (trfMatch (unLoc id)) matches)
trfBind' (PatBind pat (GRHSs rhs (unLoc -> locals)) _ _ _) = AST.USimpleBind <$> trfPattern pat <*> trfRhss rhs <*> trfWhereLocalBinds locals
trfBind' (PatSynBind _) = error "Pattern synonym bindings should be recognized on the declaration level"
trfBind' _ = error "Bindings generated by the compiler cannot be converted"

trfMatch :: TransformName n r => n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.UMatch (Dom r) RangeStage)
trfMatch id = trfLocNoSema (trfMatch' id)

trfMatch' :: TransformName n r => n -> Match n (LHsExpr n) -> Trf (AST.UMatch (Dom r) RangeStage)
trfMatch' name (Match funid pats typ (GRHSs rhss (unLoc -> locBinds)))
  -- TODO: add the optional typ to pats
  = AST.UMatch <$> trfMatchLhs name funid pats
               <*> addToScope pats (trfRhss rhss)
               <*> addToScope pats (trfWhereLocalBinds locBinds)

trfMatchLhs :: TransformName n r => n -> MatchFixity n -> [LPat n] -> Trf (Ann AST.UMatchLhs (Dom r) RangeStage)
trfMatchLhs name fb pats 
  = do implicitIdLoc <- mkSrcSpan <$> atTheStart <*> atTheStart
       closeLoc <- srcSpanStart <$> (combineSrcSpans <$> tokenLoc AnnEqual <*> tokenLoc AnnVbar)
       let (n, isInfix) = case fb of NonFunBindMatch -> (L implicitIdLoc name, False)
                                     FunBindMatch n inf -> (n, inf)
       args <- mapM trfPattern pats
       annLocNoSema (mkSrcSpan <$> atTheStart <*> (pure closeLoc)) $
         case (args, isInfix) of 
            (left:right:rest, True) -> AST.UInfixLhs left <$> define (trfOperator n) <*> pure right <*> makeList " " (pure closeLoc) (pure rest)
            _                       -> AST.UNormalLhs <$> define (trfName n) <*> makeList " " (pure closeLoc) (pure args)

trfRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.URhs (Dom r) RangeStage)
-- the original location on the GRHS misleadingly contains the local bindings
trfRhss [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> tokenBefore (srcSpanStart $ getLoc body) AnnEqual) 
                                         (AST.UUnguardedRhs <$> trfExpr body)
trfRhss rhss = annLocNoSema (pure $ collectLocs rhss) 
                      (AST.UGuardedRhss . nonemptyAnnList <$> mapM trfGuardedRhs rhss)
                      
trfGuardedRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedRhs (Dom r) RangeStage)
trfGuardedRhs = trfLocNoSema $ \(GRHS guards body) 
  -> AST.UGuardedRhs . nonemptyAnnList <$> trfScopedSequence trfRhsGuard guards <*> addToScope guards (trfExpr body)
  
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard = trfLocNoSema trfRhsGuard'
  
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' (BindStmt pat body _ _ _) = AST.UGuardBind <$> trfPattern pat <*> trfExpr body
trfRhsGuard' (BodyStmt body _ _ _) = AST.UGuardCheck <$> trfExpr body
trfRhsGuard' (LetStmt (unLoc -> binds)) = AST.UGuardLet <$> trfLocalBinds binds
trfRhsGuard' _ = error "trfRhsGuard': not a valid guard stmt"
  
trfWhereLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfWhereLocalBinds EmptyLocalBinds = nothing "" "" atTheEnd
trfWhereLocalBinds binds
  = makeJust <$> annLocNoSema (combineSrcSpans (getBindLocs binds) <$> tokenLoc AnnWhere) (AST.ULocalBinds <$> addToScope binds (trfLocalBinds binds))

getBindLocs :: HsLocalBinds n -> SrcSpan
getBindLocs (HsValBinds (ValBindsIn binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
getBindLocs (HsValBinds (ValBindsOut binds sigs)) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
getBindLocs (HsIPBinds (IPBinds binds _)) = foldLocs $ map getLoc binds
getBindLocs EmptyLocalBinds = noSrcSpan
  
trfLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfLocalBinds (HsValBinds (ValBindsIn binds sigs)) 
  = makeIndentedListBefore " " (after AnnWhere)
      (orderDefs <$> ((++) <$> mapM (copyAnnot AST.ULocalValBind . trfBind) (bagToList binds) 
                           <*> mapM trfLocalSig sigs))
trfLocalBinds (HsValBinds (ValBindsOut binds sigs)) 
  = makeIndentedListBefore " " (after AnnWhere)
      (orderDefs <$> ((++) <$> (concat <$> mapM (mapM (copyAnnot AST.ULocalValBind . trfBind) . bagToList . snd) binds)
                           <*> mapM trfLocalSig sigs))
trfLocalBinds (HsIPBinds (IPBinds binds _))
  = makeIndentedListBefore " " (after AnnWhere) (mapM trfIpBind binds)
trfLocalBinds EmptyLocalBinds
  -- TODO: implement
  = error "trfLocalBinds: EmptyLocalBinds not supported yet"

trfIpBind :: TransformName n r => Located (IPBind n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfIpBind = trfLocNoSema $ \case
  IPBind (Left (L l ipname)) expr 
    -> AST.ULocalValBind 
         <$> (annContNoSema $ AST.USimpleBind <$> focusOn l (annContNoSema (AST.UVarPat <$> define (trfImplicitName ipname)))
                                              <*> annFromNoSema AnnEqual (AST.UUnguardedRhs <$> trfExpr expr)
                                              <*> nothing " " "" atTheEnd)
  IPBind (Right _) _ -> error "trfIpBind: called on typechecked AST"
             
trfLocalSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfLocalSig = trfLocNoSema $ \case
  ts@(TypeSig {}) -> AST.ULocalSignature <$> annContNoSema (trfTypeSig' ts)
  (FixSig fs) -> AST.ULocalFixity <$> annContNoSema (trfFixitySig fs)
  _ -> error "trfLocalSig: not a type or fixity sig"
  
trfTypeSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig = trfLocNoSema trfTypeSig'

trfTypeSig' :: TransformName n r => Sig n -> Trf (AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig' (TypeSig names typ) 
  = defineTypeVars $ AST.UTypeSignature <$> makeNonemptyList ", " (mapM trfName names) <*> trfType (hswc_body $ hsib_body typ)
trfTypeSig' _ = error "trfTypeSig': not a type sig"

trfFixitySig :: TransformName n r => FixitySig n -> Trf (AST.UFixitySignature (Dom r) RangeStage)
trfFixitySig (FixitySig names (Fixity _ prec dir)) 
  = AST.UFixitySignature <$> transformDir dir
                         <*> annLocNoSema (tokenLoc AnnVal) (pure $ AST.Precedence prec) 
                         <*> (nonemptyAnnList . nub <$> mapM trfOperator names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLocNoSema (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)
        
        directionChar = annLocNoSema ((\l -> mkSrcSpan (updateCol (subtract 1) l) l) . srcSpanEnd <$> tokenLoc AnnInfix)