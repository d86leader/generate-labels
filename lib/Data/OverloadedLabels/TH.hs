{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Overlapping record field names are great, but you can rarely use the
-- fields as accessor functions, because GHC complains about overlaps.
-- OverloadedLabels to the rescue! With them GHC only complains about being
-- unable to infer types. But with OverloadedLabels the problem is that you
-- have to write instances by hand. This is no longer a problem with this
-- TemplateHaskell module.
--
-- Usage:
--
-- @
-- -- Required language extensions
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE OverloadedLabels \#-}
-- {-\# LANGUAGE DataKinds \#-}
-- {-\# LANGUAGE FlexibleInstances \#-}
-- {-\# LANGUAGE MultiParamTypeClasses \#-}
--
-- import Data.OverloadedLabels.TH (generateLabels)
--
-- data MyRecord = MyRecord {foo :: Integer, bar :: String}
-- newtype OtherRecord = OtherRecord {bar :: String}
--
-- generateLabels ''MyRecord
-- generateLabels ''OtherRecord
--
-- main =
--     let x = MyRecord {foo = 0, bar = "hello"}
--         y = OtherRecord ", world"
--     in putStrLn $ \#bar x <> \#bar y
-- @
module Data.OverloadedLabels.TH
    ( generateLabels
    , generateLabelsUnderscore
    ) where

import Data.Maybe (catMaybes)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH


-- | Generate IsLabel instances for all record fields of the given data type
generateLabels :: Name -> Q [Dec]
generateLabels = generateLabelsWith GenerateSettings
    { fieldRename = Just
    }

-- | Generate IsLabel instances for all record fields starting with the
-- underscore. The generated labels don't have the starting underscore in them.
-- Useful when mixing with lenses.
generateLabelsUnderscore :: Name -> Q [Dec]
generateLabelsUnderscore = generateLabelsWith GenerateSettings
    { fieldRename = \x -> case x of
        '_':rest -> Just rest
        _        -> Nothing
    }


data GenerateSettings = GenerateSettings
    { fieldRename :: String -> Maybe String -- ^ Nothing means don't rename
    }


generateLabelsWith :: GenerateSettings -> Name -> Q [Dec]
generateLabelsWith GenerateSettings {fieldRename} name = reify name >>= \case
    TyConI (NewtypeD _ctx tname tvars _kind (RecC _rname vars) _deriv)
        -> makeLabels (fullyApplied (ConT tname) tvars) vars
--         -> makeLabels (ConT tname) vars

    TyConI (DataD    _ctx tname tvars _kind ctrs _deriv)
        -> case catMaybes . map toRecordVars $ ctrs of
            [] -> fail "Expected data declaration with at least one record"
            records ->
                let targetType = fullyApplied (ConT tname) tvars
                in concat <$> traverse (makeLabels targetType) records

    _   -> fail "Expected record declaration"

    where
        toRecordVars (RecC _ vars) = Just vars
        toRecordVars _             = Nothing
        --
        makeLabels :: Type -> [(Name, a, Type)] -> Q [Dec]
        makeLabels tname vars = catMaybes <$> traverse (toInstance tname) vars
        --
        toInstance :: Type -> (Name, a, Type) -> Q (Maybe Dec)
        toInstance tname (labelName, _bang, labelType) =
            let className = ConT ''IsLabel
            in case fieldRename . nameBase $ labelName of
                Nothing -> reportWarning ("Skipping field " <> show labelName)
                        >> pure Nothing
                Just labelString ->
                    let labelSymbol = LitT $ StrTyLit labelString
                        funcType = AppT (AppT ArrowT tname) labelType
                        instanceHead = AppT (AppT className labelSymbol) funcType
                        --
                        isLabelDec = FunD 'fromLabel [Clause [] (NormalB $ VarE labelName) []]
                    in pure . Just $ InstanceD Nothing [] instanceHead [isLabelDec]
        --
        fullyApplied :: Type -> [TyVarBndr] -> Type
        fullyApplied t []         = t
        fullyApplied t (var:vars) =
            let t' = AppT t $ varAsType var
            in fullyApplied t' vars
--         fullyApplied baseType vars = foldl' AppT baseType $ map varAsType vars
        varAsType (PlainTV name) = VarT name
        varAsType (KindedTV name kind) = SigT (VarT name) kind
