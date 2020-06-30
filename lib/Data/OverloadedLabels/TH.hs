{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
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
    ) where

import Data.Maybe (catMaybes)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH


-- | Generate IsLabel instances for all record fields of the given data type
generateLabels :: Name -> Q [Dec]
generateLabels name = reify name >>= \case
    TyConI (NewtypeD _ctx tname _tvars _kind (RecC _rname vars) _deriv)
        -> makeLabels (ConT tname) vars

    TyConI (DataD    _ctx tname _tvars _kind ctrs _deriv)
        -> case catMaybes . map toRecordVars $ ctrs of
            [] -> fail "Expected data declaration with at least one record"
            records -> concat <$> traverse (makeLabels (ConT tname)) records

    _   -> fail "Expected record declaration"

    where
        toRecordVars (RecC _ vars) = Just vars
        toRecordVars _             = Nothing
        --
        makeLabels :: Type -> [(Name, a, Type)] -> Q [Dec]
        makeLabels tname vars = traverse (toInstance tname) vars
        toInstance tname (labelName, _bang, labelType) =
            let className = ConT ''IsLabel
                labelString = nameBase labelName
                labelSymbol = LitT $ StrTyLit labelString
                funcType = AppT (AppT ArrowT tname) labelType
                instanceHead = AppT (AppT className labelSymbol) funcType
                --
                isLabelDec = FunD 'fromLabel [Clause [] (NormalB $ VarE labelName) []]
            in pure $ InstanceD Nothing [] instanceHead [isLabelDec]
