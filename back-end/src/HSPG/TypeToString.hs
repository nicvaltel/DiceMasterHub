{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HSPG.TypeToString where

import Language.Haskell.TH

newtype UserId (r :: RegStatus) = UserId Int
data RegStatus = Registered | Anonim

-- -- Generate the getRegStatus function using Template Haskell
-- $(do
--   let getRegStatusExp Registered = ConE 'Registered
--       getRegStatusExp Anonim = ConE 'Anonim

--   clauses <- traverse (\r -> do
--     let pat = ConP r []
--     let body = NormalB $ getRegStatusExp r
--     return $ Clause [pat] body []) [Registered, Anonim]

--   let getRegStatusDef = FunD (mkName "getRegStatus") clauses
--   return [getRegStatusDef]
--  )