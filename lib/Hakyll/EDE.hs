module Hakyll.EDE
  ( edeTemplateCompiler,
    loadAndApplyEdeTemplate,
    applySelfEdeTemplate,
    applySelfDataEdeTemplate,
  )
where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as SB.Char8
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as SM
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Hakyll
import Hakyll.EDE.Context
import Helper
import qualified Text.EDE as E

applySelfDataEdeTemplate :: Compiler (Item String)
applySelfDataEdeTemplate = do
  doc <- getUnderlying
  meta <- getMetadata doc
  datas <- liftMaybe "No data specified in metadata" $ KeyMap.lookup (Key.fromString "data") meta >>= getObject
  let dataMap = KeyMap.mapMaybe getString datas
  let (mapKeys, paths) = unzip (KeyMap.toList dataMap)
  let loaded = fmap (\path -> loadBody (fromFilePath path) :: Compiler Value) paths
  parsed <- sequence loaded
  let zipped = zip (fmap Key.toText mapKeys) parsed
  content <- getResourceBody
  applyAsEdeTemplate (SM.fromList zipped) content

applySelfEdeTemplate :: SM.HashMap ST.Text Value -> Compiler (Item String)
applySelfEdeTemplate ctx = getResourceString >>= applyAsEdeTemplate ctx

applyAsEdeTemplate :: SM.HashMap ST.Text Value -> Item String -> Compiler (Item String)
applyAsEdeTemplate ctx content = do
  name <- toFilePath <$> getUnderlying
  template <- E.eitherParseWith E.defaultSyntax compilerResolver (ST.pack name) (SB.Char8.pack $ itemBody content) >>= liftEither
  text <- liftEither $ E.eitherRender template ctx
  makeItem $ LT.unpack text

loadAndApplyEdeTemplate :: Identifier -> EdeContext a -> Item a -> Compiler (Item String)
loadAndApplyEdeTemplate name ctx item = do
  template <- itemBody <$> loadEdeTemplate name
  (datactx, functx) <- unEdeContext ctx item
  result <- liftEither $ E.eitherRenderWith functx template datactx
  makeItem $ LT.unpack result

edeTemplateCompiler :: Compiler (Item LB.ByteString)
edeTemplateCompiler = getResourceLBS

loadEdeTemplate :: Identifier -> Compiler (Item E.Template)
loadEdeTemplate name = do
  content <- loadBody name
  template <- E.eitherParseWith E.defaultSyntax compilerResolver (ST.pack $ toFilePath name) (LB.toStrict content) >>= liftEither
  return $ Item name template

compilerResolver :: E.Resolver Compiler
compilerResolver _config name _delta = E.Success . itemBody <$> loadEdeTemplate (fromFilePath $ ST.unpack name)
