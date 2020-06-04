{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import qualified Data.Aeson                  as Aeson
import           Data.Foldable
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.Map.Strict             as M
import           Data.Proxy
import           Data.String                 (fromString)
import qualified Data.Text                   as Text
import qualified Generics.SOP                as SOP
import           GHC.Generics
import           GHC.TypeLits                (KnownNat, KnownSymbol, Nat,
                                              Symbol, natVal, symbolVal)

import qualified Language.Elm.Expression     as Expression
import qualified Language.Elm.Name           as Name
import qualified Language.Elm.Pretty         as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Type           as Type
import           Language.Haskell.To.Elm

-- Helpers for deriving typeclasses
newtype DeriveT
  (fieldLabelDrop :: Nat)
  (constructorLabelDrop :: Nat)
  (allNullaryToStringTag :: Bool)
  (omitNothing :: Bool)
  (unwrapUnaryRecords :: Bool)
  (tagSingleConstructors :: Bool)
  (sumEncoding :: SimpleSumEncoding)
  (name :: Symbol)
  a
  = DeriveT a

class BoolVal (a :: Bool) where
    boolVal :: Proxy a -> Bool

instance BoolVal 'False where
    boolVal _ = False

instance BoolVal 'True where
    boolVal _ = True

data SimpleSumEncoding
    = TaggedObject
    | UntaggedValue
    | ObjectWithSingleField
    | TwoElemArray

class SumEncodingVal (a :: SimpleSumEncoding) where
    sumEncodingVal :: Proxy a -> Aeson.SumEncoding

instance SumEncodingVal 'TaggedObject where
    sumEncodingVal _ = Aeson.TaggedObject "dsq" "fsdsdfkl"
instance SumEncodingVal 'UntaggedValue where
    sumEncodingVal _ = Aeson.UntaggedValue
instance SumEncodingVal 'ObjectWithSingleField where
    sumEncodingVal _ = Aeson.ObjectWithSingleField
instance SumEncodingVal 'TwoElemArray where
    sumEncodingVal _ = Aeson.TwoElemArray

aesonOptions
  :: forall
      fieldLabelDrop
      constructorLabelDrop
      allNullaryToStringTag
      omitNothing
      unwrapUnaryRecords
      tagSingleConstructors
      sumEncoding
      name
      a.
     ( KnownNat fieldLabelDrop
     , KnownNat constructorLabelDrop
     , BoolVal allNullaryToStringTag
     , BoolVal omitNothing
     , BoolVal unwrapUnaryRecords
     , BoolVal tagSingleConstructors
     , SumEncodingVal sumEncoding
     )
  => DeriveT
      fieldLabelDrop
      constructorLabelDrop
      allNullaryToStringTag
      omitNothing
      unwrapUnaryRecords
      tagSingleConstructors
      sumEncoding
      name
      a
  -> Aeson.Options
aesonOptions _ =
  Aeson.defaultOptions
  { Aeson.fieldLabelModifier = drop (fromInteger (natVal (Proxy @fieldLabelDrop)))
  , Aeson.constructorTagModifier = drop (fromInteger (natVal (Proxy @constructorLabelDrop)))
  , Aeson.allNullaryToStringTag = boolVal (Proxy @allNullaryToStringTag)
  , Aeson.omitNothingFields = boolVal (Proxy @omitNothing)
  , Aeson.unwrapUnaryRecords = boolVal (Proxy @unwrapUnaryRecords)
  , Aeson.tagSingleConstructors = boolVal (Proxy @tagSingleConstructors)
  , Aeson.sumEncoding = sumEncodingVal (Proxy @sumEncoding)
  }

instance
  ( Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , KnownNat fieldLabelDrop
  , KnownNat constructorLabelDrop
  , BoolVal allNullaryToStringTag
  , BoolVal omitNothing
  , BoolVal unwrapUnaryRecords
  , BoolVal tagSingleConstructors
  , SumEncodingVal sumEncoding
  ) =>
  Aeson.ToJSON
    (DeriveT
      fieldLabelDrop
      constructorLabelDrop
      allNullaryToStringTag
      omitNothing
      unwrapUnaryRecords
      tagSingleConstructors
      sumEncoding
      name
      a)
  where
  toJSON dt@(DeriveT a) =
    Aeson.genericToJSON (aesonOptions dt) a

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  , KnownNat fieldLabelDrop
  , KnownNat constructorLabelDrop
  , BoolVal allNullaryToStringTag
  , BoolVal omitNothing
  , BoolVal unwrapUnaryRecords
  , BoolVal tagSingleConstructors
  , SumEncodingVal sumEncoding
  ) =>
  Aeson.FromJSON
    (DeriveT
      fieldLabelDrop
      constructorLabelDrop
      allNullaryToStringTag
      omitNothing
      unwrapUnaryRecords
      tagSingleConstructors
      sumEncoding
      name
      a)
  where
  parseJSON = fmap DeriveT . 
    Aeson.genericParseJSON (aesonOptions dt)

instance
  (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), KnownSymbol name) =>
  HasElmType (DeriveT
                  fieldLabelDrop
                  constructorLabelDrop
                  allNullaryToStringTag
                  omitNothing
                  unwrapUnaryRecords
                  tagSingleConstructors
                  sumEncoding
                  name
                  a)
  where
  elmDefinition =
    Just
      $ deriveElmTypeDefinition @a defaultOptions {fieldLabelModifier = dropWhile (== '_')}
      $ fromString $ symbolVal $ Proxy @name

instance
  ( SOP.HasDatatypeInfo a
  , HasElmType a
  , SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code a)
  , HasElmType (DeriveT
    fieldLabelDrop
    constructorLabelDrop
    allNullaryToStringTag
    omitNothing
    unwrapUnaryRecords
    tagSingleConstructors
    sumEncoding
    name
    a)
  , KnownSymbol name) =>
  HasElmDecoder Aeson.Value (DeriveT
                              fieldLabelDrop
                              constructorLabelDrop
                              allNullaryToStringTag
                              omitNothing
                              unwrapUnaryRecords
                              tagSingleConstructors
                              sumEncoding
                              name
                              a)
  where
  elmDecoderDefinition =
    Just
      $ deriveElmJSONDecoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Decoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name

instance
  ( SOP.HasDatatypeInfo a
  , HasElmType a
  , SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code a)
  , HasElmType (DeriveT
    fieldLabelDrop
    constructorLabelDrop
    allNullaryToStringTag
    omitNothing
    unwrapUnaryRecords
    tagSingleConstructors
    sumEncoding
    name
    a)
  , KnownSymbol name) =>
  HasElmEncoder Aeson.Value (DeriveT
                              fieldLabelDrop
                              constructorLabelDrop
                              allNullaryToStringTag
                              omitNothing
                              unwrapUnaryRecords
                              tagSingleConstructors
                              sumEncoding
                              name
                              a)
  where
  elmEncoderDefinition =
    Just
      $ deriveElmJSONEncoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Encoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name

data Record1 a = Record1 { _r1foo :: Int, _r1bar :: Maybe Int, _r1baz :: a, _r1qux :: Maybe a, _r1jmap :: M.Map String Int } deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
data Record2 a = Record2 { _r2foo :: Int, _r2bar :: Maybe Int, _r2baz :: a, _r2qux :: Maybe a } deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance (HasElmType a, HasElmType b) => HasElmType (M.Map a b) where

data SumTest a
    = SumA a
    | SumB (Maybe a)
    | SumC a a
    | SumD { _s01foo :: a }
    | SumE { _s01bar :: Int, _s01baz :: Int }
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (Aeson.ToJSON, Aeson.FromJSON, HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via DeriveT 4 3 'True 'False 'True 'True 'UntaggedValue "SumTest" (SumTest a)


main :: IO ()
main = do
  let
    definitions =
      Simplification.simplifyDefinition <$>
        jsonDefinitions @SumTest

    modules =
      Pretty.modules definitions
  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
