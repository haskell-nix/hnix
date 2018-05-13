{-# LANGUAGE DeriveGeneric #-}
module Nix.NixString (
    stringNoContext
  , stringContextOnly
  , stringWithContext
  , stringIntentionallyDropContext
  , NixString
  , makeNixStringWithoutContext
  , makeNixString
  , modifyNixContents
) where

import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Text (Text)
import           GHC.Generics
import           Data.Semigroup

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor = 
    DirectPath
  | DerivationOutput !Text
  | AllDerivationOutputs
  deriving (Show, Eq, Ord, Generic)

instance Hashable ContextFlavor

-- | A 'StringContext' ...
data StringContext = 
  StringContext { scPath :: !Text
                , scFlavor :: !ContextFlavor
                } deriving (Eq, Ord, Show, Generic) 

instance Hashable StringContext

data NixString = NixString 
  { nsContents :: !Text
  , nsContext :: !(S.HashSet StringContext)
  } deriving (Eq, Ord, Show, Generic) 

instance Hashable NixString

instance Semigroup NixString where
  NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)

instance Monoid NixString where
  mempty = NixString mempty mempty
  mappend = (<>)

stringNoContext :: NixString -> Maybe Text
stringNoContext (NixString s c) | null c = Just s
                                | otherwise = Nothing 

stringIntentionallyDropContext :: NixString -> Text
stringIntentionallyDropContext (NixString s _) = s

stringContextOnly :: NixString -> S.HashSet StringContext
stringContextOnly (NixString _ c) = c 

stringWithContext :: NixString -> (Text, S.HashSet StringContext)
stringWithContext (NixString s d) = (s, d)

makeNixString :: Text -> S.HashSet StringContext -> NixString
makeNixString = NixString

makeNixStringWithoutContext :: Text -> NixString
makeNixStringWithoutContext = flip NixString mempty 

modifyNixContents :: (Text -> Text) -> NixString -> NixString
modifyNixContents f (NixString s c) = NixString (f s) c 


