{-# LANGUAGE DeriveGeneric #-}
module Nix.NixString (
    NixString
  , hackyStringHasContext
  , hackyStringIgnoreContextMaybe
  , hackyStringIgnoreContext
  , makeNixStringWithoutContext
  , modifyNixContents
) where

import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Text (Text)
import           GHC.Generics
import           Data.Semigroup

{-# WARNING hackyStringHasContext, hackyStringIgnoreContextMaybe, hackyStringIgnoreContext "This NixString function needs to be replaced" #-}

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor = 
    DirectPath
  | DerivationOutput !Text
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

hackyStringIgnoreContextMaybe :: NixString -> Maybe Text
hackyStringIgnoreContextMaybe (NixString s c) | null c = Just s
                                | otherwise = Nothing 

hackyStringIgnoreContext :: NixString -> Text
hackyStringIgnoreContext (NixString s _) = s

hackyStringHasContext :: NixString -> Bool
hackyStringHasContext = const False

--stringWithContext :: NixString -> (Text, S.HashSet StringContext)
--stringWithContext (NixString s d) = (s, d)

makeNixStringWithoutContext :: Text -> NixString
makeNixStringWithoutContext = flip NixString mempty 

modifyNixContents :: (Text -> Text) -> NixString -> NixString
modifyNixContents f (NixString s c) = NixString (f s) c 


