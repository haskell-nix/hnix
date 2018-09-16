{-# LANGUAGE DeriveGeneric #-}
module Nix.NixString (
    NixString
  , stringHasContext
  , hackyStringIgnoreContextMaybe
  , hackyStringIgnoreContext
  , hackyMakeNixStringWithoutContext
  , hackyModifyNixContents
  , hackyStringMappend
  , hackyStringMConcat
) where

import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Text (Text)
import           GHC.Generics
import           Data.Semigroup

{-# WARNING hackyStringMappend, hackyStringMConcat, hackyStringIgnoreContextMaybe, hackyStringIgnoreContext, hackyMakeNixStringWithoutContext, hackyModifyNixContents "This NixString function needs to be replaced" #-}

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

-- | Combine two NixStrings using mappend 
hackyStringMappend :: NixString -> NixString -> NixString
hackyStringMappend (NixString s1 t1) (NixString s2 t2) = NixString (s1 <> s2) (t1 <> t2)

-- | Combine NixStrings using mconcat 
hackyStringMConcat :: [NixString] -> NixString
hackyStringMConcat = foldr hackyStringMappend (NixString mempty mempty) 

--instance Semigroup NixString where
  --NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)

--instance Monoid NixString where
--  mempty = NixString mempty mempty
--  mappend = (<>)

-- | Extract the string contents from a NixString that has no context 
hackyStringIgnoreContextMaybe :: NixString -> Maybe Text
hackyStringIgnoreContextMaybe (NixString s c) | null c = Just s
                                | otherwise = Nothing 

-- | Extract the string contents from a NixString even if the NixString has an associated context 
hackyStringIgnoreContext :: NixString -> Text
hackyStringIgnoreContext (NixString s _) = s

-- | Returns True if the NixString has an associated context
stringHasContext :: NixString -> Bool
stringHasContext (NixString _ c) = not (null c)

-- | Constructs a NixString without a context
hackyMakeNixStringWithoutContext :: Text -> NixString
hackyMakeNixStringWithoutContext = flip NixString mempty 

-- | Modify the string part of the NixString -- ignores the context
hackyModifyNixContents :: (Text -> Text) -> NixString -> NixString
hackyModifyNixContents f (NixString s c) = NixString (f s) c 


