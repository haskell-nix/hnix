{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Render where

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Nix.Expr.Types.Annotated
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos (SourcePos(..))

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

posAndMsg :: SourcePos -> Doc a-> ParseError t Void
posAndMsg beg msg =
    FancyError (beg :| [])
        (Set.fromList [ErrorFail (show msg) :: ErrorFancy Void])

renderLocation :: MonadFile m => SrcSpan -> Doc a -> m (Doc a)
renderLocation (SrcSpan beg@(SourcePos "<string>" _ _) _) msg =
    return $ pretty $ init $ parseErrorPretty @Char (posAndMsg beg msg)

renderLocation (SrcSpan beg@(SourcePos path _ _) _) msg = do
    contents <- Nix.Render.readFile path
    return $ pretty $ init $ parseErrorPretty' contents (posAndMsg beg msg)
