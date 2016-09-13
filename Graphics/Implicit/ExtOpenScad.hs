-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad (runOpenscad, OVal (..) ) where

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(..), LanguageOpts(..), Message)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (origParseProgram)
import Graphics.Implicit.ExtOpenScad.Parser.AltStatement (altParseProgram)
import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)
import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import qualified Text.Parsec.Error as Parsec (ParseError)
import qualified Control.Monad as Monad (mapM_)
import qualified Control.Monad.State as State (runStateT)
import qualified System.Directory as Dir (getCurrentDirectory)

-- Small wrapper to handle parse errors, etc.
runOpenscad :: LanguageOpts -> String -> ([String], Maybe (IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])))
runOpenscad languageOpts s =
    let
        initial = defaultObjects
        rearrange (_, (varlookup, ovals, _ , _ , messages)) = (varlookup, obj2s, obj3s, messages) where
                                  (obj2s, obj3s, _ ) = divideObjs ovals
        parseProgram = if alternateParser languageOpts then altParseProgram else origParseProgram
    in case parseProgram "" s of
        Left e -> ([show e], Nothing)
        Right sts -> ([], Just
            $ fmap rearrange
            $ (\sts -> do
                path <- Dir.getCurrentDirectory
                State.runStateT sts (initial, [], path, languageOpts, [] )
            )
            $ Monad.mapM_ runStatementI sts)
