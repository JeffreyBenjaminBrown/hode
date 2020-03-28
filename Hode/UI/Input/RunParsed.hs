{-# LANGUAGE RankNTypes #-}

module Hode.UI.Input.RunParsed (
    parseAndRunCommand -- ^            St ->
                       -- B.EventM BrickName (B.Next St)
  , runParsedCommand   -- ^ Command -> St ->
                       -- Either String (B.EventM BrickName (B.Next St))
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.List.PointedList as P
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Lens.Micro
import           System.Directory

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Brick.Widgets.Edit    as B

import Hode.Hash.Lookup
import Hode.PTree.Initial
import Hode.Rslt.Edit
import Hode.Rslt.Files
import Hode.Rslt.Types
import Hode.UI.ExprTree
import Hode.UI.CycleBuffer
import Hode.UI.Util
import Hode.UI.Input.Parse
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


parseAndRunCommand ::
  St -> B.EventM BrickName (B.Next St)
parseAndRunCommand st =
  let cmd :: String = unlines $ B.getEditContents
                      $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left parseErr ->
      B.continue $ unEitherSt st $ Left parseErr
    Right (parsedCmd :: Command) ->
      case runParsedCommand parsedCmd st of
        Left runErr ->
          B.continue $ unEitherSt st $ Left runErr
        Right (evNextSt :: B.EventM BrickName (B.Next St)) ->
          (fmap $ fmap $ commandHistory %~ (:) parsedCmd)
          evNextSt
        -- PITFALL: Don't call `unEitherSt` on this `evNextSt`, because
        -- it might be showing errors, because the load and save commnads
        -- must return Right in order to perform IO.

-- | Pitfall: this looks like it could just return `St` rather
-- than `EventM ... St`, but it needs IO to load and save.
-- (If I really want to keep it pure I could add a field in St
-- that keeps a list of actions to execute.)
runParsedCommand ::
  Command -> St ->
  Either String (B.EventM BrickName (B.Next St))
runParsedCommand                      c0 st0 =
  prefixLeft "runParsedCommand:" $ g' c0 st0
  where

  itWorked :: String -> St -> St
  itWorked s = showReassurance s
               . (showingInMainWindow .~ SearchBuffer)

  g' c st =
    let err = "There is currently at least one cycle in what should be transitive relationships (see the Cycle Buffer). Until all such cycles are eliminated, the following features are disabled:" ++
              "\n* nmoving an expression to another address" ++
              "\n* replacing subexpressions" ++
              "\n* adding to the graph"
    in case st ^. blockingCycles of
      Just (_:_) ->
        case c of
          CommandInsert _    -> Right $ B.continue $ st & showError err
          CommandReplace _ _ -> Right $ B.continue $ st & showError err
          CommandMove _ _    -> Right $ B.continue $ st & showError err
          _ -> g c st
      _     -> g c st

  -- todo ? duplicative of the clause for CommandInsert
  g (CommandReplace a e) st = do
    (r :: Rslt, cs :: [Cycle]) <-
      replaceExpr a e (st ^. appRslt)
    let st1 = st & appRslt .~ r
    if null cs
      then Right . B.continue $ st1
           & itWorked ( "Replaced Expr at "
                        ++ show a ++ "." )
      else B.continue <$> updateCycleBuffer
           ( st1 & blockingCycles .~ Just cs )

  g (CommandMove old new) st = do
    r <- moveRefExpr old new (st ^. appRslt)
    Right $ B.continue $ st
      & appRslt .~ r
      & itWorked ( "Moved Expr from " ++ show old
                   ++ " to " ++ show new ++ "." )

  g (CommandDelete a) st = do
    r <- deleteIfUnused a (st ^. appRslt)
    Right $ B.continue $ st & appRslt .~ r
      & itWorked ( "Deleted Expr at "
                   ++ show a ++ "." )

  -- todo ? duplicative of the clause for CommandReplace
  g (CommandInsert e) st = do
    (r :: Rslt, as :: [Aged Addr], cs :: [Cycle]) <-
      exprToAddrInsert (st ^. appRslt) e
    let st1 = st & appRslt .~ r
    if null cs
      then Right $ B.continue $ st1
           & itWorked ( "Expr(s) added at " ++
                        show (catNews as) )
      else B.continue <$> updateCycleBuffer
           ( st1 & blockingCycles .~ Just cs )

  g (CommandLoad f) st = Right $ do
    (bad :: Bool) <-
      liftIO $ not <$> doesDirectoryExist f
    if bad
      then B.continue $ st
           & showError ("Non-existent folder: " ++ f)
      else do
      r <- liftIO $ readRslt f
      B.continue $ st & appRslt .~ r
        & itWorked "Rslt loaded."

  g (CommandSave f) st = Right $ do
    bad :: Bool <- liftIO $ not <$> doesDirectoryExist f
    st' <- if bad
      then return $ st & showError
           ("Non-existent folder: " ++ f)
      else do
      liftIO $ writeRslt f $ st ^. appRslt
      return $ st & itWorked "Rslt saved."
    B.continue st'

  g (CommandFind s h) st =
    prefixLeft "called to find: " $ do
    let r :: Rslt = st ^. appRslt
    as :: [Addr] <-
      S.toList <$> hExprToAddrs r mempty h
    pr :: Porest ExprRow <-
      (P.focus . pTreeHasFocus .~ True)
      <$> addrsToExprRows st mempty as
    Right $ B.continue $ st
      -- PITFALL : Replaces the Buffer's old contents.
      -- TODO ? Create a new Buffer instead.
      & ( stSet_focusedBuffer . bufferExprRowTree .~
          PTree { _pTreeLabel =
                  exprRow_fromQuery $ QueryView $
                  T.unpack . T.strip . T.pack $ s
                , _pTreeHasFocus = False
                , _pMTrees = Just pr } )
      & itWorked "Search successful."

  g (CommandSort _ bo t) st =
    prefixLeft "called to sort:" $ do
    st' <- sortFocusAndPeers (bo,t) st
    Right $ B.continue $ st'
      & itWorked "Sort successful."
