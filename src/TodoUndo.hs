{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

-- same as TodoUndo but uses an adjustable list internally
module TodoUndo (
  Todo(..)
  , TodoUndoConfig(..)
  , TodoUndo(..)
  , todoUndoConnect
  , holdTodo
) where

import           Relude
import           Relude.Extra.Map

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Data.Sequence
import Reflex.Data.Stack
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import Control.Exception (assert)

import qualified Data.List               as L
import qualified Data.Sequence as Seq
import Data.Foldable (foldrM)
import qualified Text.Show
import qualified Data.Text

-- helper methods
foldrWithIndexM :: forall a b m. (Monad m) =>  (Int -> a -> b -> m b) -> b -> Seq a -> m b
foldrWithIndexM f z xs = do
  -- internal fold function has type 'a -> (Int -> m b) -> m (Int -> m b)'
  r <- foldrM ((\x g -> return (\ !i -> g (i+1) >>= f i x))) (const (return z)) xs
  r 0


-- | reindexes indices such that each element is indexed as if all previous elements have been removed, O(n^2) lol
reindexForRemoval :: [(Int, a)] -> [(Int, a)]
reindexForRemoval [] = []
reindexForRemoval (r:xs) = r:reindexForRemoval rest where
  -- if this asserts that means you tried to remove the same index twice
  rest = map (\(x, a) -> assert (x /= fst r) $ if x > fst r then (x-1,a) else (x,a)) xs

-- TODO make sure this is correct lol
reindexForAddition :: [(Int, a)] -> [(Int, a)]
reindexForAddition = reverse . reindexForRemoval . reverse


data Todo = Todo {
  description :: Text
  , isDone    :: Bool
}

instance Show Todo where
  show (Todo d s) = Data.Text.unpack $ (if s then "t" else "f") <> d

-- each DynTodo element in the network has a dynamic var representing its state
-- note that it's possible to do a simpler first-order implementation where states are tracked in a separate dynamic
-- but for the purpose of this example, we want to do it using higher order frp
data DynTodo t = DynTodo {
  dtId     :: Int
  -- TODO make this dynamic
  , dtDesc     :: Text
  , dtIsDone :: Dynamic t Bool
}

data TodoUndoConfig t = TodoUndoConfig {
  -- input
  _trconfig_new              :: Event t Text
  , _trconfig_clearCompleted :: Event t ()
  , _trconfig_undo           :: Event t ()
  , _trconfig_redo           :: Event t ()
  -- TODO rename tick to toggle and get rid of untick
  , _trconfig_tick           :: Event t Int
  , _trconfig_untick         :: Event t Int

  , _trconfig_remove         :: Event t Int

  -- TODO
  --, _trconfig_modify :: Event t (Int, Text)
}

data TodoUndo t = TodoUndo {
  _tr_todos :: Dynamic t [Todo]
}


-- TODO actually use these or delete
data TodoUndoConnector t = TodoUndoConnector {
  _trconnector_todo_connector_tick :: Dynamic t [Todo] -> (Event t Int, Event t Int)
}

todoUndoConnect :: TodoUndoConnector t -> TodoUndo t -> TodoUndoConfig t -> TodoUndoConfig t
todoUndoConnect (TodoUndoConnector cx) (TodoUndo todos) trc = trc {
    _trconfig_tick = fst . cx $ todos
    , _trconfig_untick = snd . cx $ todos
  }

data TRAppCmd = TRAUndo | TRARedo
data TRCmd t = TRCNew (DynTodo t) | TRCDelete (Int, DynTodo t) | TRCClearCompleted | TRCTick Int | TRCUntick Int | TRCModify (Int, Text)

type UID = Int

holdTodo ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m, Adjustable t m)
  => TodoUndoConfig t
  -> m (TodoUndo t)
holdTodo TodoUndoConfig {..} = mdo
  let
    docmds = leftmostwarn "WARNING: received multiple commands at once" [
      -- construct element to put on
      fmap TRCNew $ pushAlways makeDynTodo _trconfig_new
      , fmap (const TRCClearCompleted) _trconfig_clearCompleted
      , fmap TRCTick _trconfig_tick
      , fmap TRCUntick _trconfig_untick
      , fmap TRCDelete $ pushAlways findDynTodo _trconfig_remove
      --, fmap TRCModify _trconfig_modify
      ]

    asc = ActionStackConfig {
        _actionStackConfig_do = docmds
        , _actionStackConfig_undo = _trconfig_undo
        , _actionStackConfig_redo = _trconfig_redo
        , _actionStackConfig_clear = never
      }

  as <- holdActionStack asc

  let
    doAction :: Event t (TRCmd t)
    doAction = _actionStack_do as
    undoAction :: Event t (TRCmd t)
    undoAction = _actionStack_undo as

    -- the only time we add a new element
    newEvSelect = \case
      TRCNew x -> Just x
      _ -> Nothing
    addNewEv = fmapMaybe newEvSelect doAction

    -- DynamicSeq event selectors
    -- --------------------------
    insert_do_push = \case
      TRCNew x -> do
        -- add to end
        s <- sample . current $ _dynamicSeq_contents todosDyn
        return $ Just (Seq.length s, Seq.singleton x)
      _ -> return Nothing
    insert_do_ev = push insert_do_push doAction

    insert_undo_push = \case
      -- put back element we just removed
      TRCDelete (n,x) -> return $ Just (n, Seq.singleton x)
      _ -> return Nothing
    insert_undo_ev = push insert_undo_push undoAction

    -- remove an element, note the snd tuple arg is needed for Undo so we ignore it
    remove_do_push = \case
      TRCDelete (n, _) -> return $ Just (n, 1)
      _ -> return Nothing
    remove_do_ev = push remove_do_push doAction

    remove_undo_push = \case
      TRCNew _ -> do
        -- remove from end
        s <- sample . current $ _dynamicSeq_contents todosDyn
        return $ Just (Seq.length s - 1, 1)
      _ -> return Nothing
    remove_undo_ev = push remove_undo_push undoAction

    -- DynamicSeq repeated event selectors
    -- --------------------------
    select_TRCClearCompleted = \case
      TRCClearCompleted -> Just ()
      _ -> Nothing
    clear_do_ev' = fmapMaybe select_TRCClearCompleted doAction
    clear_undo_ev' = fmapMaybe select_TRCClearCompleted undoAction
    clear_do_push _ = do
      s <- sample . current $ _dynamicSeq_contents todosDyn
      let
        foldfn :: Int -> DynTodo t -> [(Int, DynTodo t)] -> PushM t [(Int, DynTodo t)]
        foldfn i dyntodo xs = do
          done <- sample . current $ dtIsDone dyntodo
          if done
            then return $ (i,dyntodo):xs
            else return $ xs
      foldrWithIndexM foldfn [] s
    clear_do_ev :: Event t [(Int, DynTodo t)]
    clear_do_ev = pushAlways clear_do_push clear_do_ev'
    clear_undo_ev :: Event t ()
    clear_undo_ev = clear_undo_ev'

    -- DynTodo event selectors
    -- --------------------------
    -- maps tick/untick list index to Todo identifier
    -- we just toggle on do/undo so we don't need to distinguish between do and undo
    tickDoUndoPushSelect :: TRCmd t -> PushM t (Maybe UID)
    tickDoUndoPushSelect = let
        toggleFn index = do
          tds <- sample . current $ _dynamicSeq_contents todosDyn
          return . Just . dtId $ Seq.index tds index
      in
        \case
          TRCTick index -> toggleFn index
          TRCUntick index -> toggleFn index
          _ -> return Nothing where

    makeDynTodo :: Text -> PushM t (DynTodo t)
    makeDynTodo s = do
      !uid <- sample . current $ uidDyn
      let
        -- only toggle if uid of ticked element matches our own
        cffn uid' = if uid' == uid then Just () else Nothing

      -- TODO this leaks
      -- TODO switch to fan
      doneState <- toggle False
        (fmapMaybe cffn . leftmost . fmap (push tickDoUndoPushSelect) $ [doAction, undoAction])

      return DynTodo {
          dtDesc = s
          , dtId = uid
          , dtIsDone = doneState
        }

    -- should be OK as this is similar to `attach` and not `attachPromptly`
    findDynTodo :: Int -> PushM t (Int, DynTodo t)
    findDynTodo index = do
      tds <- sample . current . _dynamicSeq_contents $ todosDyn
      return $ (index, Seq.index tds index)


  -- create id assigner
  -- --------------------------
  -- TODO switch this to use DirectoryIdAssigner
  uidDyn :: Dynamic t UID <-
    foldDyn (+) 0 (fmap (const 1) addNewEv)

  -- create clear completed stack
  -- ----------------------

  -- TODO this delay thing causes leaks
  -- clearing will also likely cause _actionStack_clear to trigger so we delay it by 1 frame
  delayedClear <- sequenceEvents clear_do_ev (_actionStack_clear as)
  let
    clearedStackConfig = DynamicStackConfig {
        _dynamicStackConfig_push = clear_do_ev
      , _dynamicStackConfig_pop = clear_undo_ev
      , _dynamicStackConfig_clear = delayedClear
    }
  clearedStack :: DynamicStack t [(Int, DynTodo t)]
    <- holdDynamicStack [] clearedStackConfig
  remove_many_ev' :: Event t (Int, DynTodo t) <- repeatEvent $ fmap reindexForRemoval $ _dynamicStack_pushed clearedStack
  add_many_ev' :: Event t (Int, DynTodo t) <- repeatEvent $ fmap reindexForAddition $ _dynamicStack_popped clearedStack
  let
    remove_many_ev :: Event t (Int, Int)
    remove_many_ev = fmap (\(i,_) -> (i,1)) remove_many_ev'
    add_many_ev :: Event t (Int, Seq (DynTodo t))
    add_many_ev = fmap (\(i,e) -> (i,Seq.singleton e)) add_many_ev'


  -- create DynamicSeq
  -- ----------------------
  let
    dsc = DynamicSeqConfig {
        _dynamicSeqConfig_insert   = leftmost [insert_do_ev, insert_undo_ev, add_many_ev]
        , _dynamicSeqConfig_remove = leftmost [remove_do_ev, remove_undo_ev, remove_many_ev]
        , _dynamicSeqConfig_clear  = never
      }

  todosDyn :: DynamicSeq t (DynTodo t)
    <- holdDynamicSeq Seq.empty dsc

  -- assemble the final behavior
  ------------------------------
  let
    contents :: Dynamic t [DynTodo t]
    contents = toList <$> _dynamicSeq_contents todosDyn
    descriptions :: Dynamic t [Text]
    descriptions = dtDesc <<$>> contents

    -- TODO this leaks
    doneStates :: Dynamic t [Bool]
    doneStates = join . fmap sequence $ dtIsDone <<$>> contents

  return $ TodoUndo $ ffor2 descriptions doneStates (zipWith Todo)
