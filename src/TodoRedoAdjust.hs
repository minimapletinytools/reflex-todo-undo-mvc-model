{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

-- same as TodoRedo but uses an adjustable list internally
module TodoRedoAdjust (
  Todo(..)
  , TodoRedoConfig(..)
  , TodoRedo(..)
  , todoRedoConnect
  , holdTodo
) where

import           Relude
import           Relude.Extra.Map

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Data.List
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import qualified Data.List               as L

data Todo = Todo {
  description :: Text
  , isDone    :: Bool
} deriving Show

-- each DynTodo element in the network has a dynamic var representing its state
-- note that it's possible to do a simpler first-order implementation where states are tracked in a separate dynamic
-- but for the purpose of this example, we want to do it using higher order frp
data DynTodo t = DynTodo {
  dtDesc     :: Text
  , dtId     :: Int
  , dtIsDone :: Dynamic t Bool
}

data TodoRedoConfig t = TodoRedoConfig {
  -- input
  _trconfig_new              :: Event t Text
  , _trconfig_clearCompleted :: Event t () -- NOT IMPLEMENTED!!!
  , _trconfig_undo           :: Event t ()
  , _trconfig_redo           :: Event t ()
  , _trconfig_tick           :: Event t Int
  , _trconfig_untick         :: Event t Int
  , _trconfig_remove         :: Event t Int
}

data TodoRedo t = TodoRedo {
  _tr_todos :: Dynamic t [Todo]
}

data TodoRedoConnector t = TodoRedoConnector {
  _trconnector_todo_connector_tick :: Dynamic t [Todo] -> (Event t Int, Event t Int)
}

todoRedoConnect :: TodoRedoConnector t -> TodoRedo t -> TodoRedoConfig t -> TodoRedoConfig t
todoRedoConnect (TodoRedoConnector cx) (TodoRedo todos) trc = trc {
    _trconfig_tick = fst . cx $ todos
    , _trconfig_untick = snd . cx $ todos
  }

-- TODO change to TRCClearCompleted  [DynTodo t]
data TRAppCmd = TRAUndo | TRARedo
data TRCmd t = TRCNew (DynTodo t) | TRCDelete (Int, DynTodo t) | TRCClearCompleted | TRCTick Int | TRCUntick Int

type UID = Int

holdTodo ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, PostBuild t m)
  => TodoRedoConfig t
  -> m (TodoRedo t)
holdTodo TodoRedoConfig {..} = mdo
  let
    docmds = leftmostwarn "WARNING: received multiple commands at once" [
      -- construct element to put on
      fmap TRCNew $ pushAlways makeDynTodo _trconfig_new
      , fmap (const TRCClearCompleted) _trconfig_clearCompleted
      , fmap TRCTick _trconfig_tick
      , fmap TRCUntick _trconfig_untick
      , fmap TRCDelete $ pushAlways findDynTodo _trconfig_remove
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

    -- undoing a TRCNew is just popping top element
    popUndoSelect = \case
      TRCNew x -> Just x
      _ -> Nothing
    popUndoEv = fmapMaybe popUndoSelect undoAction

    -- the only time we add a new element
    addDoSelect = \case
      TRCNew x -> Just x
      _ -> Nothing
    addNewEv = fmapMaybe addDoSelect doAction

    -- put back element we removed
    addUndoSelect = \case
      TRCDelete x -> Just x
      _ -> Nothing

    -- take element that was removed and put it back on
    pushUndoSelect = \case
      TRCNew x -> Just x
      _ -> Nothing

    -- remove an element, note the snd tuple arg is needed for Undo so we ignore it
    removeDoSelect = \case
      TRCDelete (n, _) -> Just n
      _ -> Nothing

    -- maps tick/untick list index to Todo identifier
    -- we just toggle on do/undo so we don't need to distinguish between do and undo
    tickDoUndoPushSelect :: TRCmd t -> PushM t (Maybe UID)
    tickDoUndoPushSelect = let
        toggleFn index = do
          tdl <- sample . current $ _dynamicList_contents todosDyn
          return . Just . dtId $ tdl L.!! (length tdl - index - 1)
      in
        \case
          -- TODO switch to guards to remove copypasta
          TRCTick index -> toggleFn index
          TRCUntick index -> toggleFn index
          _ -> return Nothing where


    makeDynTodo :: Text -> PushM t (DynTodo t)
    makeDynTodo s = do
      uid <- sample . current $ uidDyn
      let
        -- only toggle if uid of ticked element matches our own
        cffn uid' = if uid' == uid then Just () else Nothing
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
      tdl <- sample . current . _dynamicList_contents $ todosDyn
      return $ (index, tdl L.!! (length tdl - index - 1))

    dlc = defaultDynamicListConfig {
        _dynamicListConfig_add = fmapMaybe addUndoSelect undoAction
        , _dynamicListConfig_remove = fmapMaybe removeDoSelect doAction
        , _dynamicListConfig_push = leftmost [addNewEv, fmapMaybe pushUndoSelect doAction]
        , _dynamicListConfig_pop = void popUndoEv
      }

  uidDyn :: Dynamic t UID <-
    foldDyn (+) 0 (fmap (const 1) addNewEv)


  --[H,A,B]   simpleList  ::                Dynamic       [v] -> (     Dynamic v -> m        a ) -> m (Dynamic       [a])

  -- TODO change to Dynamic t (Map Int DynTodo)
  -- note that internal representation is in reverse order
  todosDyn :: DynamicList t (DynTodo t)
    <- holdDynamicList [] dlc

  -- assemble the final behavior
  let
    contents :: Dynamic t [DynTodo t]
    contents = _dynamicList_contents todosDyn

    descriptions :: Dynamic t [Text]
    descriptions = dtDesc <<$>> contents

    doneStates :: Dynamic t [Bool]
    doneStates = join . fmap sequence $ dtIsDone <<$>> contents

  return $ TodoRedo . fmap reverse $ ffor2 descriptions doneStates (zipWith Todo)
