module Grunt.Component.App (component) where

import Prelude

import Component.View.Layout as Layout
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Grunt.Api as Api
import Grunt.Component.Tabs as Tabs
import Grunt.Component.TextInput as TextInput
import Grunt.Component.Utils (css)
import Grunt.Component.View.Article as Article
import Grunt.Component.View.FeedEntryList as FeedEntryList
import Grunt.Component.View.FeedList as FeedList
import Grunt.Types (Entry(..), EntryId, FeedId, Subscription(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State
  = { subscriptions :: Array Subscription
    , hiddenSubscriptions :: Set FeedId
    , unread :: Set EntryId
    , starred :: Set EntryId
    , currentEntry :: Maybe Entry
    , currentTab :: TabState
    }

data TabState
  = EntriesState { entries :: Array Entry, page :: Int, reachedEnd :: Boolean }
  | StarredState { entries :: Array Entry, page :: Int, reachedEnd :: Boolean }
  | SubscriptionState

data SideTab
  = EntriesTab
  | StarredTab
  | SubscriptionTab

derive instance Eq SideTab

instance Show SideTab where
  show = case _ of
    EntriesTab -> "Entries"
    StarredTab -> "Starred"
    SubscriptionTab -> "Feeds"

allTabs :: NonEmptyArray SideTab
allTabs = NEA.fromNonEmpty $ NonEmpty EntriesTab [ StarredTab, SubscriptionTab ]

data Action
  = Initialize
  | LoadNextPage
  | SelectedEntry Entry
  | StarEntry EntryId
  | UnstarEntry EntryId
  | MarkAsRead EntryId
  | SelectedTab SideTab
  | AddSubscription
  | DeleteSubscription FeedId
  | HideSubscription FeedId
  | UnhideSubscription FeedId
  | RefreshFeeds

type Slots
  = ( subUrlInput :: H.Slot TextInput.Query Void Unit, tabs :: H.Slot (Tabs.Query SideTab) SideTab Unit )

_subUrlInput = Proxy :: Proxy "subUrlInput"

_tabs = Proxy :: Proxy "tabs"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall i. i -> State
initialState _ =
  { subscriptions: []
  , hiddenSubscriptions: mempty
  , unread: mempty
  , starred: mempty
  , currentEntry: Nothing
  , currentTab: EntriesState { entries: [], page: 1, reachedEnd: false }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  Layout.view
    { body: [ renderSideTab, maybe (HH.div_ []) Article.view state.currentEntry ]
    , menu:
      [ HH.slot _tabs unit Tabs.component allTabs SelectedTab
      , HH.i [ css "fa fa-rotate hover-pointer", HE.onClick $ const RefreshFeeds ] []
      ]
    }
  where
  renderSideTab = case state.currentTab of
    EntriesState { entries, reachedEnd } -> renderEntryList entries reachedEnd
    StarredState { entries, reachedEnd } -> renderEntryList entries reachedEnd
    SubscriptionState ->
      FeedList.view
        { feeds: state.subscriptions
        , isHidden: flip Set.member state.hiddenSubscriptions
        , onAddSubscription: AddSubscription
        , onDeleteSubscription: DeleteSubscription
        , onHideSubscription: HideSubscription
        , onUnhideSubscription: UnhideSubscription
        , input: HH.slot_ _subUrlInput unit TextInput.component { placeholder: "RSS feed URL" }
        }

  renderEntryList entries reachedEnd =
    let visibleEntries =
          Array.filter (\(Entry {feedId}) -> not $ Set.member feedId state.hiddenSubscriptions) entries
    in FeedEntryList.view
      { entries: visibleEntries
      , reachedEnd
      , onClick: SelectedEntry
      , onStar: StarEntry
      , onUnstar: UnstarEntry
      , onNextPage: LoadNextPage
      , isUnread: flip Set.member state.unread
      , isStarred: flip Set.member state.starred
      , feedName:
          \feedId ->
            map (_.title <<< unwrap)
              $ Array.find (\(Subscription sub) -> sub.id == feedId) state.subscriptions
      }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction act =
  runFallible
    $ case act of
        Initialize -> do
          entries <- ExceptT $ liftAff $ Api.getEntries { page: 1, perPage: pageSize }
          subs <- ExceptT $ liftAff Api.getSubscriptions
          starred <- ExceptT $ liftAff Api.getStarredEntries
          unread <- ExceptT $ liftAff Api.getUnreadEntries
          let
            currentTab = EntriesState { entries, page: 1, reachedEnd: false }
          H.modify_ $ _ { subscriptions = subs, starred = Set.fromFoldable starred, unread = Set.fromFoldable unread, currentTab = currentTab }
        
        SelectedEntry entry@(Entry { id }) -> do
          H.modify_ $ _ { currentEntry = Just entry }
          lift $ handleAction $ MarkAsRead id
        
        SelectedTab EntriesTab -> do
          entries <- ExceptT $ liftAff $ Api.getEntries { page: 1, perPage: pageSize }
          H.modify_ \st -> st { currentTab = EntriesState { entries, page: 1, reachedEnd: false } }
        
        SelectedTab StarredTab -> do
          entries <- ExceptT $ liftAff $ Api.getStarred { page: 1, perPage: pageSize }
          H.modify_ \st -> st { currentTab = StarredState { entries, page: 1, reachedEnd: false } }
        
        SelectedTab SubscriptionTab -> H.modify_ $ _ { currentTab = SubscriptionState }
        
        StarEntry entry -> do
          _ <- ExceptT $ liftAff $ Api.starEntries $ Array.singleton entry
          H.modify_ \st -> st { starred = Set.insert entry st.starred }
        
        UnstarEntry entry -> do
          _ <- ExceptT $ liftAff $ Api.unstarEntries $ Array.singleton entry
          H.modify_ \st -> st { starred = Set.delete entry st.starred }
        
        MarkAsRead entry -> do
          _ <- ExceptT $ liftAff $ Api.deleteUnread $ Array.singleton entry
          H.modify_ \st -> st { unread = Set.delete entry st.unread }
        
        AddSubscription -> do
          url <- lift $ H.request _subUrlInput unit TextInput.GetValue
          case url of
            Just ur -> do
              res <- liftAff $ Api.addSubscription ur
              case res of
                Right sub -> H.modify_ $ \st -> st { subscriptions = Array.snoc st.subscriptions sub }
                Left err -> lift $ H.tell _subUrlInput unit $ TextInput.SetError $ show err
            Nothing -> pure unit
        
        DeleteSubscription id -> do
          _ <- ExceptT $ liftAff $ Api.deleteSubscription id
          H.modify_ \st -> st { subscriptions = Array.filter (\(Subscription sub) -> sub.feedId /= id) st.subscriptions }
        
        HideSubscription id ->
          H.modify_ \st -> st { hiddenSubscriptions = Set.insert id st.hiddenSubscriptions }

        UnhideSubscription id ->
          H.modify_ \st -> st { hiddenSubscriptions = Set.delete id st.hiddenSubscriptions }

        LoadNextPage -> do
          state <- H.get
          case state.currentTab of
            EntriesState { entries, page } -> do
              more <- ExceptT $ liftAff $ Api.getEntries { page: page + 1, perPage: pageSize }
              let reachedEnd = Array.length more < 25
              H.modify_ $ _ { currentTab = EntriesState { entries: entries <> more, page: page + 1, reachedEnd } }
            StarredState { entries, page } -> do
              more <- ExceptT $ liftAff $ Api.getStarred { page: page + 1, perPage: pageSize }
              let reachedEnd = Array.length more < 25
              H.modify_ $ _ { currentTab = StarredState { entries: entries <> more, page: page + 1, reachedEnd } }
            _ -> H.modify_ $ _ { currentTab = SubscriptionState }
        
        RefreshFeeds -> do
          _ <- ExceptT $ liftAff Api.refreshSubscriptions
          _ <- lift $ H.tell _tabs unit $ Tabs.SelectTab EntriesTab
          lift $ handleAction $ SelectedTab EntriesTab
  
  where
  pageSize = 25

  runFallible fa = do
    res <- runExceptT fa
    case res of
      Right val -> pure val
      Left err -> Console.error $ show err
