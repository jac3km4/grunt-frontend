module Grunt.Component.App (component) where

import Prelude

import Component.View.Layout as Layout
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Grunt.Types (Entry(..), EntryId, FeedId, Subscription(..), Tag(..), Tagging(..), TaggingId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

type State
  = { subscriptions :: Array Subscription
    , unread :: Set EntryId
    , starred :: Set EntryId
    , currentEntry :: Maybe Entry
    , currentTab :: TabState
    , tags :: Map Tag (Map FeedId TaggingId)
    , tagPopupOpen :: Boolean
    , filters :: Set Tag
    , tagInput :: String
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
  | RefreshFeeds
  | ToggleTagPopup
  | SubmitTagPopup
  | TagFeed Tag FeedId
  | UntagFeed Tag TaggingId
  | EnableFilter Tag Boolean
  | TagInput String

type Slots
  = ( feedUrlInput :: H.Slot TextInput.Query Void Unit
    , tabs :: H.Slot (Tabs.Query SideTab) SideTab Unit
    )

_feedUrlInput = Proxy :: Proxy "feedUrlInput"
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
  , unread: mempty
  , starred: mempty
  , currentEntry: Nothing
  , currentTab: EntriesState { entries: [], page: 1, reachedEnd: false }
  , tags: Map.empty
  , tagPopupOpen: false
  , filters: Set.empty
  , tagInput: ""
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
    EntriesState { entries, reachedEnd } ->
      HH.div [ css "side-bar" ]
        [ renderTags
        , renderEntryList entries reachedEnd
        ]
    StarredState { entries, reachedEnd } ->
      HH.div [ css "side-bar" ]
        [ renderEntryList entries reachedEnd
        ]
    SubscriptionState ->
      FeedList.view
        { feeds: state.subscriptions
        , tags: state.tags
        , popupIsOpen: state.tagPopupOpen
        , onAddSubscription: AddSubscription
        , onDeleteSubscription: DeleteSubscription
        , onToggleTagPopup: ToggleTagPopup
        , onSubmitTagPopup: SubmitTagPopup
        , onTagFeed: TagFeed
        , onUntagFeed: UntagFeed
        , onTagInput: TagInput
        , feedUrlInput: HH.slot_ _feedUrlInput unit TextInput.component { placeholder: "RSS feed URL" }
        , tagInputValue: state.tagInput
        }

  renderEntryList entries reachedEnd =
    FeedEntryList.view
      { entries
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

  renderTags =
    HH.details [ css "tag-filter", HPA.role "list"]
      [ HH.summary [] [ renderFilters ]
      , HH.ul [ HPA.role "listbox" ]
          $ map renderTag
          $ Set.toUnfoldable
          $ Map.keys state.tags
      ]

  renderTag tag =
    let isChecked = Set.member tag state.filters
    in HH.li_
      [ HH.label_
        [ HH.input [ HP.type_ InputCheckbox, HP.checked isChecked, HE.onChecked $ EnableFilter tag ]
        , HH.text $ coerce tag
        ]
      ]
  
  renderFilters
    | Set.isEmpty state.filters = HH.text "No filters"
    | otherwise = HH.text $ "Filters: " <> intercalate ", " (Set.map coerce state.filters)

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction act =
  runFallible
    $ case act of
        Initialize -> do
          entries <- ExceptT $ liftAff $ Api.getEntries { page: 1, perPage: pageSize, tags: Set.empty }
          subs <- ExceptT $ liftAff Api.getSubscriptions
          starred <- ExceptT $ liftAff Api.getStarredEntries
          unread <- ExceptT $ liftAff Api.getUnreadEntries
          taggings <- ExceptT $ liftAff Api.getTaggings
          let
            tags = Array.foldl addTagging Map.empty taggings
            currentTab = EntriesState { entries, page: 1, reachedEnd: false }
          H.modify_ $ _
            { subscriptions = subs
            , starred = Set.fromFoldable starred
            , unread = Set.fromFoldable unread
            , tags = tags
            , currentTab = currentTab
            }
        
        SelectedEntry entry@(Entry { id }) -> do
          H.modify_ $ _ { currentEntry = Just entry }
          lift $ handleAction $ MarkAsRead id
        
        SelectedTab EntriesTab -> do
          {filters} <- H.get
          entries <- ExceptT $ liftAff $ Api.getEntries { page: 1, perPage: pageSize, tags: filters }
          unread <- ExceptT $ liftAff Api.getUnreadEntries
          H.modify_ \st -> st
            { currentTab = EntriesState { entries, page: 1, reachedEnd: false }
            , unread = Set.fromFoldable unread
            }

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
          url <- lift $ H.request _feedUrlInput unit TextInput.GetValue
          case url of
            Just ur -> do
              res <- liftAff $ Api.addSubscription ur
              case res of
                Right sub -> H.modify_ $ \st -> st { subscriptions = Array.snoc st.subscriptions sub }
                Left err -> lift $ H.tell _feedUrlInput unit $ TextInput.SetError $ show err
            Nothing -> pure unit
        
        DeleteSubscription id -> do
          _ <- ExceptT $ liftAff $ Api.deleteSubscription id
          H.modify_ \st -> st { subscriptions = Array.filter (\(Subscription sub) -> sub.feedId /= id) st.subscriptions }
        
        LoadNextPage -> do
          {currentTab, filters} <- H.get
          case currentTab of
            EntriesState { entries, page } -> do
              more <- ExceptT $ liftAff $ Api.getEntries { page: page + 1, perPage: pageSize, tags: filters }
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
        
        ToggleTagPopup ->
          H.modify_ \st -> st { tagPopupOpen = not st.tagPopupOpen }
        
        SubmitTagPopup -> do
          {tagInput} <- H.get
          H.modify_ \st -> st
            { tags = Map.alter (Just <<< fromMaybe Map.empty) (Tag tagInput) st.tags
            , tagPopupOpen = false
            }

        TagFeed tag feedId -> do
          tagging <- ExceptT $ liftAff $ Api.addTagging feedId tag
          H.modify_ \st -> st { tags = addTagging st.tags tagging }

        UntagFeed tag tagId -> do
          _ <- ExceptT $ liftAff $ Api.deleteTagging tagId
          H.modify_ \st -> st { tags = Map.update (Just <<< Map.filter (_ /= tagId)) tag st.tags }
        
        EnableFilter tag enabled -> do
          if enabled then
            H.modify_ \st -> st { filters = Set.insert tag st.filters }
          else
            H.modify_ \st -> st { filters = Set.delete tag st.filters }
          _ <- lift $ H.tell _tabs unit $ Tabs.SelectTab EntriesTab
          lift $ handleAction $ SelectedTab EntriesTab
        
        TagInput val ->
          H.modify_ $ _ { tagInput = val }

  where
  pageSize = 25

  runFallible fa = do
    res <- runExceptT fa
    case res of
      Right val -> pure val
      Left err -> Console.error $ show err

  addTagging acc (Tagging { feedId, id, name }) =
    Map.alter (Just <<< Map.insert feedId id <<< fromMaybe Map.empty) name acc
