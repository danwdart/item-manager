{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import Service.Category
import Service.Item
import Types.Category as Category
import Types.Item as Item
import UI.Bootstrap.Button
import UI.Bootstrap.TabbedNav

categoryToMap :: [Category] -> Map Text (Map Text Text)
categoryToMap categories = M.fromList $ fmap mapper categories
  where
    mapper category =
      ( T.pack . show . Category.categoryId $ category,
        M.fromList
          [ ("id", T.pack . show . Category.categoryId $ category),
            ("name", Category.name category)
          ]
      )

categoriesToSelect :: [Category] -> Map Int Text
categoriesToSelect categories = M.fromList $ fmap mapper categories
  where
    mapper category =
      ( Category.categoryId category,
        Category.name category
      )

itemToMap :: [Item] -> Map Text (Map Text Text)
itemToMap categories = M.fromList $ fmap mapper categories
  where
    mapper item =
      ( T.pack . show . itemId $ item,
        M.fromList
          [ ("id", T.pack . show . itemId $ item),
            ("name", Item.name item),
            ("categoryId", T.pack . show $ Item.categoryId item)
          ]
      )

widgetItems :: (MonadWidget t m) => m ()
widgetItems = mdo
  epb <- getPostBuild

  eGetItems <- getAllItems $ leftmost [epb, evCloseAddModal, evCloseEditModal, refreshWhen]

  eGetCategories <- getAllCategories $ leftmost [epb, addItem, void editWhen]

  dynCategories <- holdDyn [] $ categoriesToSelect <$> eGetCategories

  dynTableData <- holdDyn [] $ itemToMap <$> eGetItems

  addItem <- bsButton "btn btn-primary" "Add Item"

  showSection <-
    holdDyn
      False
      ( leftmost
          [ True <$ addItem,
            False <$ evCloseAddModal
          ]
      )

  let refreshWhen = switchDyn $ leftmost . (fst . last . snd <$>) . M.elems <$> table

  let editWhen = switchDyn $ leftmost . (snd . (!! 2) . snd <$>) . M.elems <$> table

  table <-
    tableDynAttr
      "table table-striped table-bordered table-hover"
      [ -- ("", \k v -> void . inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ [("type", "checkbox")]),
        ( "Name",
          \k v -> do
            dynText $ fmap (M.findWithDefault "(none)" "name") v
            pure (never, never)
        ),
        ( "Category",
          \k v -> do
            let dynCategoryId = read @Int . T.unpack <$> fmap (M.findWithDefault "(none)" "categoryId") v
            dynText $ M.findWithDefault "(None)" <$> dynCategoryId <*> dynCategories
            pure (never, never)
        ),
        ( "",
          \k v -> do
            editButton <- bsButton "btn btn-warning" "Edit"
            pure (never, k <$ editButton)
        ),
        ( "",
          \k v -> do
            deleteButton <- bsButton "btn btn-danger" "Delete"
            deleted <- deleteItem $ read (T.unpack k) <$ deleteButton
            pure (void deleted, never)
        )
      ]
      dynTableData
      ( \k -> pure $ constDyn [] -- no attrs
      )

  dynAddModalShown <-
    holdDyn
      False
      ( leftmost
          [ True <$ addItem,
            False <$ evCloseAddModal
          ]
      )

  evCloseAddModal <- elDynAttr "div" ((\shown -> [("class", "modal fade" <> if shown then " show" else ""), ("style", if shown then "display: block" else "")]) <$> dynAddModalShown)
    . divClass "modal-dialog"
    . divClass "modal-content"
    $ do
      divClass "modal-header"
        . elClass "h5" "modal-title"
        $ text "Add"
      (elItemName, elCategory) <- divClass "modal-body"
        . el "form"
        $ do
          elItemName <- divClass "form-group" $ do
            elAttr "label" [("for", "name")] $ text "Name"
            inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ [("id", "name"), ("class", "form-control"), ("placeholder", "Bob")]
          elCategory <- divClass "form-group" $ do
            elAttr "label" [("for", "categoryId")] $ text "Category"
            dropdown 0 dynCategories $ def & dropdownConfig_attributes .~ pure [("id", "categoryId"), ("class", "form-control")]
          pure (elItemName, elCategory)
      divClass "modal-footer" $ do
        evtSaveAddModal <- bsButton "btn btn-primary" "Save"
        let dynItemName = _inputElement_value elItemName
        let dynCategoryId = _dropdown_value elCategory
        let dynCreateItem = CreateItem <$> dynItemName <*> dynCategoryId
        evtCreate <- createItem $ tagPromptlyDyn dynCreateItem evtSaveAddModal
        evtCloseModal <- bsButton "btn btn-secondary" "Cancel"
        pure $
          leftmost
            [ void evtCreate,
              evtCloseModal
            ]

  dynEditModalShown <-
    holdDyn
      False
      ( leftmost
          [ True <$ editWhen,
            False <$ evCloseEditModal
          ]
      )

  evCloseEditModal <- elDynAttr "div" ((\shown -> [("class", "modal fade" <> if shown then " show" else ""), ("style", if shown then "display: block" else "")]) <$> dynEditModalShown)
    . divClass "modal-dialog"
    . divClass "modal-content"
    $ do
      divClass "modal-header"
        . elClass "h5" "modal-title"
        $ text "Edit"
      (dynItemId, dynItemName, dynItemCategoryId) <- divClass "modal-body"
        . el "form"
        $ do
          evtGetItem <- getItem (read . T.unpack <$> editWhen)

          let evtItemName = maybe "" Item.name <$> evtGetItem
          let evtItemId = maybe 0 itemId <$> evtGetItem
          let evtCategoryId = maybe 0 Item.categoryId <$> evtGetItem

          dynItemName <- holdDyn "" evtItemName
          dynItemId <- holdDyn 0 evtItemId
          dynCategoryId <- holdDyn 0 evtCategoryId

          inputElName <- divClass "form-group" $ do
            elAttr "label" [("for", "name")] $ text "Name"
            inputElement $
              def
                & inputElementConfig_elementConfig . elementConfig_initialAttributes
                  .~ [ ("id", "name"),
                       ("class", "form-control"),
                       ("placeholder", "Bob's Item")
                     ]
                & inputElementConfig_setValue .~ updated dynItemName

          let dynItemNameText = _inputElement_value inputElName

          inputElCategory <- divClass "form-group" $ do
            elAttr "label" [("for", "categoryId")] $ text "Category"
            dropdown 0 dynCategories $
              def
                & dropdownConfig_attributes .~ pure [("id", "categoryId"), ("class", "form-control")]
                & dropdownConfig_setValue .~ updated dynCategoryId

          let dynItemCategoryId = _dropdown_value inputElCategory

          pure (dynItemId, dynItemNameText, dynItemCategoryId)

      divClass "modal-footer" $ do
        evtSaveModal <- bsButton "btn btn-primary" "Save"
        let dynItem =
              Item
                <$> dynItemId
                <*> dynItemName
                <*> dynItemCategoryId
        let evItem = tagPromptlyDyn dynItem evtSaveModal
        evtModify <- modifyItem evItem
        evtCloseModal <- bsButton "btn btn-secondary" "Cancel"
        pure $
          leftmost
            [ void evtModify,
              evtCloseModal
            ]
  pure ()

widgetCategories :: (MonadWidget t m) => m ()
widgetCategories = mdo
  epb <- getPostBuild

  eGetCategories <- getAllCategories $ leftmost [epb, evCloseAddModal, evCloseEditModal, refreshWhen]

  dynTableData <- holdDyn [] $ categoryToMap <$> eGetCategories

  addCategory <- bsButton "btn btn-primary" "Add Category"

  showSection <-
    holdDyn
      False
      ( leftmost
          [ True <$ addCategory,
            False <$ evCloseAddModal
          ]
      )

  let refreshWhen = switchDyn $ leftmost . (fst . last . snd <$>) . M.elems <$> table

  let editWhen = switchDyn $ leftmost . (snd . (!! 1) . snd <$>) . M.elems <$> table

  table <-
    tableDynAttr
      "table table-striped table-bordered table-hover"
      [ -- ("", \k v -> void . inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ [("type", "checkbox")]),
        ( "Name",
          \k v -> do
            dynText $ fmap (M.findWithDefault "(none)" "name") v
            pure (never, never)
        ),
        ( "",
          \k v -> do
            editButton <- bsButton "btn btn-warning" "Edit"
            pure (never, k <$ editButton)
        ),
        ( "",
          \k v -> do
            deleteButton <- bsButton "btn btn-danger" "Delete"
            deleted <- deleteCategory $ read (T.unpack k) <$ deleteButton
            pure (void deleted, never)
        )
      ]
      dynTableData
      ( \k -> pure $ constDyn [] -- no attrs
      )

  dynAddModalShown <-
    holdDyn
      False
      ( leftmost
          [ True <$ addCategory,
            False <$ evCloseAddModal
          ]
      )

  evCloseAddModal <- elDynAttr "div" ((\shown -> [("class", "modal fade" <> if shown then " show" else ""), ("style", if shown then "display: block" else "")]) <$> dynAddModalShown)
    . divClass "modal-dialog"
    . divClass "modal-content"
    $ do
      divClass "modal-header"
        . elClass "h5" "modal-title"
        $ text "Add"
      elCategory <- divClass "modal-body"
        . el "form"
        . divClass "form-group"
        $ do
          elAttr "label" [("for", "name")] $ text "Name"
          inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ [("id", "name"), ("class", "form-control"), ("placeholder", "Bob")]
      divClass "modal-footer" $ do
        evtSaveAddModal <- bsButton "btn btn-primary" "Save"
        let dynCategory = _inputElement_value elCategory
        evtCreate <- createCategory $ tagPromptlyDyn dynCategory evtSaveAddModal
        evtCloseModal <- bsButton "btn btn-secondary" "Cancel"
        pure $
          leftmost
            [ void evtCreate,
              evtCloseModal
            ]

  dynEditModalShown <-
    holdDyn
      False
      ( leftmost
          [ True <$ editWhen,
            False <$ evCloseEditModal
          ]
      )

  evCloseEditModal <- elDynAttr "div" ((\shown -> [("class", "modal fade" <> if shown then " show" else ""), ("style", if shown then "display: block" else "")]) <$> dynEditModalShown)
    . divClass "modal-dialog"
    . divClass "modal-content"
    $ do
      divClass "modal-header"
        . elClass "h5" "modal-title"
        $ text "Edit"
      (dynCategoryId, dynCategoryName) <- divClass "modal-body"
        . el "form"
        . divClass "form-group"
        $ mdo
          elAttr "label" [("for", "name")] $ text "Name"
          evtGetCategory <- getCategory (read . T.unpack <$> editWhen)
          let evtGrabbedCategory = maybe "" Category.name <$> evtGetCategory
          let evtCategoryId = maybe 0 Category.categoryId <$> evtGetCategory
          dynCategoryName <- holdDyn "" evtGrabbedCategory
          dynCategoryId <- holdDyn 0 evtCategoryId
          inputCategory <-
            inputElement $
              def
                & inputElementConfig_elementConfig . elementConfig_initialAttributes
                  .~ [ ("id", "name"),
                       ("class", "form-control"),
                       ("placeholder", "Bob's Category")
                     ]
                & inputElementConfig_setValue .~ updated dynCategoryName
          let dynCategory = _inputElement_value inputCategory
          pure (dynCategoryId, dynCategory)
      divClass "modal-footer" $ do
        evtSaveModal <- bsButton "btn btn-primary" "Save"
        let dynCategory =
              Category
                <$> dynCategoryId
                <*> dynCategoryName
        let evCategory = tagPromptlyDyn dynCategory evtSaveModal
        evtModify <- modifyCategory evCategory
        evtCloseModal <- bsButton "btn btn-secondary" "Cancel"
        pure $
          leftmost
            [ void evtModify,
              evtCloseModal
            ]

  pure ()

widget :: (MonadWidget t m) => m ()
widget =
  divClass "container-fluid" $
    bsTabbedNav
      "Item Manager"
      "items"
      [ ("items", "Items", widgetItems),
        ("categories", "Categories", widgetCategories)
      ]

main :: IO ()
main =
  mainWidgetWithHead
    ( do
        elAttr
          "link"
          [ ("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/css/bootstrap.min.css"),
            ("rel", "stylesheet"),
            ("integrity", "sha384-BmbxuPwQa2lc/FVzBcNJ7UAyJxM6wuqIj61tLrc4wSX0szH/Ev+nYRRuWlolflfl"),
            ("crossorigin", "anonymous")
          ]
          blank
        elAttr
          "meta"
          [ ("charset", "utf-8")
          ]
          blank
        elAttr
          "meta"
          [ ("name", "viewport"),
            ("content", "width=device-width, initial-scale=1.0")
          ]
          blank
    )
    widget
