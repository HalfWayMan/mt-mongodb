{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TemplateHaskell                                                      #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Template
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Template Haskell functions for the automatic derivation of 'MongoEntity' instances.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.Template ( asMongoEntity
                                         , useDefaults
                                         , setCollectionName
                                         , forConstructor
                                         , ConstructorOp
                                         , setConstructorName
                                         , renameFields
                                         , assocFieldNames
                                         , indexedFieldName
                                         , setFieldReadOnly
                                         , asMongoValue
                                         , encodedViaShow
                                         ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bson (Field ((:=)))
import qualified Data.Bson as Bson
import           Data.Char (toUpper, toLower)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH.Syntax hiding (lift, newName)
import           Text.Printf
import           Massive.Database.MongoDB.MongoValue
import           Massive.Database.MongoDB.MongoEntity

-----------------------------------------------------------------------------------------------------------------------

-- Represents a field declaration from the type we are to store.
data FieldDecl = -- A field ina record data type constructor.
                 FieldDecl { fieldName       ∷ Name
                           , fieldSimpleName ∷ String   -- The simple name of the field, as stored in the document.
                                                        -- Can be changed.
                           , fieldType       ∷ Type
                           , fieldReadOnly   ∷ Bool     -- If 'True', the field will not be output by the 'toDocument'
                                                        -- function.
                           }
                 -- Represents a field in a 'NormalC' data type constructor.
               | SimpleFieldDecl { simpleFieldName ∷ String
                                 , simpleFieldType ∷ Type
                                 }

-- Represents a constructor to a data type. We blend both 'NormalC' and 'RecordC' data type constructors.
data Constructor = Constructor { constrName       ∷ Name
                               , constrSimpleName ∷ String
                               , constrBody       ∷ [FieldDecl]
                               }

-- The environment in which our template builder lives. This is actually used as _state_, but whatever.
data TemplateEnv = TemplateEnv { envTypeName       ∷ Name
                               , envSimpleTypeName ∷ String
                               , envCollectionName ∷ String
                               , envEncodedViaShow ∷ Bool
                               , envConstructors   ∷ [Constructor]
                               }


type EndoFunctor α = α → α
type TemplateGen   = StateT TemplateEnv (WriterT (EndoFunctor [Dec]) Q)

-----------------------------------------------------------------------------------------------------------------------

-- | This function is used to indicate to 'asMongoEntity' and 'asMongoValue' that the default behaviour is to be
--   assumed when generating either instances.
useDefaults ∷ TemplateGen ()
useDefaults = return ()

-- | This function generates an instance of the 'MongoEntity' and 'MongoValue' type classes for the specified type.
asMongoEntity ∷ Name → TemplateGen () → Q [Dec]
asMongoEntity typeName actions =
  runTemplateGen typeName (actions >> genEntityInstance >> genValueInstance)

-- | This function generates an instance of the 'MongoValue' type class for a given type. If the type is to be
--   encoded via 'show' and 'read', then a 'MongoEntity' instance is /not/ created; otherwise one will be.
asMongoValue ∷ Name → TemplateGen () → Q [Dec]
asMongoValue typeName actions =
  runTemplateGen typeName $ do
    actions
    viaShow ← gets envEncodedViaShow
    if viaShow
       then genValueViaShowInstance else (genEntityInstance >> genValueInstance)

-----------------------------------------------------------------------------------------------------------------------

-- | Set the name of the collection to which the type is to be stored.
--
--   By default, the name of the collection is the same as the name of the type.
setCollectionName ∷ String → TemplateGen ()
setCollectionName newName =
  modify (\s → s { envCollectionName = newName })

-- | Set whether the 'MongoValue' instance for the type should be encoded via 'read' and 'show' rather than
--   'fromDocument' and 'toDocument'.
encodedViaShow ∷ TemplateGen ()
encodedViaShow =
  modify (\s → s { envEncodedViaShow = True })

-----------------------------------------------------------------------------------------------------------------------

-- | Operations over constructors live in the 'ConstructorOp' monad.
type ConstructorOp = StateT Constructor TemplateGen

-- | For @forConstructor name ops@, perform @ops@ for the constructor matching @name@. This allows us to change the
--   name of the constructor stored in the @_type@ field, and alter the behaviour of individual fields.
forConstructor ∷ Name → ConstructorOp () → TemplateGen ()
forConstructor name f = do
  constrs ← dConstr =<< gets envConstructors
  modify (\s → s { envConstructors = constrs })
  where
    dConstr       [] = return []
    dConstr (x : xs)
      | constrName x == name = do
        (_, x') ← runStateT f x
        return (x' : xs)
      | otherwise            = do
        xs' ← dConstr xs
        return (x : xs')

-- | Set the name stored in the @_type@ field of the document for the current constructor.
setConstructorName ∷ String → ConstructorOp ()
setConstructorName newName = do
  modify (\c -> c { constrSimpleName = newName })

-- | Set the field with the specified name to read only.
--
--   This will mean that the 'encodeEntity' function will not output this field. This means that the fields value
--   will not be written to the document.
setFieldReadOnly ∷ Name → ConstructorOp ()
setFieldReadOnly name =
  modify (\c → c { constrBody = dField (constrBody c) })
  where
    dField       [] = []
    dField (f : fs)
      | fieldName f == name = f { fieldReadOnly = True } : fs
      | otherwise           = f : dField fs

-- | Change the names of a number of fields.
--
--   The default behaviour is for the fields in the document to have the same name as the fields in the record
--   type constructor.
assocFieldNames ∷ [(Name, String)] → ConstructorOp ()
assocFieldNames assocs =
  modify (\c → c { constrBody = dField (constrBody c) })
  where
    dField [] = []
    dField (field : fields) =
      case field of
        FieldDecl name _ _ _ →
          (maybe field (\name' → field { fieldSimpleName = name' }) $ lookup name assocs) : dField fields
        SimpleFieldDecl name _ →
          let name'' = mkName name in (maybe field (\name' → field { simpleFieldName = name' }) $ lookup name'' assocs) : dField fields

-- | Renames fields in the order they are found in the type constructor.
--
--   This is useful as you cannot use 'assocFieldNames' with data type constructors that do not provide field names.
--   As an example:
--   @
--   data MyType = MyTypeA Text Double
--               | MyTypeB Int
--
--   asMongoEntity ''MyType $
--     forConstructor 'MyTypeA $
--       renameFields ["userName", "userAge"]
--     forConstructor 'MyTypeB $
--       renameFields ["someIndex"]
--   @
renameFields ∷ [String] -> ConstructorOp ()
renameFields newNames =
  modify (\c → c { constrBody = zipWith renameField newNames (constrBody c) })
  where
    renameField name (FieldDecl fn _ ft ro) = FieldDecl fn name ft ro
    renameField name (SimpleFieldDecl _ ft) = SimpleFieldDecl name ft

-----------------------------------------------------------------------------------------------------------------------

indexedFieldName ∷ Int → Name
indexedFieldName = mkName ∘ ("field" ++) ∘ show

runTemplateGen ∷ Name → TemplateGen () → Q [Dec]
runTemplateGen typeName actions = do
  (simpleName, constructors) ← getSimpleNameAndConstrs typeName
  let env = TemplateEnv { envTypeName       = typeName
                        , envSimpleTypeName = simpleName
                        , envCollectionName = uncapitalize simpleName
                        , envEncodedViaShow = False
                        , envConstructors   = buildConstructors constructors
                        }
  (_, decls) ← runWriterT (runStateT actions env)
  return $ decls []

inner ∷ TemplateGen α → TemplateGen (α, [Dec])
inner action = do
  st ← get
  ((result, newState), decls) ← lift ∘ lift $ runWriterT (runStateT action st)
  put newState
  return (result, decls [])

getSimpleNameAndConstrs ∷ Name → Q (String, [Con])
getSimpleNameAndConstrs typeName = do
  typeInfo ← reify typeName
  case typeInfo of
    TyConI tyCon →
      case tyCon of
        DataD    _ n _ c _ → return (dropPrefix $ show n,  c )
        NewtypeD _ n _ c _ → return (dropPrefix $ show n, [c])
        _                  → fail $ printf "Not able to handle '%s'; not a data or newtype" (show typeName)
    _ → fail $ printf "Not able to handle '%s'; not a type constructor" (show typeName)

buildConstructors ∷ [Con] → [Constructor]
buildConstructors =
  map buildConstructor
  where
    buildConstructor ∷ Con → Constructor
    buildConstructor (NormalC name types) =
      Constructor { constrName       = name
                  , constrSimpleName = dropPrefix $ show name
                  , constrBody       =
                      map (\(n, (_, t)) → SimpleFieldDecl { simpleFieldName = "field" ++ show n
                                                          , simpleFieldType = t
                                                          }) $ zip ([1 ..] ∷ [Int]) types
                  }
    buildConstructor (RecC name fields) =
      Constructor { constrName       = name
                  , constrSimpleName = dropPrefix $ show name
                  , constrBody       =
                      map (\(n, _, t) →
                           let (r, n') = if ((dropPrefix ∘ show $ n) == ((uncapitalize ∘ dropPrefix ∘ show $ name) ++ "Id"))
                                            then (True, "_id")
                                            else (False, dropPrefix $ show n)
                           in FieldDecl { fieldName       = n
                                        , fieldSimpleName = n'
                                        , fieldType       = t
                                        , fieldReadOnly   = r
                                        }) fields
                  }
    buildConstructor _ =
      error "cannot build constructor for non-record or non-normal data type constructor"


-- Emits a declaration in the underlying writer monad.
emitDecl ∷ Dec → TemplateGen ()
emitDecl = lift ∘ tell ∘ (:)

-- Generate the key newtype and the 'fromKey' and 'toKey' functions for the specified type. For a given type MyType,
-- we generate a newtype with the following definition:
--
--   newtype Key MyType = MyTypeId { unMyTypeId :: ObjectId }
--                      deriving (Eq)
--
-- The two functions 'toKey' and 'fromKey' are trivial aliases for constructing and deconstructing this type:
--
--   toKey = MyTypeId
--   fromKey = unMyTypeId
--
genKeyDecls ∷ TemplateGen ()
genKeyDecls = do
  name  ← gets envTypeName
  name' ← gets envSimpleTypeName
  emitDecl $ NewtypeInstD [] ''Key [ConT name]
                          (RecC (mkName $ "Mk" ++ name' ++ "Id")
                                [ (mkName $ "un" ++ name' ++ "Id", NotStrict, ConT ''ObjectId) ])
                          [ ''Eq ]
  emitDecl $ FunD (mkName "toKey")
                  [ Clause [] (NormalB ∘ ConE ∘ mkName $ "Mk" ++ name' ++ "Id") [] ]
  emitDecl $ FunD (mkName "fromKey")
                  [ Clause [] (NormalB ∘ VarE ∘ mkName $ "un" ++ name' ++ "Id") []]


genDocumentDecls ∷ TemplateGen ()
genDocumentDecls = do
  name  ← gets envTypeName
  name' ← gets envSimpleTypeName
  emitDecl $ NewtypeInstD [] ''Document [ConT name]
                          (RecC (mkName $ name' ++ "Document")
                                [ (mkName $ "un" ++ name' ++ "Document", NotStrict, ConT ''Bson.Document) ])
                          []
  emitDecl $ FunD (mkName "toDocument")
                  [ Clause [] (NormalB ∘ ConE ∘ mkName $ name' ++ "Document") [] ]
  emitDecl $ FunD (mkName "fromDocument")
                  [ Clause [] (NormalB ∘ VarE ∘ mkName $ "un" ++ name' ++ "Document") [] ]


genCollNameDecl ∷ TemplateGen ()
genCollNameDecl = do
  cName ← gets envCollectionName
  emitDecl $ FunD (mkName "collectionName")
                  [ Clause [WildP] (NormalB ∘ LitE ∘ StringL $ cName) [] ]

-- This function generates the filter declaration. The filter type instance has the following form:
--
--   data Filter MyType = MyFieldA Int | MyFieldB String | ...
--
-- Where each field in MyType has a constructor in the (Filter MyType) data type.
--
genFilterDecl ∷ TemplateGen ()
genFilterDecl = do
  name    ← gets envTypeName
  constrs ← gets envConstructors
  let cons = (idCon name :) . catMaybes $ concatMap (\c → let cname = dropPrefix . show . constrName $ c in map (buildFieldCon cname) (constrBody c)) constrs
  emitDecl $ DataInstD [] ''Filter [ConT name]
                       (map fst cons)
                       []
  let clauses = map (\((NormalC name' _), fName) →
                      Clause [ConP name' [WildP]] (NormalB ∘ LitE ∘ StringL ∘ dropPrefix ∘ show $ fName) []) cons
  if not ∘ null $ clauses
     then emitDecl $ FunD (mkName "filterFieldName") clauses
     else emitDecl $ FunD (mkName "filterFieldName") [Clause [WildP] (NormalB ((VarE 'error) `AppE` (LitE $ StringL "no filters can be defined"))) []]
  where
    idCon :: Name -> (Con, Name)
    idCon name =
      let cname = dropPrefix . show $ name
      in (NormalC (mkName (cname ++ "Id")) [(NotStrict, (ConT ''Key) `AppT` (ConT name))], mkName "_id")

    buildFieldCon ∷ String → FieldDecl → Maybe (Con, Name)
    buildFieldCon cname (FieldDecl fname simpleName fType _) =
      if ((dropPrefix ∘ show $ fname) == (uncapitalize cname ++ "Id")) || (simpleName == "_id")
         then Nothing
         else Just (NormalC (mkName ∘ capitalize ∘ dropPrefix ∘ show $ fname) [(NotStrict, fType)], mkName simpleName)
    buildFieldCon _ (SimpleFieldDecl _ _) = Nothing

-- This function generates the 'encodeEntity' function for the 'MongoEntity' instance. The 'encodeEntity' function
-- generates a 'Document' from an instance of the type. For example, given the type:
--
--   data A = A { fieldA :: Int
--              , fieldB :: String
--              }
--          | B { fieldC :: String
--              , fieldD :: Int
--              }
--
-- A document will have one of the following forms:
--
--   { _type: "A", fieldA: 123, fieldB: "hello" }
--   { _type: "B", fieldA: "world", fieldB: 456 }
--
-- The Haskell code to generate this will be equivalent to:
--
--   encodeEntity (A field1 field2) = toDocument [ "_type" := toValue "A"
--                                               , "fieldA" := toValue field1
--                                               , "fieldB" := toValue field2 ]
--   encodeEntity (B field1 field2) = toDocument [ "_type" := toValue "B"
--                                               , "fieldC" := toValue field1
--                                               , "fieldD" := toValue field2 ]
--
-- Note: The extra field "_type" is stored in the document so that we know which constructor to use when we load the
--       document back into Haskell.
--
-- Note: A field will not be written to the document if it has been set read-only.
--
-- Note: A field will not be written to the document if it is identified as the ID of the document.
--       A field is recognised as the ID of the document if its name matches the constrctor of the type, followed by
--       "Id". For example, for a data type "MyType", a field with the name "myTypeId" will be assumed to be the ID.
--
genEncodeEntity ∷ TemplateGen ()
genEncodeEntity = do
  constrs ← gets envConstructors
  emitDecl $ FunD (mkName "encodeEntity") $ map buildClause constrs
  where
    buildClause ∷ Constructor → Clause
    buildClause constr =
      let fn (FieldDecl   _ n _ _) = n
          fn (SimpleFieldDecl n _) = n
          patNames = map (mkName ∘ ('_' :) ∘ fn) (constrBody constr)
          setType  = InfixE (Just ∘ LitE ∘ StringL $ "_type")
                            (ConE '(:=))
                            (Just ((VarE 'toValue) `AppE` ((LitE $ StringL (constrSimpleName constr)) `SigE` (ConT ''Text))))
          setFields = catMaybes (map (genField (dropPrefix ∘ show ∘ constrName $ constr)) (zip patNames (constrBody constr)))
      in Clause [ConP (constrName constr) (map VarP patNames)]
                (NormalB $ ((VarE 'toDocument) `AppE` (ListE (setType : setFields))))
                []

    genField ∷ String → (Name, FieldDecl) → Maybe Exp
    genField _ (patName, SimpleFieldDecl fname _) =
      Just (InfixE (Just ∘ LitE ∘ StringL $ fname) (ConE '(:=)) (Just ((VarE 'toValue) `AppE` (VarE patName))))
    genField cname (patName, FieldDecl fname simpleName _ readOnly) =
      if readOnly ||
         ((dropPrefix ∘ show $ fname) == (uncapitalize cname ++ "Id")) ||
         (simpleName == "_id")
         then Nothing
         else Just (InfixE (Just ∘ LitE ∘ StringL $ simpleName) (ConE '(:=)) (Just ((VarE 'toValue) `AppE` (VarE patName))))


{-
decodeEntity doc' = do
  let doc = fromDocument doc'
  typ <- lookupThrow "_type" doc
  case typ of
    "A"   -> A <$> lookupThrow "fieldA" doc <*> lookupThrow "fieldB" doc
    other -> throwError $ "Unknown type constructor '" ++ other ++ "' found in collection for data type '" ++ "A"
-}

genDecodeEntity ∷ TemplateGen ()
genDecodeEntity = do
  name    ← gets envTypeName
  cName   ← gets envCollectionName
  constrs ← gets envConstructors
  let doc' = mkName "doc'"
  emitDecl $ FunD (mkName "decodeEntity") $
                  [ Clause [VarP doc'] (NormalB $ buildBody name cName doc' constrs) [] ]
  where
    buildBody ∷ Name → String → Name → [Constructor] → Exp
    buildBody name cName doc' constrs =
      let typ   = mkName "typ"
          other = mkName "other"
          doc   = mkName "doc"
      in DoE [ LetS [ ValD (VarP doc) (NormalB ((VarE 'fromDocument) `AppE` (VarE doc'))) [] ]
             , BindS (VarP typ) (((VarE 'lookupThrow) `AppE` (VarE doc)) `AppE` (LitE $ StringL "_type"))
             , NoBindS $ CaseE (VarE typ) (map (buildMatch doc) constrs ++ [ Match (VarP other)
                                                                                   (NormalB $ ((VarE 'throwError) `AppE`
                                                                                               ((VarE 'concat) `AppE`
                                                                                                ListE [ LitE $ StringL "unknown type constructor '"
                                                                                                      , ((VarE 'T.unpack) `AppE` (VarE other))
                                                                                                      , LitE $ StringL "' found in collection for data type '"
                                                                                                      , LitE $ StringL (dropPrefix $ show name)
                                                                                                      , LitE $ StringL "' ("
                                                                                                      , LitE $ StringL cName
                                                                                                      , LitE $ StringL ")"]))) []]) ]

    buildMatch ∷ Name → Constructor → Match
    buildMatch doc constr =
      let body = if null (constrBody constr)
                    then ((VarE 'return) `AppE` (ConE (constrName constr)))
                    else foldl (genFieldApp doc)
                               (InfixE (Just (ConE $ constrName constr))
                                       (VarE '(<$>))
                                       (Just (genFieldLookup doc $ head (constrBody constr))))
                               (tail (constrBody constr))
      in Match (LitP $ StringL $ constrSimpleName constr) (NormalB body) []

    genFieldApp ∷ Name → Exp → FieldDecl → Exp
    genFieldApp doc lhs fieldDecl =
      InfixE (Just lhs)
             (VarE '(<*>))
             (Just $ genFieldLookup doc fieldDecl)

    genFieldLookup ∷ Name → FieldDecl → Exp
    genFieldLookup doc (SimpleFieldDecl simpleName _) =
      ((VarE 'lookupThrow) `AppE` (VarE doc)) `AppE` (LitE $ StringL simpleName)
    genFieldLookup doc (FieldDecl _ simpleName _ _) =
      ((VarE 'lookupThrow) `AppE` (VarE doc)) `AppE` (LitE $ StringL simpleName)



genEntityInstance ∷ TemplateGen ()
genEntityInstance = do
  name       ← gets envTypeName
  (_, decls) ← inner $ do
    genKeyDecls
    genDocumentDecls
    genCollNameDecl
    genFilterDecl
    genEncodeEntity
    genDecodeEntity
  emitDecl $ InstanceD [] (ConT ''MongoEntity `AppT` ConT name) decls

genValueViaShowInstance ∷ TemplateGen ()
genValueViaShowInstance = do
  name ← gets envTypeName
  emitDecl $ InstanceD [] (ConT ''MongoValue `AppT` ConT name)
                          [ FunD (mkName "toValue")
                                 [ Clause []
                                   (NormalB (InfixE (Just (VarE 'toValue)) (VarE '(.)) (Just $ InfixE (Just (VarE 'T.pack)) (VarE '(.)) (Just (VarE 'show)))))
                                   []
                                 ]
                          , FunD (mkName "fromValue")
                                 [ Clause [VarP (mkName "v")]
                                   (NormalB (InfixE (Just (InfixE (Just (VarE 'return)) (VarE '(.))
                                    (Just $ InfixE (Just (VarE 'read)) (VarE '(.)) (Just (VarE 'T.unpack)))))
                                   (VarE '(=<<))
                                   (Just (AppE (VarE 'fromValue) (VarE (mkName "v"))))))
                                   []
                                 ]
                          ]

genValueInstance ∷ TemplateGen ()
genValueInstance = do
  name ← gets envTypeName
  let x   = InfixE (Just $ ConE 'Doc) (VarE '(.)) (Just $ InfixE (Just $ VarE 'fromDocument) (VarE '(.)) (Just $ VarE 'encodeEntity))
      y d = InfixE (Just $ InfixE (Just $ VarE 'decodeEntity) (VarE '(.)) (Just $ VarE 'toDocument)) (VarE '($)) (Just d)
      doc = mkName "doc"
      val = mkName "val"
  emitDecl $ InstanceD [] (ConT ''MongoValue `AppT` ConT name)
                       [ FunD (mkName "toValue") [Clause [] (NormalB x) []]
                       , FunD (mkName "fromValue")
                              [ Clause [ConP 'Doc [VarP doc]] (NormalB $ y (VarE doc)) []
                              , Clause [VarP val] (NormalB (((VarE 'expected) `AppE` (LitE $ StringL "Document")) `AppE` (VarE val))) []
                              ]
                       ]


dropPrefix ∷ String → String
dropPrefix =
  reverse ∘ takeWhile (/= '.') ∘ reverse

capitalize ∷ String → String
capitalize       [] = []
capitalize (c : cs) = toUpper c : cs

uncapitalize ∷ String → String
uncapitalize       [] = []
uncapitalize (c : cs) = toLower c : cs

