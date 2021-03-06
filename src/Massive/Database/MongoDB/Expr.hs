{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TemplateHaskell, TupleSections                                       #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Expr
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Provides a quasi-quote parser for MongoDB expressions.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.Expr ( mongo
                                     ) where

import Prelude.Unicode
import Control.Applicative
import Data.Bson (Field ((:=)), Value (Doc))
import Data.Char (isSpace, digitToInt)
import Data.Text (Text)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Exts as H
import Massive.Database.MongoDB.MongoValue
import Massive.Database.MongoDB.MongoEntity
import Text.Parsec hiding (many, (<|>))

-----------------------------------------------------------------------------------------------------------------------

mongo ∷ QuasiQuoter
mongo = QuasiQuoter { quoteExp  = mongoQuote
                    , quotePat  = undefined
                    , quoteType = undefined
                    , quoteDec  = undefined
                    }

-----------------------------------------------------------------------------------------------------------------------

mongoQuote ∷ String → Q Exp
mongoQuote input = do
  expr ← runParserT parseMongo () "" input
  case expr of
    Left err → error $ show err
    Right  e → return e

-----------------------------------------------------------------------------------------------------------------------

type Parser = ParsecT String () Q

-----------------------------------------------------------------------------------------------------------------------

parseMongo ∷ Parser Exp
parseMongo = do
  ((VarE 'toDocument) `AppE`) <$> (whitespace *> topObjectDef)
  where
    -- Root parser rule is either an object definition as "{ [fieldDef] }"
    -- or just "[fieldDef]".
    topObjectDef =
      objectDef <|> objectFields

-----------------------------------------------------------------------------------------------------------------------

whitespace ∷ Parser ()
whitespace =
  skipMany ∘ satisfy $ isSpace

-----------------------------------------------------------------------------------------------------------------------

lexeme ∷ Parser α → Parser α
lexeme = (<* whitespace)

-----------------------------------------------------------------------------------------------------------------------

-- An objectDef is a list of comma-separated fields surrounded by braces. The
-- resulting expression is of the form ["fieldName" := toValue fieldValue, ...]
objectDef ∷ Parser Exp
objectDef =
     (lexeme (char '{') <?> "'{' at start of JSON object")
  *> (lexeme objectFields) <*
     (lexeme (char '}') <?> "'}' at end of JSON object")

-----------------------------------------------------------------------------------------------------------------------

objectFields ∷ Parser Exp
objectFields =
  ListE <$> sepBy objectField (lexeme $ char ',')

-----------------------------------------------------------------------------------------------------------------------

objectField ∷ Parser Exp
objectField = do
  name ← lexeme (identifier <|> stringLiteral) <?> "identifier for field definition"
  _    ← lexeme (char ':')
  val  ← lexeme (fieldValue <|> arrayValue <|>  (((ConE 'Doc) `AppE`) <$> objectDef)) <?> ("value for field `" ++ name ++ "'")

  return (InfixE (Just ∘ LitE ∘ StringL $ name)
                 (ConE '(:=))
                 (Just (AppE (VarE 'toValue) val)))

-----------------------------------------------------------------------------------------------------------------------

arrayValue ∷ Parser Exp
arrayValue = do
     (lexeme (char '[') <?> "'[' at start of JSON array")
  *> (lexeme arrayElements) <*
     (lexeme (char ']') <?> "']' at end of JSON array")
  where
    arrayElements =
      ListE ∘ map ((VarE 'toValue) `AppE`) <$> sepBy (fieldValue <|> arrayValue <|> objectDef) (lexeme $ char ',')

-----------------------------------------------------------------------------------------------------------------------

fieldValue ∷ Parser Exp
fieldValue =
  (stringValue <|> altBaseIntegerValue <|> numericalValue <|> boolNullValue <|> pasteValue) <?> "field value"
  where
    stringValue =
      ((`SigE` (ConT ''Text)) ∘ LitE ∘ StringL) <$> stringLiteral

    numericalValue = do
      int ← decimal
      rl  ← do
        (Left ∘ (+ fromIntegral int) <$> denom) <|> pure (Right int)
      mEx ← option Nothing (Just <$> (oneOf "eE" *> (option id ((char '-' *> pure negate) <|> (char '+' *> pure id)) <*> decimal)))
      case mEx of
        Just ex → pure $! ((`SigE` (ConT ''Double)) ∘ LitE ∘ RationalL ∘ toRational $ either id fromIntegral rl * ((10.0 ∷ Double) ** fromIntegral ex))
        Nothing →
          case rl of
            Left  f → pure $! ((`SigE` (ConT  ''Double)) ∘ LitE ∘ RationalL ∘ toRational $ f)
            Right i → pure $! ((`SigE` (ConT ''Integer)) ∘ LitE ∘ IntegerL               $ i)

    altBaseIntegerValue =
      ((`SigE` (ConT ''Integer)) ∘ LitE ∘ IntegerL) <$> (hexadecimal <|> octal <|> binary)
    hexadecimal = try (char '0' *> oneOf "xX" *> numberBuilder 16 hexDigit)
    octal       = try (char '0' *> oneOf "oO" *> numberBuilder  8 octDigit)
    binary      = try (char '0' *> oneOf "bB" *> numberBuilder  2 (char '0' <|> char '1'))
    denom       =
      let op = ((/ 10) ∘) ∘ ((+) ∘ (fromIntegral ∘ digitToInt))
      in pure (foldr op 0.0) <*> (char '.' *> many1 digit)

    boolNullValue =
      identifier >>= \i → case i of
        "true"  → pure ∘ ConE $ 'True
        "false" → pure ∘ ConE $ 'False
        "null"  → pure ∘ ConE $ '()
        _       → unexpected i

    pasteValue = do
      text ← char '#' *> betweenBraces
      case H.parseExp text of
        H.ParseOk      expr → return (mapExpToTH expr)
        H.ParseFailed _ msg → parserFail msg

-----------------------------------------------------------------------------------------------------------------------

betweenBraces ∷ Parser String
betweenBraces =
  between (char '{') (char '}') (concat <$> many insideBraces)
  where
    insideBraces =
      ((\s → '{' : s ++ "}") <$> betweenBraces) <|> ((: []) <$> satisfy (/= '}'))

-----------------------------------------------------------------------------------------------------------------------

identifier ∷ Parser String
identifier = try $ do
  i ← ident <?> "identifier"
  case i of
    ('$' : special) →
      if not (special `elem` knownSpecials)
        then parserFail $ "unrecognised special identifier: " ++ i
        else return i
    _ → return i
  where
    ident = do
      c  ← (letter <|> char '_' <|> char '$')
      cs ← many (alphaNum <|> char '_')
      return (c : cs)

    knownSpecials =
      [ "all", "exists", "mod", "ne", "in", "nin", "nor", "or", "and",
        "size", "type", "elemMatch", "not", "where", "returnKey", "maxScan",
        "query", "orderby", "explain", "snapshot", "min", "max", "showDiskLoc",
        "hint", "comment", "lt", "lte", "gt", "gte", "set" ]

-----------------------------------------------------------------------------------------------------------------------

stringLiteral ∷ Parser String
stringLiteral =
  (foldr (maybe id (:)) "" <$> between (char '"') (char '"' <?> "end of string") (many stringChar)) <?> "string literal"

-----------------------------------------------------------------------------------------------------------------------

stringChar ∷ Parser (Maybe Char)
stringChar =
  (Just <$> stringLetter) <|> stringEscape <?> "string character"

-----------------------------------------------------------------------------------------------------------------------

stringLetter ∷ Parser Char
stringLetter =
  satisfy (\c → (c /= '"') && (c /= '\\') && (c > '\026'))

-----------------------------------------------------------------------------------------------------------------------

stringEscape ∷ Parser (Maybe Char)
stringEscape =
  char '\\' *> ((escapeGap   *> return Nothing) <|>
                (escapeEmpty *> return Nothing) <|>
                (Just <$> escapeCode          ))
  where
    escapeEmpty = char '&'
    escapeGap   = many1 space >> (char '\\' <?> "end of string gap")
    escapeCode  = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

    charControl = char '^' *> ((\c → toEnum (fromEnum c - fromEnum 'A')) <$> upper)
    charNum     =
      (toEnum ∘ fromInteger) <$> (decimal <|> (char 'o' *> numberBuilder  8 octDigit)
                                          <|> (char 'x' *> numberBuilder 16 hexDigit))
    charEsc     =
      choice (map parseEsc escMap)
      where
        parseEsc (c, code) = char c *> pure code
    charAscii   =
      choice (map parseAscii asciiMap)
      where
        parseAscii (asc, code) = try (string asc *> pure code)

    escMap      = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
    asciiMap    = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS",
                    "GS", "RS", "US", "SP" ]
    ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE",
                    "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB",
                    "ESC", "DEL" ]
    ascii2      = [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\EM',
                    '\FS', '\GS', '\RS', '\US', '\SP' ]
    ascii3      = [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK', '\BEL',
                    '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB',
                    '\CAN', '\SUB', '\ESC', '\DEL' ]

-----------------------------------------------------------------------------------------------------------------------

numberBuilder ∷ Integer → Parser Char → Parser Integer
numberBuilder base baseDigit = do
  digits ← many1 baseDigit
  let n = foldl (\ x d → base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

-----------------------------------------------------------------------------------------------------------------------

decimal ∷ Parser Integer
decimal = numberBuilder 10 digit

-----------------------------------------------------------------------------------------------------------------------

mapQName :: H.QName -> Name
mapQName (H.Qual modu name) = Name (OccName $ strFromName name) (NameQ (ModName $ strFromModule modu))
mapQName (H.UnQual    name) = mkName (strFromName name)
mapQName (H.Special    con) =
  case con of
    H.UnitCon          -> '()
    H.ListCon          -> '[]
    H.FunCon           -> mkName "(->)"
    H.TupleCon _ n     -> mkName ('(' : replicate n ',' ++ ")")
    H.Cons             -> '(:)
--    H.UnboxedSingleCon -> '(# #)
    H.UnboxedSingleCon -> error "No name for unboxed constructor"

mapName :: H.Name -> Name
mapName = mkName . strFromName

strFromName :: H.Name -> String
strFromName (H.Ident str) = str
strFromName (H.Symbol op) = op

strFromModule :: H.ModuleName -> String
strFromModule (H.ModuleName name) = name

mapDecl :: H.Decl -> [Dec]
mapDecl (H.TypeDecl _ name binds typ) = [TySynD (mapName name) (map mapTypeBind binds) (mapTypeToTH typ)]
mapDecl (H.TypeFamDecl _ name binds mKind) = [FamilyD TypeFam (mapName name) (map mapTypeBind binds) (maybe Nothing (Just . mapKind) mKind)]
mapDecl (H.DataDecl _ H.DataType ctx name binds qConDecl deriv) = [DataD (mapContext ctx) (mapName name) (map mapTypeBind binds)
                                                                         (map mapQCon qConDecl) (map (mapQName . fst) deriv)]
mapDecl (H.DataDecl _ H.NewType  ctx name binds qConDecl deriv) = [NewtypeD (mapContext ctx) (mapName name) (map mapTypeBind binds)
                                                                            (mapQCon (head qConDecl)) (map (mapQName . fst) deriv)]
mapDecl (H.GDataDecl _ _ _ _ _ _ _ _) = error "No support for GADTs in Template Haskell"
mapDecl (H.DataFamDecl _ _ name binds mKind) = [FamilyD DataFam (mapName name) (map mapTypeBind binds) (maybe Nothing (Just . mapKind) mKind)]
mapDecl (H.TypeInsDecl _ _ _) = error "No support for type instances"
mapDecl (H.DataInsDecl _ H.DataType _ _ _) = error "No support for data instances"
mapDecl (H.DataInsDecl _ H.NewType  _   _        _    ) = error "No support for data instances"
mapDecl (H.GDataInsDecl _ _ _ _ _ _) = error "No support for GADTs in Template Haskell"
mapDecl (H.ClassDecl _ ctx name binds funDeps classDecls) = [ClassD (mapContext ctx) (mapName name) (map mapTypeBind binds)
                                                                    (map mapFunDep funDeps) (map mapClassDecl classDecls)]
mapDecl (H.InstDecl _ _ _ ctx _ types instDecls) = [InstanceD (mapContext ctx) (mapTypeToTH (head types)) (map mapInstDecl instDecls)]
mapDecl (H.DerivDecl _ _ _ _ _ _) = error "No support for standalone deriving declarations in Template Haskell"
mapDecl (H.InfixDecl _ _ _ _) = error "No support for operator fixity declarations in Template Haskell"
mapDecl (H.DefaultDecl _ _) = error "No support for default declarations in Template Haskell"
mapDecl (H.SpliceDecl _ _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapDecl (H.TypeSig _ names typ) = map (flip SigD (mapTypeToTH typ) . mapName) names
mapDecl (H.FunBind matches) = [FunD (matchName (head matches)) (map mapClause matches)]
mapDecl (H.PatBind _ pat rhs binds) = [ValD (mapPatToTH pat) (mapRhs rhs) (mapBinds binds)]
mapDecl (H.ForImp _ cc safe str name typ) = [ForeignD (ImportF (mapCC cc) (mapSafety safe) str (mapName name) (mapTypeToTH typ))]
mapDecl (H.ForExp _ cc str name typ) = [ForeignD (ExportF (mapCC cc) str (mapName name) (mapTypeToTH typ))]
mapDecl (H.RulePragmaDecl _ _) = error "No support for RULES pragma in Template Haskell"
mapDecl (H.DeprPragmaDecl _ _) = error "No support for DEPRECATED pragma in Template Haskell"
mapDecl (H.WarnPragmaDecl _ _) = error "No support for WARNING pragma in Template Haskell"
--mapDecl (H.InlineSig _ _ _ name) = [PragmaD (InlineP (mapQName name) (InlineSpec True False Nothing))]
mapDecl (H.InlineSig _ _ _ name) = [PragmaD (InlineP (mapQName name) Inline ConLike AllPhases)]
mapDecl (H.InlineConlikeSig _ _ _) = error "No current support for INLINE CONLIKE pragma"
mapDecl (H.SpecSig _ _ name types) = [PragmaD (SpecialiseP (mapQName name) (mapTypeToTH (head types)) Nothing AllPhases)]
--mapDecl (H.SpecInlineSig _ _ _ name types) = [PragmaD (SpecialiseP (mapQName name) (mapTypeToTH (head types)) (Just (InlineP (mapQName name) Inline ConLike AllPhases)) AllPhases)]
--mapDecl (H.SpecInlineSig _ _ _ name types) = [PragmaD (SpecialiseP (mapQName name) (mapTypeToTH (head types)) (Just (InlineP (mapQName name) Inline ConLike AllPhases)) AllPhases)]
mapDecl (H.SpecInlineSig _ _ _ _ _) = error "No support for inline specialisation signatures(???)"
mapDecl (H.InstSig _ _ _ _ _) = error "No support for SPECIALISE instance pragma in Template Haskell (I think...)"
mapDecl (H.AnnPragma _ _) = error "No support for ANN pragma in Template Haskell"

mapCC :: H.CallConv -> Callconv
mapCC (H.StdCall) = StdCall
mapCC (H.CCall  ) = CCall
mapCC other = error $ "no support for calling convention: " ++ show other

mapSafety :: H.Safety -> Safety
mapSafety (H.PlayRisky ) = Unsafe
mapSafety (H.PlaySafe t) = if t then Interruptible else Safe
mapSafety (H.PlayInterruptible) = Interruptible

matchName :: H.Match -> Name
matchName (H.Match _ name _ _ _ _) = mapName name

mapClause :: H.Match -> Clause
mapClause (H.Match _ _ pats _ rhs binds) = Clause (map mapPatToTH pats) (mapRhs rhs) (mapBinds binds)

mapRhs :: H.Rhs -> Body
mapRhs (H.UnGuardedRhs     e) = NormalB (mapExpToTH e)
mapRhs (H.GuardedRhss guards) =
  GuardedB (map mapGRhs guards)
  where
    mapGRhs (H.GuardedRhs _ stmts e) = (PatG (map mapStmt stmts), mapExpToTH e)


mapInstDecl :: H.InstDecl -> Dec
mapInstDecl (H.InsDecl decl) = head (mapDecl decl)
mapInstDecl _                = error "Unsupported"

mapClassDecl :: H.ClassDecl -> Dec
mapClassDecl (H.ClsDecl decl) = head (mapDecl decl)
mapClassDecl _                = error "Unsupported"

mapFunDep :: H.FunDep -> FunDep
mapFunDep (H.FunDep xs ys) = FunDep (map mapName xs) (map mapName ys)

mapQCon :: H.QualConDecl -> Con
mapQCon (H.QualConDecl _ []    []  conDecl) = mapCon conDecl
mapQCon (H.QualConDecl _ binds ctx conDecl) = ForallC (map mapTypeBind binds) (mapContext ctx) (mapCon conDecl)

mapCon :: H.ConDecl -> Con
mapCon (H.ConDecl     name args) = NormalC (mapName name) (map ((NotStrict, ) . mapTypeToTH) args)
mapCon (H.InfixConDecl x name y) = InfixC (NotStrict, mapTypeToTH x) (mapName name) (NotStrict, mapTypeToTH y)
mapCon (H.RecDecl   name fields) = RecC (mapName name) $ map (uncurry mapFieldDecl) $ concatMap (uncurry ((. repeat) . zip)) fields

mapFieldDecl :: H.Name -> H.Type -> VarStrictType
mapFieldDecl name bType = let (strict, typ) = (NotStrict, mapTypeToTH bType)
                          in (mapName name, strict, typ)

-- mapBangType :: H.BangType -> StrictType
-- mapBangType (H.BangedTy   t) = (IsStrict,  mapTypeToTH t)
-- --mapBangType (H.UnBangedTy t) = (NotStrict, mapTypeToTH t)
-- mapBangType (H.UnpackedTy _) = error "No support for unboxed type (via UNPACK pragma) in Template Haskell"


mapExpToTH :: H.Exp -> Exp
mapExpToTH (H.Var             name) = VarE (mapQName name)
mapExpToTH (H.IPVar              _) = error "No implicit parameter support"
mapExpToTH (H.Con             name) = ConE (mapQName name)
mapExpToTH (H.Lit              lit) = LitE (mapLitToTH lit)
mapExpToTH (H.InfixApp      l op r) = InfixE (Just $ mapExpToTH l) (mapQOpToTH op) (Just $ mapExpToTH r)
mapExpToTH (H.App              l r) = (mapExpToTH l) `AppE` (mapExpToTH r)
mapExpToTH (H.NegApp             o) = AppE (VarE 'negate) (mapExpToTH o)
mapExpToTH (H.Lambda       _ pat e) = LamE (map mapPatToTH pat) (mapExpToTH e)
mapExpToTH (H.Let             bs e) = LetE (mapBinds bs) (mapExpToTH e)
mapExpToTH (H.If             e t f) = CondE (mapExpToTH e) (mapExpToTH t) (mapExpToTH f)
mapExpToTH (H.Case            e ms) = CaseE (mapExpToTH e) (map mapAlt ms)
mapExpToTH (H.Do                 s) = DoE (map mapStmt s)
mapExpToTH (H.MDo                _) = error "No support for mdo expressions"
mapExpToTH (H.Tuple           _ es) = TupE (map mapExpToTH es)
mapExpToTH (H.TupleSection     _ _) = error "Tuple sections currently not supported by template haskell"
mapExpToTH (H.List              es) = ListE (map mapExpToTH es)
mapExpToTH (H.Paren              e) = mapExpToTH e
mapExpToTH (H.LeftSection      e o) = InfixE (Just (mapExpToTH e)) (mapQOpToTH o) Nothing
mapExpToTH (H.RightSection     o e) = InfixE Nothing (mapQOpToTH o) (Just (mapExpToTH e))
mapExpToTH (H.RecConstr       n fs) = RecConE (mapQName n) (map mapFieldUpdate fs)
mapExpToTH (H.RecUpdate       e fs) = RecUpdE (mapExpToTH e) (map mapFieldUpdate fs)
mapExpToTH (H.EnumFrom           e) = ArithSeqE (FromR (mapExpToTH e))
mapExpToTH (H.EnumFromTo       x y) = ArithSeqE (FromToR (mapExpToTH x) (mapExpToTH y))
mapExpToTH (H.EnumFromThen     x y) = ArithSeqE (FromThenR (mapExpToTH x) (mapExpToTH y))
mapExpToTH (H.EnumFromThenTo x y z) = ArithSeqE (FromThenToR (mapExpToTH x) (mapExpToTH y) (mapExpToTH z))
mapExpToTH (H.ListComp        e qs) = CompE (map mapQualStmt qs ++ [NoBindS $ mapExpToTH e])
mapExpToTH (H.ParComp          _ _) = error "No support for parallel list comprehensions in Template Haskell"
mapExpToTH (H.ExpTypeSig     _ e t) = SigE (mapExpToTH e) (mapTypeToTH t)
mapExpToTH (H.VarQuote           _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapExpToTH (H.TypQuote           _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapExpToTH (H.BracketExp         _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapExpToTH (H.SpliceExp          _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapExpToTH (H.QuasiQuote       _ _) = error "Template Haskell brackets cannot be nested (without intervening splices)"
mapExpToTH (H.XTag       _ _ _ _ _) = error "No support for XML extension in Template Haskell"
mapExpToTH (H.XETag        _ _ _ _) = error "No support for XML extension in Template Haskell"
mapExpToTH (H.XPcdata            _) = error "No support for XML extension in Template Haskell"
mapExpToTH (H.XExpTag            _) = error "No support for XML extension in Template Haskell"
mapExpToTH (H.XChildTag        _ _) = error "No support for XML extension in Template Haskell"
mapExpToTH (H.CorePragma       _ _) = error "No support for pragmas in Template Haskell"
mapExpToTH (H.SCCPragma        _ _) = error "No support for pragmas in Template Haskell"
mapExpToTH (H.GenPragma    _ _ _ _) = error "No support for pragmas in Template Haskell"
mapExpToTH (H.Proc           _ _ _) = error "No support for arrows 'proc' in Template Haskell"
mapExpToTH (H.LeftArrApp       _ _) = error "No support for left arrow application in Template Haskell"
mapExpToTH (H.RightArrApp      _ _) = error "No support for right arrow application in Template Haskell"
mapExpToTH (H.LeftArrHighApp   _ _) = error "No support for higher-order left arrow application in Template Haskell"
mapExpToTH (H.RightArrHighApp  _ _) = error "No support for higher-order right arrow application in Template Haskell"


mapQualStmt :: H.QualStmt -> Stmt
mapQualStmt (H.QualStmt stmt) = mapStmt stmt
mapQualStmt _                 = error "No support for SQL-like generalized list comprehensions (not supported by Template Haskell)"

mapFieldUpdate :: H.FieldUpdate -> FieldExp
mapFieldUpdate (H.FieldUpdate n e) = (mapQName n, mapExpToTH e)
mapFieldUpdate (H.FieldPun      _) = error "No support for field puns in update expressions"
mapFieldUpdate (H.FieldWildcard  ) = error "No support for field wildcards in update expressions"

mapAlt :: H.Alt -> Match
mapAlt (H.Alt _ p g bs) = Match (mapPatToTH p) (mapGuard g) (mapBinds bs)

mapGuard :: H.Rhs -> Body
mapGuard (H.UnGuardedRhs e) = NormalB (mapExpToTH e)
mapGuard (H.GuardedRhss gs) =
  GuardedB (map mapGAlt gs)
  where
    mapGAlt (H.GuardedRhs _ stmts e) = (PatG (map mapStmt stmts), mapExpToTH e)

mapStmt :: H.Stmt -> Stmt
mapStmt (H.Generator _ p e) = BindS (mapPatToTH p) (mapExpToTH e)
mapStmt (H.Qualifier     e) = NoBindS (mapExpToTH e)
mapStmt (H.LetStmt      bs) = LetS (mapBinds bs)
mapStmt (H.RecStmt      rs) = ParS [map mapStmt rs]

mapBinds :: H.Binds -> [Dec]
mapBinds (H.BDecls decls) = concatMap mapDecl decls
mapBinds (H.IPBinds    _) = error "No support for implicit parameter bindings"

mapQOpToTH :: H.QOp -> Exp
mapQOpToTH (H.QVarOp name) = VarE (mapQName name)
mapQOpToTH (H.QConOp name) = ConE (mapQName name)

{-
mapQOpToTHT :: H.QOp -> Type
mapQOpToTHT (H.QVarOp name) = VarT (mapQName name)
mapQOpToTHT (H.QConOp name) = ConT (mapQName name)
-}

mapPatToTH :: H.Pat -> Pat
mapPatToTH (H.PVar        name) = VarP (mapName name)
mapPatToTH (H.PLit       _ lit) = LitP (mapLitToTH lit)
--mapPatToTH (H.PNeg           _) = error "What?! (http://trac.haskell.org/haskell-src-exts/ticket/209)"
mapPatToTH (H.PNPlusK      _ _) = error "No support for N+K patterns"
mapPatToTH (H.PInfixApp  l n r) = InfixP (mapPatToTH l) (mapQName n) (mapPatToTH r)
mapPatToTH (H.PApp         n p) = ConP (mapQName n) (map mapPatToTH p)
mapPatToTH (H.PTuple       _ p) = TupP (map mapPatToTH p)
mapPatToTH (H.PList          p) = ListP (map mapPatToTH p)
mapPatToTH (H.PParen         p) = mapPatToTH p
mapPatToTH (H.PRec        n pf) = RecP (mapQName n) (map mapPatFieldToTH pf)
mapPatToTH (H.PAsPat       n p) = AsP (mapName n) (mapPatToTH p)
mapPatToTH (H.PWildCard       ) = WildP
mapPatToTH (H.PIrrPat        p) = TildeP (mapPatToTH p)
mapPatToTH (H.PatTypeSig _ p t) = SigP (mapPatToTH p) (mapTypeToTH t)
mapPatToTH (H.PViewPat     _ _) = error "No support for view patterns"
mapPatToTH (H.PRPat          _) = error "I don't know what a PR pattern is"
mapPatToTH (H.PXTag  _ _ _ _ _) = error "No support for XML"
mapPatToTH (H.PXETag   _ _ _ _) = error "No support for XML"
mapPatToTH (H.PXPcdata       _) = error "No support for XML"
mapPatToTH (H.PXPatTag       _) = error "No support for XML"
mapPatToTH (H.PXRPats        _) = error "No support for XML"
--mapPatToTH (H.PExplTypeArg _ _) = error "No support for explicit type arguments"
mapPatToTH (H.PQuasiQuote  _ _) = error "No support for quasi-quotation"
mapPatToTH (H.PBangPat       _) = error "No support for bang patterns"

mapTypeToTH :: H.Type -> Type
mapTypeToTH (H.TyForall vb ctx t) = ForallT (maybe [] (map mapTypeBind) vb) (mapContext ctx) (mapTypeToTH t)
mapTypeToTH (H.TyFun         l r) = AppT (AppT ArrowT (mapTypeToTH l)) (mapTypeToTH r)
mapTypeToTH (H.TyTuple      _ ts) = foldl AppT (TupleT (length ts)) (map mapTypeToTH ts)
mapTypeToTH (H.TyList          t) = AppT ListT (mapTypeToTH t)
mapTypeToTH (H.TyApp         l r) = AppT (mapTypeToTH l) (mapTypeToTH r)
mapTypeToTH (H.TyVar           v) = VarT (mapName v)
mapTypeToTH (H.TyCon           c) = ConT (mapQName c)
mapTypeToTH (H.TyParen         t) = mapTypeToTH t
mapTypeToTH (H.TyInfix    l op r) = AppT (AppT (ConT $ mapQName op) (mapTypeToTH l)) (mapTypeToTH r)
mapTypeToTH (H.TyKind        _ _) = error "No support for types with explicit type kinds"

mapTypeBind :: H.TyVarBind -> TyVarBndr
mapTypeBind (H.KindedVar n k) = KindedTV (mapName n) (mapKind k)
mapTypeBind (H.UnkindedVar n) = PlainTV (mapName n)

mapKind :: H.Kind -> Kind
mapKind (H.KindStar   ) = StarT
mapKind (H.KindBang   ) = error "No support for bang-kinds"
mapKind (H.KindFn  x y) = AppT (AppT ArrowT (mapKind x)) (mapKind y)
mapKind (H.KindParen k) = mapKind k
mapKind (H.KindVar   _) = error "No support for kind variables"

mapContext :: H.Context -> Cxt
mapContext =
  map mapAssert
  where
    mapAssert (H.ClassA  q ts) = ClassP (mapQName q) (map mapTypeToTH ts)
    mapAssert (H.InfixA x q y) = ClassP (mapQName q) [mapTypeToTH x, mapTypeToTH y]
    mapAssert (H.IParam   _ _) = error "No support for implicit parameter assertion"
    mapAssert (H.EqualP   x y) = EqualP (mapTypeToTH x) (mapTypeToTH y)

mapPatFieldToTH :: H.PatField -> FieldPat
mapPatFieldToTH (H.PFieldPat name pat) = (mapQName name, mapPatToTH pat)
mapPatFieldToTH (H.PFieldPun        _) = error "field puns not yet supported"
mapPatFieldToTH (H.PFieldWildcard    ) = (mkName "", WildP)

mapLitToTH :: H.Literal -> Lit
mapLitToTH (H.Char       c) = CharL       c
mapLitToTH (H.String     s) = StringL     s
mapLitToTH (H.Int        i) = IntegerL    i
mapLitToTH (H.Frac       r) = RationalL   r
mapLitToTH (H.PrimInt    i) = IntPrimL    i
mapLitToTH (H.PrimWord   w) = WordPrimL   w
mapLitToTH (H.PrimFloat  f) = FloatPrimL  f
mapLitToTH (H.PrimDouble d) = DoublePrimL d
mapLitToTH (H.PrimChar   c) = CharL       c
mapLitToTH (H.PrimString s) = StringPrimL (map (fromIntegral ∘ fromEnum) s)

