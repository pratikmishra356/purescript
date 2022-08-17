{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.PureScript.TypeMerge where

import Prelude

-- import           Control.Applicative
-- import           Control.Monad
-- import qualified Data.Aeson as A
-- import           Data.Bool (bool)
-- import qualified Data.ByteString.Lazy.UTF8 as LBU8
-- import           Data.List (intercalate)
-- import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.IO.Class(liftIO)
-- import           Data.Traversable (for)
-- import qualified Language.PureScript as P
import qualified Data.Map as M
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Names as N
-- import           Language.PureScript.Errors.JSON
-- import           Language.PureScript.Make
-- import qualified System.Console.ANSI as ANSI
-- import           System.Exit (exitSuccess, exitFailure)
-- import           System.Directory (getCurrentDirectory)
-- import           System.FilePath.Glob (glob)
-- import           System.IO (hPutStr, hPutStrLn, stderr, stdout)
-- import           System.IO.UTF8 (readUTF8FilesT)
import Data.Foldable as DF
import Data.Maybe (isJust,fromJust,fromMaybe)
import Language.PureScript.PSString (PSString)
import Data.Text (Text)
import Data.List as DL
import qualified Language.PureScript.PSString as PSString
import Language.PureScript.ImportDecl (travImportDecl, travExportDecl)

filePathTypeName :: FilePath -> Bool -> T.Text
filePathTypeName fp isType = do 
    let pl = fromMaybe "State" (T.stripSuffix ".purs" (T.pack fp))
    let po = T.split (=='/') pl
    let lenList = DL.length po
    let typeText = T.concat $ DL.drop (lenList-2) po
    if isType
      then T.append (T.toUpper $ T.take 1 typeText) (T.drop 1 typeText)
      else T.append (T.toLower $ T.take 1 typeText) (T.drop 1 typeText)

createType :: FilePath -> [(PSString,Maybe Text)] -> T.Text
createType fp typeList = do
  let mapJust = DL.map (\(a,b) -> (PSString.decodeString a,b)) typeList
  let filterJust = DL.map (\(c,d) -> (T.unpack $ fromJust c,T.unpack $ fromJust d) ) (DL.filter (\(a,b)-> isJust a && isJust b) mapJust)

  let concatList = DL.drop 1 (DF.concatMap (\(a,b)-> ", " <> a <> " :: "<> b <> "\n" ) filterJust)
  "type "<> filePathTypeName fp True <> " = " <> "{\n" <> T.pack concatList <> "}\n"

travCst :: [(FilePath, T.Text)] -> IO()
travCst modfi = do
    for_ modfi (\(_, a) -> do
      case CST.parseModule (CST.lex a) of
        Left _ -> liftIO $ putStrLn ""
        Right y -> (case snd (CST.resFull y) of
              Right z ->  do
                let modna = CST.nameValue $ CST.modNamespace z
                if T.isInfixOf (T.pack "Main") (T.pack (show modna))
                  then do
                    let _ = CST.Module {
                       modAnn =  CST.modAnn z
                      , modKeyword = CST.modKeyword z
                      , modNamespace = CST.modNamespace z
                      , modExports = CST.modExports z
                      , modWhere = CST.modWhere z
                      , modImports = CST.modImports z
                      , modDecls = CST.modDecls z
                      , modTrailingComments = CST.modTrailingComments z
                      }
                    liftIO $ writeFile ("cst_out_"++(show modna)++".txt") (show z++"\n")
                  else liftIO $ putStrLn ""
              Left _ -> putStrLn "" )
        )
pathModuleMap :: [(FilePath, T.Text)] -> M.Map FilePath (CST.Module ())
pathModuleMap moduleFiles = do
    let _ = M.empty
    let plo = map (\(j, a) -> do
            case (CST.parseModule (CST.lex a)) of
                Left _ -> (j,Nothing)
                Right y -> case (snd (CST.resFull y)) of
                    Right z -> do
                        let _ = CST.nameValue $ CST.modNamespace z
                        if (T.isInfixOf (T.pack "Controller") (T.pack (show j))) || (T.isInfixOf (T.pack "App.State.purs") (T.pack (show j)))
                            then (j,Just z)
                            else (j,Nothing)
                    Left _ -> (j,Nothing)
                ) moduleFiles
    let kl = M.fromList plo
    let ml = M.filterWithKey (\_ a -> isJust a) kl
    M.map (\a -> fromJust a) ml

typeUnion :: Maybe Text -> Maybe Text -> Maybe Text
typeUnion a b = if a == b
                  then a
                  else Nothing

filterFPDeclMap :: [(Text,[(PSString, Maybe Text)])] -> Maybe [(PSString, Maybe Text)]
filterFPDeclMap a = do
  if length a == 1
    then Just (head (map snd a))
  else Nothing
mergeTypes :: M.Map FilePath (CST.Module ()) -> IO ()
mergeTypes cstFileMap = do
  let mapTypeDecl = M.map (\a -> do 
        let modNam = CST.nameValue $ CST.modNamespace a
        let importMap =  (map (\imd -> fromMaybe [] (travImportDecl imd)) (CST.modImports a))
        let expMap = map (\ex -> fromMaybe [] (travExportDecl ex modNam)) (CST.modDecls a)
        let impExpMap = M.fromList $ DL.concat (importMap ++ expMap)
        (CST.modDecls a, impExpMap) ) cstFileMap
  let laebelTyp = M.map (\(a,k) -> map (\b-> (getTypeName b , travNameVal (travRow $ travTypeRow $ travDecl b) k) ) a ) mapTypeDecl
  --let laebelTyp = M.map (\a -> map (\b-> (getTypeName b , travNameVal $ travRow $ travTypeRow $ travDecl b) ) a ) mapTypeDecl
  let filterMaybeList = M.map (\a -> filter (\(b,c)-> isJust b && isJust c )a) laebelTyp
  let mapMaybeList = M.map (\a -> map (\(b,c)-> (fromJust  b , fromJust  c ))a) filterMaybeList
  let stateTypeList = M.map (\a -> filter (\(b,_)-> (b=="State" || b=="AppState")) a) mapMaybeList
  let filteredStateMap = M.mapMaybe filterFPDeclMap stateTypeList
  let mapValue = map (\a-> (a,1::Int))(concat $ M.elems filteredStateMap)
  let mergeMap = M.fromListWith (+) (mapValue)
  let mergedType = M.keys $ M.filter (> (1::Int)) mergeMap
  let notMergedType = M.keys $ M.filter (== (1::Int)) mergeMap

  let multipleTypeMap = M.filter (\c-> not (null c))(M.map (\a -> DL.filter (\b -> DL.elem b notMergedType) a ) filteredStateMap)
  -- let stateList = filter (\a-> length a >0) stateTypeList
  -- let stateTL = map (\a-> a!!0) stateList
  -- let kl = map snd stateTL
  -- let stateMapList = map (\a -> M.fromList a) kl
  -- let unionState = M.unionsWith (typeUnion) stateMapList
  let appStateList = mergedType ++ DL.map (\a -> (PSString.mkString (filePathTypeName a False)<>"State",Just (filePathTypeName a True))) (M.keys multipleTypeMap)
  let mulTypeText = (T.concat (DL.map (\(a,b) -> createType a b) (M.toList multipleTypeMap)))
  writeFile "MulType.txt" (T.unpack $ T.append mulTypeText (createType "State" appStateList))
  putStrLn (show mergedType)
  putStrLn (show notMergedType)
  putStrLn (show multipleTypeMap)
  putStrLn (show appStateList)



-- mergeTypes :: M.Map FilePath (CST.Module ()) -> IO ()
-- mergeTypes cstFileMap = do
--   let cstModuleList = map (\(_,b) -> CST.modDecls b ) (M.toList cstFileMap)
--   let typeList = map (\a -> map (\b-> (getTypeName b , travNameVal $ travRow $ travTypeRow $ travDecl b)) a) cstModuleList
--   --let laebelTyp = M.map (\a -> map (\b-> (getTypeName b , travNameVal $ travRow $ travTypeRow $ travDecl b) ) a ) mapTypeDecl
--   let filterMaybeList = map (\a -> filter (\(b,c)-> isJust b && isJust c )a) typeList
--   let mapMaybeList = map (\a -> map (\(b,c)-> (fromJust  b , fromJust  c ))a) filterMaybeList
--   let stateTypeList = map (\a -> filter (\(b,_)-> b=="State") a) mapMaybeList
--   let stateList = filter (\a-> length a >0) stateTypeList
--   let stateTL = map (\a-> a!!0) stateList
--   let kl = map snd stateTL
--   let stateMapList = map (\a -> M.fromList a) kl
--   let unionState = M.unionsWith (typeUnion) stateMapList
--   putStrLn (show unionState)


-- mergeTypes :: M.Map FilePath (CST.Module ()) -> IO ()
-- mergeTypes cstFileMap = do
--     let mapTypeDecl = M.map (\a -> CST.modDecls a ) cstFileMap
--     let laebelTyp = M.map (\a -> map (\b-> (getTypeName b , travNameVal $ travRow $ travTypeRow $ travDecl b) ) a ) mapTypeDecl
--     let pl = M.map (\a -> filter (\(b,c) -> isJust b && isJust c) a) laebelTyp
--     -- let ml = M.map (\a -> M.fromList $ map (\(b,c) ->(fromJust b ,M.fromList $ fromJust c)) a) pl
--     let filterMaybe = M.map (\a -> map (\(b,c)-> (fromJust b, fromJust c)) a) pl

--     let filterState = map snd $ filter (\(a,_)-> a=="State") (( map (\(_,b)-> b) (M.toList filterMaybe))!!0)
--     let mapState = map (\a-> M.fromList a) filterState
--     putStrLn (show filterMaybe)
--     let unionState = M.unionsWith (typeUnion) mapState
--     putStrLn (show (IP.mergeImport cstFileMap))
--     putStrLn (show unionState)

getTypeName :: forall a. CST.Declaration a -> Maybe Text
getTypeName (CST.DeclType _ p _ _) = Just (N.runProperName $ CST.nameValue $ CST.dataHdName p)
getTypeName _ = Nothing

travDecl :: forall a. CST.Declaration a -> Maybe (CST.Type a)
travDecl (CST.DeclType _ _ _ pl) = Just pl
travDecl _ = Nothing

travTypeRow :: forall a. Maybe (CST.Type a) ->  Maybe (CST.Separated (CST.Labeled CST.Label (CST.Type a)))
travTypeRow (Just (CST.TypeRecord _ p)) = CST.rowLabels $ CST.wrpValue p
travTypeRow _ = Nothing

travRow :: forall a. Maybe (CST.Separated (CST.Labeled CST.Label (CST.Type a))) -> Maybe [(CST.Labeled CST.Label (CST.Type a))]
travRow (Just (CST.Separated k t)) = Just (k:map snd t)
travRow _ = Nothing

checkModuleName :: Maybe N.ModuleName -> Text -> M.Map Text Text -> Text
checkModuleName (Just (N.ModuleName p)) tyName mn = case M.lookup p mn of
                                              Nothing -> case M.lookup tyName mn of
                                                  Nothing -> tyName
                                                  Just y -> T.append (T.append y ".") tyName
                                              Just x -> T.append (T.append x ".") tyName
checkModuleName _ tyName mn =  case M.lookup tyName mn of
                                  Nothing -> tyName
                                  Just y -> T.append (T.append y ".") tyName


-- checkQualifiedImpModule :: Text -> M.Map Text Text -> Text
-- checkQualifiedImpModule typName mn = case M.lookup typName mn of
--                                       Nothing -> typName
--                                       Just x -> T.append (T.append x ".") typName

-- travTypeConstName :: forall a. CST.Type a -> Maybe T.Text
-- travTypeConstName (CST.TypeConstructor _ (CST.QualifiedName _ m p)) =  Just(T.append (checkModuleName m) (N.runProperName p))
-- travTypeConstName (CST.TypeApp _ f s) = Just(T.append (fromMaybe "" (travTypeConstName f)) $ T.append " " ( fromMaybe "" (travTypeConstName s)))
-- travTypeConstName _ = Nothing
-- l
-- travTypeConstName :: forall a. CST.Type a -> Maybe T.Text
-- travTypeConstName (CST.TypeConstructor _ (CST.QualifiedName _ m p)) =  Just(T.append (checkModuleName m) (N.runProperName p))
-- travTypeConstName (CST.TypeApp _ f s) = Just(T.append (fromMaybe "" (travTypeConstName f)) $ T.append " " ( fromMaybe "" (travTypeConstName s)))
-- travTypeConstName (CST.TypeRecord _ p) =  Just $ T.concat (maybe [] (map (\(a,b)-> T.unwords ["(",fromMaybe "" (PSString.decodeString a),",",fromMaybe "" b, ")"])) (travNameVal $ travRow $ CST.rowLabels $ CST.wrpValue p))
-- travTypeConstName _ = Nothing

travTypeConstName :: forall a. CST.Type a -> M.Map Text Text -> Maybe T.Text
travTypeConstName (CST.TypeConstructor _ (CST.QualifiedName _ m p)) mn =  Just (checkModuleName m (N.runProperName p) mn )
travTypeConstName (CST.TypeApp _ f s) mn = Just(T.append (fromMaybe "" (travTypeConstName f mn)) $ T.append " " ( fromMaybe "" (travTypeConstName s mn)))
travTypeConstName (CST.TypeRecord _ p) fp =  do
    case travNameVal (travRow $ CST.rowLabels $ CST.wrpValue p) fp of
      Nothing -> Nothing
      Just x -> Just $ T.append (T.append "{ \n" (T.drop 1 (T.concat (map (\(a,b)-> T.unwords [", ",fromMaybe "" (PSString.decodeString a)," :: ",fromMaybe "" b, "\n"]) x ))) )" }\n"
travTypeConstName _ _ = Nothing
--Just (DF.concatMap (\(a,b)-> ", " <> fromMaybe "" (PSString.decodeString a) <> " :: " <> fromMaybe "" b ) (fromMaybe [] (travNameVal $ travRow $ CST.rowLabels $ CST.wrpValue p)))
-- DL.drop 1 (DF.concatMap (\(a,b)-> ", " <> a <> " :: "<> b <> "\n" ) filterJust)

travNameVal :: forall a. Maybe [CST.Labeled CST.Label (CST.Type a)] -> M.Map Text Text  -> Maybe [(PSString, Maybe T.Text)]
travNameVal (Just p) mn = Just (map (\(CST.Labeled k _ t)-> (CST.lblName k , travTypeConstName t mn)) p)
travNameVal _ _ = Nothing


-- travDecl :: CST.Declaration () -> Maybe (CST.Type ())
-- travDecl (CST.DeclType _ _ _ pl) = Just pl
-- travDecl _ = Nothing

-- travTypeRow :: Maybe (CST.Type ()) ->  Maybe (CST.Separated (CST.Labeled CST.Label (CST.Type ())))
-- travTypeRow (Just (CST.TypeRecord _ p)) = CST.rowLabels $ CST.wrpValue p
-- travTypeRow _ = Nothing

-- travRow :: Maybe (CST.Separated (CST.Labeled CST.Label (CST.Type ()))) -> Maybe (CST.Labeled CST.Label (CST.Type ()))
-- travRow (Just (CST.Separated k _)) = Just k
-- travRow _ = Nothing

-- travTypeConstName :: CST.Type () -> Maybe T.Text
-- travTypeConstName (CST.TypeConstructor _ (CST.QualifiedName _ _ p)) = Just( N.runProperName p)
-- travTypeConstName _ = Nothing

-- travNameVal :: Maybe (CST.Labeled CST.Label (CST.Type ())) -> Maybe (PSString, Maybe T.Text)
-- travNameVal (Just (CST.Labeled k _ t)) = Just (CST.lblName k , travTypeConstName t)
-- travNameVal _ = Nothing
-- mergeTypes :: M.Map FilePath (CST.Module ()) -> IO ()
-- mergeTypes cstFileMap = do
--     let mapTypeDecl = M.map (\a -> CST.modDecls a ) cstFileMap
--     let laebelTyp = M.map (\a -> map (\b-> travNameVal $ travRow $ CST.rowLabels $ travTypeRow $ travDecl b ) a ) mapTypeDecl
--     putStrLn (show laebelTyp)

-- travDecl :: CST.Declaration () -> CST.Type ()
-- travDecl (CST.DeclType _ _ _ pl) = pl
-- travDecl _ = _

-- travTypeRow :: CST.Type () ->  CST.Row ()
-- travTypeRow (CST.TypeRecord _ p) = CST.wrpValue p

-- travTypeConstName :: CST.Type () -> T.Text
-- travTypeConstName (CST.TypeConstructor _ (CST.QualifiedName _ _ p)) =  N.runProperName p
-- travTypeConstName _ = ""

-- travRow :: Maybe (CST.Separated (CST.Labeled CST.Label (CST.Type ()))) -> CST.Labeled CST.Label (CST.Type ())
-- travRow (Just (CST.Separated k _)) = k

-- travLabel :: CST.Labeled CST.Label (CST.Type ()) -> PSString
-- travLabel (CST.Labeled l _ _) = CST.lblName l

-- travLabType :: CST.Labeled CST.Label (CST.Type ()) -> CST.Type()
-- travLabType (CST.Labeled _ _ t) = t

-- travNameVal :: CST.Labeled CST.Label (CST.Type ()) -> (PSString, T.Text)
-- travNameVal (CST.Labeled k _ t) = (CST.lblName k ,travTypeConstName t)

-- travWrap :: CST.Wrapped (CST.Row ()) -> CST.Wrapped (CST.Row ())
-- travWrap (CST.TypeRecord _ p) = p
-- travWrap _ = _
-- let newmod = CST.Module {
--     modAnn =  CST.modAnn z
--     , modKeyword = CST.modKeyword z
--     , modNamespace = CST.modNamespace z
--     , modExports = CST.modExports z
--     , modWhere = CST.modWhere z
--     , modImports = CST.modImports z
--     , modDecls = CST.modDecls z
--     , modTrailingComments = CST.modTrailingComments z
--     }