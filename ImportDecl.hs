{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Language.PureScript.ImportDecl where

import Prelude

-- import           Control.Applicative
-- import           Control.Monad
-- import qualified Data.Aeson as A
-- import           Data.Bool (bool)
-- import qualified Data.ByteString.Lazy.UTF8 as LBU8
-- import           Data.List (intercalate)
-- import qualified Data.Set as S
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

-- import           Language.PureScript.Errors.JSON
-- import           Language.PureScript.Make
-- import qualified System.Console.ANSI as ANSI
-- import           System.Exit (exitSuccess, exitFailure)
-- import           System.Directory (getCurrentDirectory)
-- import           System.FilePath.Glob (glob)
-- import           System.IO (hPutStr, hPutStrLn, stderr, stdout)
-- import           System.IO.UTF8 (readUTF8FilesT)
import Data.Maybe (fromJust,isNothing, isJust, fromMaybe)
import Data.Text (Text)


mergeImport :: M.Map FilePath (CST.Module ()) -> IO() --M.Map FilePath [M.Map Text Text]
mergeImport cstFileMap = do
    let mapTypeDecl = M.map (\a -> CST.modImports a ) cstFileMap

    let laebelTyp = M.map (\a -> map (\b-> M.fromList $ fromMaybe [] (travImportDecl b) ) a ) mapTypeDecl
    putStrLn (show laebelTyp)

checkModuleName :: N.ModuleName -> Text
checkModuleName (N.ModuleName p) = p

travImportDecl :: CST.ImportDecl () -> Maybe [(Text,Text)]
travImportDecl decl
  | isNothing (CST.impNames decl) && isNothing (CST.impQual decl) = Nothing
  | isJust (CST.impNames decl) = Just (map (\a-> (a, checkModuleName $ CST.nameValue $ CST.impModule decl))(travImpName $ snd (fromJust (CST.impNames decl))))
  | otherwise = Just ([(checkModuleName $ CST.nameValue $ snd $ fromJust (CST.impQual decl),checkModuleName $ CST.nameValue $ CST.impModule decl)])

travExportDecl :: CST.Declaration () -> N.ModuleName -> Maybe [(Text,Text)]
travExportDecl (CST.DeclType _ n _ _) (N.ModuleName p)=Just [(N.runProperName $ CST.nameValue $ CST.dataHdName n,p)]
travExportDecl (CST.DeclNewtype _ n _ _ _) (N.ModuleName p) =Just [(N.runProperName $ CST.nameValue $ CST.dataHdName n,p)]
travExportDecl _ _= Nothing


travImpName :: CST.DelimitedNonEmpty (CST.Import ()) -> [Text]
travImpName p = travImport (CST.sepHead (CST.wrpValue p)): map (\(_,a)-> travImport a) (CST.sepTail $ CST.wrpValue p)

travImport :: CST.Import () -> Text
travImport (CST.ImportValue _ p) = CST.getIdent $ CST.nameValue p
travImport (CST.ImportOp _ p) = N.runOpName $ CST.nameValue p
travImport (CST.ImportType _ p _) = N.runProperName $ CST.nameValue p
travImport (CST.ImportTypeOp _ _ p) = N.runOpName $ CST.nameValue p
travImport (CST.ImportClass _ _ p) = N.runProperName $ CST.nameValue p