module SourceCodeReader (extractEntryData) where

import Data.List
import System.IO
import System.Directory

import DirectoryController as DC (FileName)

type DataName = String
type NewtypeName = String
type ClassName = String
type DtNtName = String

-- extracts data, newtype and class names + instances (new data-oriented format)
extractEntryData :: DC.FileName -> FilePath -> 
  IO ([DataName],[NewtypeName],[ClassName],[(DtNtName,ClassName)])
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  handle <- openFile fileName ReadMode
  scriptFile <- hGetContents handle
  forceRead scriptFile `seq` hClose handle
  let rScriptFileLines = lines scriptFile
  -- removes comment lines
      scriptFileLines = rScriptFileLines \\ filter (isInfixOf "--") rScriptFileLines
      dataTypes = filter (isPrefixOf "data ") scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
      definInstances = filter (isPrefixOf "instance ") scriptFileLines

      rAllClasslines = filter (isPrefixOf "class ") scriptFileLines
      allClasslines = zipWith gL (getIndexes 0 rAllClasslines rScriptFileLines) rAllClasslines

      gL num line
        | "=>" `isInfixOf` line && not (isSuffixOf "=>" line || isSuffixOf "=> " line) = line
        | not ("=>" `isInfixOf` line) && not ("(" `isInfixOf` line) && "class" `isPrefixOf` line = line
        | isSuffixOf "=>" line || isSuffixOf "=> " line = "=> " ++ dropWhile (==' ') (rScriptFileLines !! (num + 1))
        | otherwise = gL (num + 1) (rScriptFileLines !! (num + 1))

  let dataNames = map (takeWhile (/=' ') . (\\ "data ")) dataTypes
      newtypeNames = map (takeWhile (/=' ') . (\\ "newtype ")) newtypeTypes
      ordClassNames = map getClassName allClasslines
      stripInstances = map getStripInstance definInstances

  return (dataNames,newtypeNames,ordClassNames,stripInstances)

-- enforces strict file reading; files can be closed to avoid memory exhaustion
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

-- index number, class + script lines, indexes list (for multi-line classes)
getIndexes :: Int -> [String] -> [String] -> [Int]
getIndexes _ _ [] = []
getIndexes idx clsLines (x:xs) = if isClassLine then addIdx else nextIdx where
  isClassLine = x `elem` clsLines
  addIdx = idx : nextIdx
  nextIdx = getIndexes (idx + 1) clsLines xs

-- used to extract the class name from a raw script line
getClassName :: String -> ClassName
getClassName rsl = if derived then stripDv else stripDf where
  derived = "=>" `isInfixOf` rsl
  -- operates on derived classes
  stripDv = takeWhile (/=' ') . (\\ "> ") $ dropWhile (/='>') rsl
  -- operates on defined classes
  stripDf = takeWhile (/=' ') $ (\\ "class ") rsl

-- used to extract data/newtype name + class instance name
getStripInstance :: String -> (DtNtName,ClassName)
getStripInstance rsl = if derived then stripDv else stripDf where
  derived = "=>" `isInfixOf` rsl
  -- operates on derived class instances
  stripDv
    | "(" `isInfixOf` rsl = (stripDvLmdn,stripDvLmc)
    | otherwise = (stripDvLs !! 1,head stripDvLs)
  stripDvLs = words . (\\ "> ") $ dropWhile (/='>') rsl
  stripDvLm = (\\ "> ") $ dropWhile (/='>') rsl
  stripDvLmdn = takeWhile (/=')') . (\\ "(") $ dropWhile (/='(') stripDvLm
  stripDvLmc = takeWhile (/=' ') stripDvLm
  -- operates on defined class instances
  stripDf = (stripDfL !! 1,head stripDfL)
  stripDfL = words $ (\\ "instance ") rsl
