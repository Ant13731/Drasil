module Language.Drasil.TeX.Helpers where

import Text.PrettyPrint (text)
import qualified Text.PrettyPrint as TP
import Control.Applicative (pure)

import Language.Drasil.Config (numberedSections, hyperSettings)
import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.TeX.Monad

-----------------------------------------------------------------------------
-- Infrastructre for defining commands, environments, etc.
--   (calls to TP should only occur in this section)

br, sq, parens :: D -> D
br x = lb <> x <> rb
  where
  lb = pure $ text "{"
  rb = pure $ text "}"
sq x = ls <> x <> rs
  where
  ls = pure $ text "["
  rs = pure $ text "]"
parens x = lp <> x <> rp
  where
  lp = pure $ text "("
  rp = pure $ text ")"

-- Make 1-argument command
command :: String -> (String -> D)
command s c = pure $ (H.bslash TP.<> text s) TP.<> H.br c

commandD :: String -> (D -> D)
commandD s c = (pure $ (H.bslash TP.<> text s)) <> br c

-- 1-argument command, with optional argument
command1o :: String -> Maybe String -> String -> D
command1o s o c = pure $ (H.bslash TP.<> text s) TP.<>
  (maybe TP.empty H.sq o) TP.<> H.br c

-- no braces!
command1oD :: String -> Maybe D -> D -> D
command1oD s o c = 
  (pure $ (H.bslash TP.<> text s)) <> (maybe empty sq o) <> c

-- 0-argument command
command0 :: String -> D
command0 s = pure $ H.bslash TP.<> text s

-- 2-argument command
command2 :: String -> String -> String -> D
command2 s a0 a1 = pure $ (H.bslash TP.<> text s) TP.<> H.br a0 TP.<> H.br a1

-- Encapsulate environments
mkEnv :: String -> D -> D
mkEnv nm d =
  (pure $ text ("\\begin" ++ H.brace nm)) $+$ 
  d $+$
  (pure $ text ("\\end" ++ H.brace nm))

-- for defining (LaTeX) macros
comm :: String -> String -> Maybe String -> D
comm b1 b2 s1 = command0 "newcommand" <> (pure $ H.br ("\\" ++ b1) TP.<> 
  maybe TP.empty H.sq s1 TP.<> H.br b2)

-- this one is special enough, let this sub-optimal implementation stand
renewcomm :: String -> String -> D
renewcomm b1 b2 = pure $ text "\\renewcommand" TP.<> H.br ("\\" ++ b1) TP.<> H.br b2


-- Useful to have empty 
empty :: D
empty = pure TP.empty

-- For sections
genSec :: Int -> D
genSec d
  | d < 0 = error "Cannot have section with negative depth"
  | d > 3 = error "Section depth must be from 0-2"
  | d == 3 = pure $ H.bslash TP.<> text "paragraph"
  | otherwise = pure $ 
     H.bslash TP.<> text (concat $ replicate d "sub") TP.<> text "section" 
      TP.<> (if (not numberedSections) then text "*" else TP.empty) 

-- For references
ref, sref, hyperref, mref, rref, aref, lcref, ucref :: String -> D -> D
ref t x = (pure $ text (t ++ "~\\ref")) <> br x
hyperref t x = command0 "hyperref" <> sq x <> br ((pure $ text (t ++ "~")) <> x)
sref = if numberedSections then ref else hyperref
mref _ x = (pure $ text "M\\ref") <> br x
rref _ x = (pure $ text "R\\ref") <> br x
aref _ x = (pure $ text "A\\ref") <> br x
lcref _ x = (pure $ text "LC\\ref") <> br x
ucref _ x = (pure $ text "UC\\ref") <> br x
-----------------------------------------------------------------------------
-- Now create standard LaTeX stuff

usepackage, count, includegraphics :: String -> D
usepackage      = command "usepackage"
-- changed to command "newcounter" from command "count" (I assume this was
-- what was intended?)
count           = command "newcounter"
includegraphics = command "includegraphics"

author, caption, item, label, title :: D -> D
author          = commandD "author"
caption         = commandD "caption"
item            = commandD "item"
label           = commandD "label"
title           = commandD "title"

item' :: D -> D -> D
item' bull s = command1oD "item" (Just bull) s

maketitle, maketoc, newline, newpage, centering :: D
maketitle = command0 "maketitle"
maketoc = command0 "tableofcontents"
newline = command0 "newline"
newpage = command0 "newpage"
centering = command0 "centering"

code, itemize, enumerate, description, figure, center, document, equation :: D -> D
code        = mkEnv "lstlisting"
itemize     = mkEnv "itemize"
enumerate   = mkEnv "enumerate"
description = mkEnv "description"
figure      = mkEnv "figure"
center      = mkEnv "center"
document    = mkEnv "document"
equation    = mkEnv "dmath" --displays math and wraps long lines

docclass, exdoc :: Maybe String -> String -> D
docclass = command1o "documentclass"
exdoc = command1o "externaldocument"

sec :: Int -> D -> D
sec d b1 = genSec d <> br b1

subscript, superscript :: D -> D -> D
subscript a b = a <> (pure $ H.unders) <> br b
superscript a b = a <> (pure $ H.hat) <> br b

-- Macro / Command def'n --
--TeX--
srsComms, lpmComms, bullet, counter, ddefnum, ddref, colAw, colBw, arrayS
 , modcounter, modnum, reqcounter, reqnum, assumpcounter, assumpnum
 , lccounter, lcnum, uccounter, ucnum :: D
srsComms = bullet %% counter %% ddefnum %% ddref %% colAw %% colBw %% arrayS
lpmComms = pure $ text ""

counter = count "datadefnum"
modcounter = count "modnum"
reqcounter = count "reqnum"
assumpcounter = count "assumpnum"
lccounter = count "lcnum"
uccounter = count "ucnum"

bullet  = comm "blt"             "- "                Nothing
ddefnum = comm "ddthedatadefnum" "MG\\thedatadefnum" Nothing
ddref   = comm "ddref"           "MG\\ref{#1}"       (Just "1")
colAw   = comm "colAwidth"       "0.2\\textwidth"    Nothing
colBw   = comm "colBwidth"       "0.73\\textwidth"   Nothing

modnum    = comm "mthemodnum"        "M\\themodnum"        Nothing
reqnum    = comm "rthereqnum"        "R\\thereqnum"        Nothing
assumpnum = comm "atheassumpnum"     "A\\theassumpnum"     Nothing
lcnum     = comm "lcthelcnum"        "LC\\thelcnum"        Nothing
ucnum     = comm "uctheucnum"        "UC\\theucnum"        Nothing

arrayS  = renewcomm "arraystretch" "1.2"

fraction :: D -> D -> D
fraction n d = command0 "frac" <> br n <> br d

hyperConfig :: D
hyperConfig = command "hypersetup" hyperSettings

useTikz :: D
useTikz = usepackage "luatex85" $+$ (pure $ text "\\def") <>
  command "pgfsysdriver" "pgfsys-pdftex.def" $+$
  -- the above is a workaround..  temporary until TeX packages have been fixed
  usepackage "tikz" $+$ command "usetikzlibrary" "arrows.meta" $+$
  command "usetikzlibrary" "graphs" $+$ command "usetikzlibrary" "graphdrawing" $+$
  command "usegdlibrary" "layered"
  
-----------------------------------------------------------------------------
-- This 'belongs' in Monad, but it would make Monad depend on Helpers, which depends
-- on Monad...

-- toEqn is special; it switches to Math, but inserts an equation environment
toEqn :: D -> D
toEqn (PL g) = equation $ PL (\_ -> g Math)

-----------------------------------------------------------------------------
-- Helper(s) for String-Printing in TeX where it varies from HTML/Plaintext

paren, sqbrac :: String -> String
paren x = "\\left(" ++ x ++ "\\right)"
sqbrac x = "\\left[" ++ x ++ "\\right]"
