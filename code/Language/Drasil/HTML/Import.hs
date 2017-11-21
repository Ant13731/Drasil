module Language.Drasil.HTML.Import where
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..),
                             Bound(..),DerivType(..), Set, Quantifier(..))
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec
import qualified Language.Drasil.HTML.AST as H
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.NamedIdea (term, short, getA)
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Quantity (Quantity(..), eqSymb)
import Language.Drasil.ChunkDB (HasSymbolTable(..), getUnitLup, symbLookup)
import Language.Drasil.Expr.Extract
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Symbol
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.SymbolAlphabet (lD)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Unit (usymb)
import Language.Drasil.Citations (Citation(..),CiteField(..))

import Control.Lens hiding ((:>),(:<),set)

-- | expr translation function from Drasil to HTML 'AST'
expr :: HasSymbolTable s => Expr -> s -> H.Expr
expr (V v)            _ = H.Var   v
expr (Dbl d)          _ = H.Dbl   d
expr (Int i)          _ = H.Int   i
expr (a :* b)         sm = H.Mul   (expr a sm) (expr b sm)
expr (a :+ b)         sm = H.Add   (expr a sm) (expr b sm)
expr (a :/ b)         sm = H.Frac  (replace_divs a sm) (replace_divs b sm)
expr (a :^ b)         sm = H.Pow   (expr a sm) (expr b sm)
expr (a :- b)         sm = H.Sub   (expr a sm) (expr b sm)
expr (a :. b)         sm = H.Dot   (expr a sm) (expr b sm)
expr (Neg a)          sm = H.Neg   (expr a sm)
expr (Deriv Part a 1) sm = H.Mul (H.Sym (Special Partial)) (expr a sm)
expr (Deriv Total a 1)sm = H.Mul (H.Sym lD) (expr a sm)
expr (Deriv Part a b) sm = H.Frac (H.Mul (H.Sym (Special Partial)) (expr a sm)) 
                          (H.Mul (H.Sym (Special Partial)) (expr b sm))
expr (Deriv Total a b)sm = H.Frac (H.Mul (H.Sym lD) (expr a sm)) 
                          (H.Mul (H.Sym lD) (expr b sm))
expr (C c)            sm = -- FIXME: Add Stage for Context
  H.Sym $ (eqSymb (symbLookup c (sm ^. symbolTable)))
expr (FCall f x)      sm = H.Call (expr f sm) (map (flip expr sm) x)
expr (Case ps)        sm = if length ps < 2 then 
                    error "Attempting to use multi-case expr incorrectly"
                    else H.Case (zip (map (flip expr sm . fst) ps) (map (flip rel sm . snd) ps))
expr e@(_ := _)       sm = rel e sm
expr e@(_ :!= _)      sm = rel e sm
expr e@(_ :> _)       sm = rel e sm
expr e@(_ :< _)       sm = rel e sm 
expr e@(_ :<= _)      sm = rel e sm 
expr e@(_ :>= _)      sm = rel e sm 
expr (Matrix a)       sm = H.Mtx $ map (map (flip expr sm)) a
expr (Index a i)      sm = H.Index (expr a sm) (expr i sm)
expr (UnaryOp u)      sm = (\(x,y) -> H.Op x [y]) (ufunc u sm)
expr (Grouping e)     sm = H.Grouping (expr e sm)
expr (BinaryOp b)     sm = (\(x,y) -> H.Op x y) (bfunc b sm)
expr (Not a)          sm = H.Not   (expr a sm)
expr (a  :&&  b)      sm = H.And   (expr a sm) (expr b sm)
expr (a  :||  b)      sm = H.Or    (expr a sm) (expr b sm)
expr (a  :=>  b)      sm = H.Impl  (expr a sm) (expr b sm)
expr (a  :<=> b)      sm = H.Iff   (expr a sm) (expr b sm)
expr (IsIn  a b)      sm = H.IsIn  (expr a sm) (set b)
expr (State a b)      sm = H.State (map (flip quan sm) a) (expr b sm)
expr (Len _)           _ = error "Len not yet implemented"
expr (Append _ _)      _ = error "Append not yet implemented"

-- | Healper for translating Quantifier
quan :: HasSymbolTable s => Quantifier -> s -> H.Quantifier
quan (Forall e) sm = H.Forall (expr e sm)
quan (Exists e) sm = H.Exists (expr e sm)

-- | Helper function for translating 'UFunc's
ufunc :: HasSymbolTable s => UFunc -> s -> (H.Function, H.Expr)
ufunc (Log e) sm = (H.Log, expr e sm)
ufunc (Summation (Just (s, Low v, High h)) e) sm = 
  (H.Summation (Just ((s, expr v sm), expr h sm)), (expr e sm))
ufunc (Summation Nothing e) sm = (H.Summation Nothing,(expr e sm))
ufunc (Summation _ _) _ = error "HTML/Import.hs Incorrect use of Summation"
ufunc (Product (Just (s, Low v, High h)) e) sm = 
  (H.Product (Just ((s, expr v sm), expr h sm)), expr e sm)
ufunc (Product Nothing e) sm = (H.Product Nothing, (expr e sm))
ufunc (Product _ _) _ = error "HTML/Import.hs Incorrect use of Product"
ufunc (Abs e) sm = (H.Abs, expr e sm)
ufunc (Norm e) sm = (H.Norm, expr e sm)
ufunc i@(Integral _ _ _) sm = integral i sm
ufunc (Sin e)    sm = (H.Sin,  expr e sm)
ufunc (Cos e)    sm = (H.Cos,  expr e sm)
ufunc (Tan e)    sm = (H.Tan,  expr e sm)
ufunc (Sec e)    sm = (H.Sec,  expr e sm)
ufunc (Csc e)    sm = (H.Csc,  expr e sm)
ufunc (Cot e)    sm = (H.Cot,  expr e sm)
ufunc (Exp e)    sm = (H.Exp,  expr e sm)
ufunc (Sqrt e)   sm = (H.Sqrt, expr e sm)

-- | Helper function for translating 'BiFunc's
bfunc :: HasSymbolTable s => BiFunc -> s -> (H.Function, [H.Expr])
bfunc (Cross e1 e2) sm = (H.Cross, map (flip expr sm) [e1,e2])

-- | Helper function for translating 'Relation's
rel :: HasSymbolTable s => Relation -> s -> H.Expr
rel (a := b)  sm = H.Eq (expr a sm) (expr b sm)
rel (a :!= b) sm = H.NEq (expr a sm) (expr b sm)
rel (a :< b)  sm = H.Lt (expr a sm) (expr b sm)
rel (a :> b)  sm = H.Gt (expr a sm) (expr b sm)
rel (a :<= b) sm = H.LEq (expr a sm) (expr b sm)
rel (a :>= b) sm = H.GEq (expr a sm) (expr b sm)
rel _ _ = error "Attempting to use non-Relation Expr in relation context."

-- | Helper for translating Sets
set :: Set -> H.Set
set Integer  = H.Integer
set Rational = H.Rational
set Real     = H.Real
set Natural  = H.Natural
set Boolean  = H.Boolean
set Char     = H.Char
set String   = H.String
set Radians  = H.Radians
set (Vect a) = H.Vect (set a)
set (Obj a)  = H.Obj a
set (DiscreteI a) = H.DiscreteI a
set (DiscreteD a) = H.DiscreteD a
set (DiscreteS a) = H.DiscreteS a

-- | Helper function for translating Integrals (from 'UFunc')
integral :: HasSymbolTable s => UFunc -> s -> (H.Function, H.Expr)
integral (Integral (Just (Low v), Just (High h)) e wrtc) sm = 
  (H.Integral (Just (expr v sm), Just (expr h sm)) (int_wrt wrtc sm), expr e sm)
integral (Integral (Just (High h), Just (Low v)) e wrtc) sm = 
  (H.Integral (Just (expr v sm), Just (expr h sm)) (int_wrt wrtc sm), expr e sm)
integral (Integral (Just (Low v), Nothing) e wrtc) sm = 
  (H.Integral (Just (expr v sm), Nothing) (int_wrt wrtc sm), expr e sm)
integral (Integral (Nothing, Just (Low v)) e wrtc) sm = 
  (H.Integral (Just (expr v sm), Nothing) (int_wrt wrtc sm), expr e sm)
integral (Integral (Just (High h), Nothing) e wrtc) sm = 
  (H.Integral (Nothing, Just (expr h sm)) (int_wrt wrtc sm), expr e sm)
integral (Integral (Nothing, Just (High h)) e wrtc) sm = 
  (H.Integral (Nothing, Just (expr h sm)) (int_wrt wrtc sm), expr e sm)
integral (Integral (Nothing, Nothing) e wrtc) sm = 
  (H.Integral (Nothing, Nothing) (int_wrt wrtc sm), expr e sm)
integral _ _ = error "TeX/Import.hs Incorrect use of Integral"

-- | Helper function for translating the differential
int_wrt :: (HasSymbolTable s) => Expr -> s -> H.Expr
int_wrt wrtc sm = expr (Deriv Total wrtc 1) sm

-- | Helper function for translating operations in expressions 
replace_divs :: HasSymbolTable s => Expr -> s -> H.Expr
replace_divs (a :/ b) sm = H.Div (replace_divs a sm) (replace_divs b sm)
replace_divs (a :+ b) sm = H.Add (replace_divs a sm) (replace_divs b sm)
replace_divs (a :* b) sm = H.Mul (replace_divs a sm) (replace_divs b sm)
replace_divs (a :^ b) sm = H.Pow (replace_divs a sm) (replace_divs b sm)
replace_divs (a :- b) sm = H.Sub (replace_divs a sm) (replace_divs b sm)
replace_divs a        sm = expr a sm

-- | Translates Sentence to the HTML representation of Sentence ('Spec')
spec :: HasSymbolTable s => Sentence -> s -> H.Spec
spec (S s)      _ = H.S s
spec (Sy s)     _ = H.Sy s
spec (EmptyS :+: b) sm = spec b sm
spec (a :+: EmptyS) sm = spec a sm
spec (a :+: b) sm = spec a sm H.:+: spec b sm
spec (G g)      _ = H.G g
spec (Sp s)     _ = H.Sp s
spec (P s)      _ = H.N s
spec (F f s)    sm = spec (accent f s) sm
spec (Ref t r) sm = H.Ref t (spec r sm)
spec (Quote q) sm = H.S "&quot;" H.:+: spec q sm H.:+: H.S "&quot;"
spec EmptyS     _ = H.EmptyS
spec (E e)     sm = H.E $ expr e sm

-- | Helper function for translating accented characters to 
-- an HTML renderable form.
accent :: Accent -> Char -> Sentence
accent Grave  s = S $ '&' : s : "grave;" --Only works on vowels.
accent Acute  s = S $ '&' : s : "acute;" --Only works on vowels.

-- | Helper function for translating decorated characters to
-- an HTML renderable form.
decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = s :+: S "&#770;" 
decorate Vector s = S "<b>" :+: s :+: S "</b>"
decorate Prime  s = s :+: S "&prime;"

-- | Translates from Document to the HTML representation of Document
makeDocument :: HasSymbolTable s => Document -> s -> H.Document
makeDocument (Document title author sections) sm = 
  H.Document (spec title sm) (spec author sm) (createLayout sections sm)

-- | Translates from LayoutObj to the HTML representation of LayoutObj
layout :: HasSymbolTable s => Int -> SecCons -> s -> H.LayoutObj
layout currDepth (Sub s) sm = sec (currDepth+1) s sm
layout _         (Con c) sm = lay c sm

-- | Helper function for creating sections as layout objects
createLayout :: HasSymbolTable s => [Section] -> s -> [H.LayoutObj]
createLayout secs sm = map (flip (sec 0) sm) secs

-- | Helper function for creating sections at the appropriate depth
sec :: HasSymbolTable s => Int -> Section -> s -> H.LayoutObj
sec depth x@(Section title contents) sm = 
  H.HDiv [(concat $ replicate depth "sub") ++ "section"] 
  ((H.Header (depth+2) (spec title sm)):(map (flip (layout depth) sm) contents)) 
  (spec (refName x) sm)

-- | Translates from Contents to the HTML Representation of LayoutObj.
-- Called internally by layout.
lay :: HasSymbolTable s => Contents -> s -> H.LayoutObj
lay x@(Table hdr lls t b) sm = H.Table ["table"] 
  ((map (flip spec sm) hdr) : (map (map (flip spec sm)) lls)) (spec (refName x) sm) b (spec t sm)
lay (Paragraph c)       sm = H.Paragraph (spec c sm)
lay (EqnBlock c)        sm = H.HDiv ["equation"] [H.Tagless (H.E (expr c sm))] (H.EmptyS)
--lay (CodeBlock c)        = H.CodeBlock c
lay x@(Definition c)    sm = H.Definition c (makePairs c sm) (spec (refName x) sm)
lay (Enumeration cs)    sm = H.List $ makeL cs sm
lay x@(Figure c f wp)   sm = H.Figure (spec (refName x) sm) (spec c sm) f wp
lay x@(Module m)        sm = H.Module (formatName m) (spec (refName x) sm)
lay (Graph _ _ _ _)      _ = H.Paragraph (H.EmptyS)  -- need to implement!
lay x@(Requirement r)   sm = 
  H.Requirement (spec (phrase $ r ^. term) sm) (spec (refName x) sm) (spec (short r) sm)
lay x@(Assumption a)    sm = 
  H.Assumption (spec (phrase $ a ^. term) sm) (spec (refName x) sm) (spec (short a) sm)
lay x@(LikelyChange lc) sm = 
  H.LikelyChange (spec (phrase $ lc ^. term) sm) (spec (refName x) sm) (spec (short lc) sm)
lay x@(UnlikelyChange uc) sm = 
  H.UnlikelyChange (spec (phrase $ uc ^. term) sm) (spec (refName x) sm) (spec (short uc) sm)
lay (Defnt dtyp pairs rn) sm = H.Definition dtyp (layPairs pairs) (spec rn sm)
  where layPairs = map (\(x,y) -> (x, (map (\z -> lay z sm) y)))
lay (GDef)               _ = H.Paragraph (H.EmptyS)  -- need to implement!
lay (IMod)               _ = H.Paragraph (H.EmptyS)  -- need to implement!
lay (TMod ps rf r)      sm = H.Definition (Theory r) 
  (map (\(x,y) -> (x, map (flip lay sm) y)) ps) (spec rf sm)
lay (DDef ps rf d)      sm = H.Definition (Data d)
  (map (\(x,y) -> (x, map (flip lay sm) y)) ps) (spec rf sm)
lay (Bib bib)           sm = H.Bib $ map (flip layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable s => Citation -> s -> H.Citation
layCite (Book      fields) sm = H.Book      $ map (flip layField sm) fields
layCite (Article   fields) sm = H.Article   $ map (flip layField sm) fields
layCite (MThesis   fields) sm = H.MThesis   $ H.Thesis H.M   : map (flip layField sm) fields
layCite (PhDThesis fields) sm = H.PhDThesis $ H.Thesis H.PhD : map (flip layField sm) fields
layCite (Misc      fields) sm = H.Misc      $ map (flip layField sm) fields
layCite (Online    fields) sm = H.Online    $ map (flip layField sm) fields

layField :: HasSymbolTable s => CiteField -> s -> H.CiteField
layField (Author     p)   _ = H.Author     p
layField (Title      s)  sm = H.Title      $ spec s sm
layField (Series     s)  sm = H.Series     $ spec s sm
layField (Collection s)  sm = H.Collection $ spec s sm
layField (Volume     n)   _ = H.Volume     n
layField (Edition    n)   _ = H.Edition    n
layField (Place (c, s))  sm = H.Place      (spec c sm, spec s sm)
layField (Publisher  s)  sm = H.Publisher  $ spec s sm
layField (Journal    s)  sm = H.Journal    $ spec s sm
layField (Year       n)   _ = H.Year       n
layField (Date    n m y)  _ = H.Date       n m y
layField (URLdate n m y)  _ = H.URLdate    n m y
layField (Page       n)   _ = H.Page       n
layField (Pages     ns)   _ = H.Pages      ns
layField (Note       s)  sm = H.Note       $ spec s sm
layField (Issue      n)   _ = H.Issue      n
layField (School     s)  sm = H.School     $ spec s sm
layField (URL        s)  sm = H.URL        $ spec s sm
layField (HowPub     s)  sm = H.HowPub     $ spec s sm
layField (Editor     p)   _ = H.Editor     p

-- | Translates lists
makeL :: HasSymbolTable s => ListType -> s -> H.ListType
makeL (Bullet bs)      sm = H.Unordered   $ map (flip item sm) bs
makeL (Number ns)      sm = H.Ordered     $ map (flip item sm) ns
makeL (Simple ps)      sm = H.Simple      $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Desc ps)        sm = H.Desc        $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Definitions ps) sm = H.Definitions $ map (\(x,y) -> (spec x sm, item y sm)) ps

-- | Helper for translating list items
item :: HasSymbolTable s => ItemType -> s -> H.ItemType
item (Flat i)     sm = H.Flat (spec i sm)
item (Nested t s) sm = H.Nested (spec t sm) (makeL s sm)

-- | Translates definitions
-- (Data defs, General defs, Theoretical models, etc.)
makePairs :: HasSymbolTable s => DType -> s -> [(String,[H.LayoutObj])]
makePairs (Data c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "DD") $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [H.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (buildEqn c m)] (H.EmptyS)]),
  ("Description", [H.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "T") $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [H.HDiv ["equation"] [H.Tagless (H.E (rel (c ^. relat) m))]
                  (H.EmptyS)]),
  ("Description", [H.Paragraph (spec (c ^. defn) m)])
  ]
makePairs General  _ = error "Not yet implemented"
makePairs Instance _ = error "Not yet implemented"
makePairs TM _       = error "Not yet implemented"
makePairs DD _       = error "Not yet implemented"

missingAcro :: Sentence -> Maybe Sentence -> Sentence
missingAcro dflt Nothing = S "<b>":+: dflt :+: S "</b>"
missingAcro _ (Just a) = S "<b>":+: a :+: S "</b>"

-- | Translates the defining equation from a QDefinition to 
-- HTML's version of Sentence
buildEqn :: HasSymbolTable s => QDefinition -> s -> H.Spec  
buildEqn c sm = H.N (eqSymb c) H.:+: H.S " = " H.:+: 
  H.E (expr (equat c) sm)

-- | Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable s => QDefinition -> s -> H.Spec
buildDDDescription c m = descLines 
  (if verboseDDDescription then (vars (getQ c := equat c) m) else []) m
  where getQ (EC a _) = C a

-- | Helper for building each line of the description of a data def
descLines :: (HasSymbolTable s, Quantity q) => [q] -> s -> H.Spec  
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (H.N (eqSymb vc) H.:+: 
  (H.S " is the " H.:+: (spec (phrase $ vc ^. term) m) H.:+:
   unWrp (getUnitLup vc m)))
  where unWrp (Just a) = H.S " (" H.:+: H.Sy (a ^. usymb) H.:+: H.S ")"
        unWrp Nothing  = H.S ""
descLines (vc:vcs) m = descLines (vc:[]) m H.:+: H.HARDNL H.:+: descLines vcs m
