If you are looking for **build instructions**, find them in the README file [here](https://github.com/JacquesCarette/literate-scientific-software).

--------------------------------------------------
### Drasil Coding Style Guide
--------------------------------------------------

1. Use spaces instead of tabs. Preferably 2 spaces per indent level. Ex:

```Haskell
top level
  indent level 1
    indent level 2
```
2. Keep line lengths to a maximum of 80 characters.

3. Use camel case for naming. Ex: `someFunctionFoo`

4. Make names meaningful. Ex: `htTransCladFuel` as opposed to `hG`

5. One blank line between top-level definitions. No blank lines between type signatures and function definitions.

6. Surround binary operators with a single space on either side. Ex:

`x ^. term`

7. Do not break operations across lines, unless they are sentence combinators (`:+:`, `+:+`, `(sC)`, etc.). Ex: 

```Haskell
S "The" +:+ (phrase $ heat_trans ^. term) +:+ 
S "is the"
```
...

-------------------------------------------------
### Quick Start Legend
-------------------------------------------------

| Combinator | Usage / Meaning | Type Signature |
|------------|-----------------|----------------|
| `+:+` | Concatenates two sentences and adds a space in between them | Sentence -> Sentence -> Sentence |
| `:+:` | Concatenates two sentences | Sentence -> Sentence -> Sentence |
| `at_start` | Returns the singular form of a NounPhrase, for use at the start of a sentence (ie. Capitalized) | NounPhrase n => n -> Sentence |
| `at_start'` | Returns the plural form of a NounPhrase, for use at the start of a sentence (ie. Capitalized) | NounPhrase n => n -> Sentence |
| `introduceAbb` | Introduces an abbreviation using the titleized version of the term followed by the abbreviation in parentheses | NamedIdea n => n -> Sentence |
| `of_` | Creates a compound nounphrase from two terms, with " of " between them | (NamedIdea a, NamedIdea b) => a -> b -> NP |
| `of'` | Similar to `of_` except the second term is always plural and the arguments must be NounPhrases, not NamedIdeas | (NounPhrase a, NounPhrase b) => a -> b -> NP |
| `phrase` | Returns the singular form of a NounPhrase | NounPhrase n => n -> Sentence |
| `plural` | Returns the plural form of a NounPhrase | NounPhrase n => n -> Sentence |
| `sParen` | Wraps a Sentence in parentheses | Sentence -> Sentence |
| `titleize` | Returns the singular, title form of a NounPhrase | NounPhrase n => n -> Sentence |
| `titleize'` | Returns the plural, title form of a NounPhrase | NounPhrase n => n -> Sentence |

**Note: See Language.Drasil.Spec.hs for more information on the Sentence type**

More to come as questions arise.

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Data**
  - Contains the current common-knowledge base for Drasil
  
**Example**
  - Contains the currently implemented examples
  
**Language**
  - Contains the base for the Drasil language
  
**stable**
  - Contains the (currently) expected output for each of the examples
  
Makefile
  - The makefile for building Drasil and the examples

README.md
  - This file
  
drasil.cabal
  - Cabal file, used by stack to build Drasil
  
log_check.sh
  - A shell script for comparing the generated output to the expected output for each example. Outputs whether the examples match their stable versions or not
  
stack.yaml
  - Used by Stack
