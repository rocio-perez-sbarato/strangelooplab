{- |
Module      : HypersetParadox.Examples
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module HypersetParadox.Examples where 

import HypersetParadox.BuildSelfRefSentence
    ( SelfRefSentence(SelfRefSentence),
        selfRefSentenceToSystem,
        selfRefSentenceLabeling )
import HypersetParadox.BuildSentence
    ( Sentence(Sentence), labelingCombined, sentenceToSystem )
import HypersetParadox.BuildReferenceChain
    ( sentenceYablo, yabloFamilySystem, yabloFamilyLabeling )
import Hyperset.Types ( System, Labeling ) 
import HypersetParadox.BuildFunctionApplication
    ( Inclosure(Inclosure), closureToSystem ) 

-- * Aplicación de predicados

closureIncSchemeSystem :: System String 
closureIncSchemeSystem = closureToSystem (Inclosure "omega" "x" "delta")

-- * Paradojas sin autorreferencia

-- ** Paradoja del mentiroso dual

{-  El mentiroso dual. 
    q = "p", Q = "Es verdadera", 0 = False
    p = "q", P = "Es verdadera", 1 = True
-}

liar1 :: Sentence
liar1 = Sentence "p" "q" "E1" "0"

liar2 :: Sentence
liar2 = Sentence "q" "p" "E2" "1"

-- | Labeling de dual Liar
dualLiarLabeling :: Labeling String 
dualLiarLabeling = labelingCombined liar1 liar2 

-- | Sistema de ecuaciones de una paradoja compuesta por dos sentencias
dualLiarSystem :: System String
dualLiarSystem = sentenceToSystem liar1 ++ sentenceToSystem liar2

-- ** Paradoja de Yablo de 3 niveles 

sentYablo3 :: Sentence 
sentYablo3 = sentenceYablo 1 3

sysYablo3 :: System String
sysYablo3 = yabloFamilySystem 3

labelingYablo3 :: Labeling String
labelingYablo3 = yabloFamilyLabeling 3

-- * Paradojas autorreferenciales

{-  El mentiroso. 
    q = "Esta oración", E = "Es verdadera", 0 = False
-}
liar :: SelfRefSentence
liar = SelfRefSentence "q" "E" "0"

liarSystem :: System String 
liarSystem = selfRefSentenceToSystem liar

liarLabeling :: Labeling String 
liarLabeling = selfRefSentenceLabeling liar

{- La paradoja de Russell
    R = "El conjunto de Russell", E = "Pertenece a sí mismo", 0 = False
-}
russell :: SelfRefSentence
russell = SelfRefSentence "R" "E" "0"

{- Ejemplo: El barbero
    B = "El barbero", E = "Se afeita a sí mismo", 0 = False
-}
barber :: SelfRefSentence
barber = SelfRefSentence "B" "E" "0"


