{-#LANGUAGE ScopedTypeVariables#-}

module Lib
    ( someFunc
    , dfaFromTuples
    , runWordDFA
    , checkWordDFA
    , nfaFromTuples
    , runWordNFA
    , checkWordNFA
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Control.Monad
import Data.Foldable (foldlM)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Letter = Char
type Alphabet = HS.HashSet Letter
type AState = Int
type AStates = HS.HashSet AState
type AFinal = HS.HashSet AState
type AWord = [Letter]

type NFATrans = HM.HashMap AState (HM.HashMap Letter (HS.HashSet AState))
type NFAStart = HS.HashSet AState
type NFA = (NFAStart, NFATrans, AFinal)

type DFATrans = HM.HashMap AState (HM.HashMap Letter AState)
type DFAStart = AState
type DFA = (DFAStart, DFATrans, AFinal)

nfaFromTuples :: [(AState,Letter,AState)] -> NFATrans
nfaFromTuples a = foldl fromT HM.empty a
  where
    fromT :: NFATrans -> (AState,Letter,AState) -> NFATrans
    fromT acc (s,l,s') = HM.insertWith (HM.unionWith HS.union) s hm acc
      where
        hm = HM.singleton l $ HS.singleton s'

stepNFA' :: NFA -> AState -> Letter -> Maybe (HS.HashSet AState)
stepNFA' (_,t,_) s l = join $ HM.lookup l <$> (HM.lookup s t)

runWordNFA :: NFA -> AWord -> Maybe AState
runWordNFA nfa@(s,_,_) w = foldlM (stepNFA nfa) s w

checkWordNFA :: NFA -> AWord -> Bool
checkWordNFA nfa@(s,_,f) w = 
  case foldlM (stepNFA nfa) s w of
    Nothing -> False
    Just s -> HS.member s f

dfaFromTuples :: [(AState,Letter,AState)] -> DFATrans
dfaFromTuples a = foldl fromT HM.empty a
  where
    fromT :: DFATrans -> (AState,Letter,AState) -> DFATrans
    fromT acc (s,l,s') = HM.insertWith (HM.union) s (HM.singleton l s') acc

stepDFA :: DFA -> AState -> Letter -> Maybe AState
stepDFA (_,t,_) s l = join $ HM.lookup l <$> (HM.lookup s t)  

runWordDFA :: DFA -> AWord -> Maybe AState
runWordDFA dfa@(s,_,_) w = foldlM (stepDFA dfa) s w 

checkWordDFA :: DFA -> AWord -> Bool
checkWordDFA dfa@(s,_,f) w = 
  case foldlM (stepDFA dfa) s w of
    Nothing -> False
    Just s -> HS.member s f