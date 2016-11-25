module Main where

import Lib

import qualified Data.HashSet as HS

main :: IO ()
main = do
  let a = dfaFromTuples [
            (0,'a',1)
          , (1,'b',2)
          , (0,'b',3)
          , (3,'a',4)
          , (4,'a',4)
          , (4,'b',4)
          ]
  let n = nfaFromTuples [
            (0,'a',1)
          , (0,'a',2)
          , (0,'a',3)
          , (2,'b',3)
          , (3,'a',4)
          , (2,'a',4)
          ]
  let dfa = (0,a,HS.singleton 4)
  let nfa = (0,n,HS.singleton 4)
  print $ checkWordDFA dfa "abba"
  print $ checkWordDFA dfa "ba"
  print $ checkWordDFA dfa "aba"
  print $ checkWordDFA dfa "bba"
  print $ checkWordDFA dfa "baa"

  print $ checkWordNFA nfa "baa"
  print $ checkWordNFA nfa "baa"
  print $ checkWordNFA nfa "baa"
