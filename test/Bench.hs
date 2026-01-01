{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
import Test.Tasty.Bench ( defaultMain, Benchmark, bgroup, bench, nf, env )
import GHC.Generics
import Control.DeepSeq

import Program.Operations
import Program.Program (solutions, solutions', solutions'')

main :: IO ()
main = defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks =  env @([Int], Int) (pure ([1, 2, 3], 4)) (\ ~(ns, n) ->
    bgroup "Solutions" [
        bench "1st" $ nf (solutions ns) n,
        bench "2nd" $ nf (solutions' ns) n, 
        bench "3rd" $ nf (solutions'' ns) n
    ])


deriving instance Generic Expr
deriving instance NFData Expr
deriving instance Generic Op
deriving instance NFData Op