import Data.Bits (xor, shiftR)
import Data.Word (Word64)

newtype Seed = Seed Word64 deriving Show

makeSeed :: Word64 -> Seed
makeSeed = Seed

next :: Seed -> (Word64, Seed)
next(Seed s) =
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      z3 = z2 `xor` (z2 `shiftR` 31)
  in (z3, Seed s')

randoms :: Seed -> [Word64]
randoms seed =
  let (x, seed') = next seed
  in x : randoms seed'

main :: IO ()
main = print (take 10 (randoms (makeSeed 123)))