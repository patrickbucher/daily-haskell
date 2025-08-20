import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

parity :: [Bit] -> Bit
parity xs =
  if (ones `mod` 2) == 1
    then 1
    else 0
  where
    ones = length (filter (== 1) xs)

addParity :: [Bit] -> [Bit]
addParity xs = parity xs : xs

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chopN :: Int -> [Bit] -> [[Bit]]
chopN _ [] = []
chopN 0 _ = []
chopN n bits = take n bits : chopN n (drop n bits)

validParity :: [Bit] -> Bool
validParity (x:xs) = x == parity xs

checkParity :: [Bit] -> [Bit]
checkParity bits =
  if validParity bits
    then tail bits
    else error "parity error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chopN 9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
