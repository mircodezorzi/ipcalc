import System.Environment

import Text.Printf

import Data.List.Split
import Data.List

import Data.Bits
import Data.Word

-- | Return true if string represents real IP
isIp :: String -> Bool
isIp ip = all (==True) (map (\n -> check (read n)) (splitOn "." ip)) where
  check n = case n of
    n | 0 <= n && n <= 255 -> True
    _                      -> False

-- | Returns true if mask is valid
isMask :: Word32 -> Bool
isMask mask = (countLeadingZeros (complement mask)) + (countTrailingZeros mask) == 32

-- | Convert IP to string representation
showIp :: Word32 -> String
showIp ip = printf "%d.%d.%d.%d"
  (shiftR ((.&.) ip 0xff000000) 24)
  (shiftR ((.&.) ip 0x00ff0000) 16)
  (shiftR ((.&.) ip 0x0000ff00) 8)
  (shiftR ((.&.) ip 0x000000ff) 0)

-- | Convert string to IP
readIp :: String -> Word32
readIp ip =
  (shiftL (is !! 0) 24) + (shiftL (is !! 1) 16) + (shiftL (is !! 2) 8) + (is !! 3) where
    is = map readIp' (splitOn "." ip) where
      readIp' ip' = (read ip') :: Word32

-- | Convert IP to class
getClass :: Word32 -> Char
getClass ip = case ((.&.) (shiftR ip 24) 0xc0) of
  0x00 -> 'A'
  0x80 -> 'B'
  0xc0 -> 'C'
  _    -> ' '

-- | Convert class to corresponding mask
getSlash :: Char -> Word32
getSlash n = case n of
  'A' -> 8
  'B' -> 16
  'C' -> 24
  _   -> 0

-- | Convert mask to slash notation
slash :: Word32 -> Int
slash n = 32 - (countTrailingZeros n)

-- | Convert slash notation to mask
fromSlash :: Int -> Word32
fromSlash n = (.&.) 0xffffffff (complement ((rotateR 2 (32 - n + 1)) - 1))

data Subnet = Subnet { start :: Word32
                     , end   :: Word32
                     , hosts :: Int
                     , mask  :: Word32
                     } deriving Show

-- | Given a starting IP and number of hosts, find the appropriate subnet
getSubnet :: Word32 -> Int -> Subnet
getSubnet start hosts = do
  let s = ceiling (logBase 2 (fromIntegral hosts + 2))
  let m = bit (fromIntegral s)
  Subnet
    start
    (start + m - 1)
    hosts
    (fromSlash s)

-- | Print subnet information
showSubnet :: Subnet -> IO ()
showSubnet (Subnet start end hosts mask) = do
  printf "%s /%d (%d hosts)\nstart: %s\nend: %s\nmask: %s\n\n"
    (showIp start)
    (slash mask)
    hosts
    (showIp start)
    (showIp end)
    (showIp mask)

-- | Given a starting ip and a list of number of hosts find the appropriate
-- | subnets. Function could have been a simple Word -> [Int] -> [Subnet] and
-- | the check could have been done before calling it, but I decided to to it
-- | this way to get some practive in.
solve :: Word32 -> [Int] -> Maybe [Subnet]
solve ip (h : hs)
  | h > 0 =
    case solve (end subnet + 1) hs of
              Just solution -> Just ([subnet] ++ solution)
              Nothing       -> Nothing
  | otherwise = Nothing
  where subnet = getSubnet ip h
solve ip [] = Just []

{-| Alternative solution:

validate :: [Int] -> Bool
validate hosts = (all (> 0) hosts)

solve :: Word32 -> [Int] -> [Subnet]
solve ip (h : hs) = [subnet] ++ (solve (end subnet + 1) hs)
  where subnet = getSubnet ip h
solve ip [] = []

-}

-- | Reverse list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main :: IO ()
main = do
  args <- getArgs
  let ip = args !! 0
  let ts = map (\x -> read x) (tail args)

  case isIp ip of
    True -> do
      case solve (readIp ip) (reverseList (sort ts)) of
        Just solution -> (mapM_ showSubnet solution)
        Nothing       -> putStrLn "invalid hosts"
    False -> putStrLn "invalid ip"
