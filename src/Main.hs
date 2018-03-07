module Main where
import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as Bs
import           Data.Word            (Word32, Word8)

import           Control.Monad        (replicateM)
import           Control.Monad.Loops  (iterateUntilM)
import           Data.Binary.Bits.Get (BitGet, getWord8, runBitGet)
import           Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
import qualified Data.Binary.Put      as P
import           Data.Bits            (setBit, shiftR, testBit, (.&.))
import           Data.Int             (Int64)
import           Data.List            (elemIndex, elemIndices, group)
import           Data.List.Split      (chunksOf)
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

type Nybble = Word8
type Scanline = [Word8] --4 bytes per scanline
type Size = Word32
data CompressCommand = Same
 | OneColor {col ::Nybble}
 | MirrorBack {offset :: Int}
 | Raw {rawScan :: Scanline}
 | SetByMask {col :: Nybble, setMask :: Word8}
 | Set3ByMask {leftCol :: Nybble, midCol :: Nybble, rightCol:: Nybble,
                setMask :: Word8}
 | Shl {col :: Nybble}
 | Shr {col :: Nybble}
 | OneBorder {leftCol :: Nybble, rightCol :: Nybble, rightCount::Word8}
 | TwoBorder {leftCol :: Nybble, midCol :: Nybble, rightCol:: Nybble,
                midCount :: Word8, rightCount :: Word8}
 | SetPair {pos :: Word8, pair :: Word8}
 | SetPairs {pos1 :: Word8, pair1 :: Word8, pos2 :: Word8, pair2 :: Word8}
  deriving (Show)

instance Eq CompressCommand where --for groupping at serialization phase
  Same == Same = True
  Raw _ == Raw _ = True
  _ == _ = False

main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "stngDelta v0.2\nDelta coding compression tool \
      \for 'Star Trek - The Next Generation' game. Mode 6 enabled."
    parse ["-d", inFileName, offs, outFileName] =
      decompress inFileName (read offs) outFileName
    parse ["-c", inFileName, outFileName] = compress inFileName outFileName
    parse _ = putStrLn "Usage:\n\
      \  stngDelta -d <inFile> <offset> <outFile>  Decompress block from given ROM file.\n\
      \  stngDelta -c <inFile> <outFile> Compress given plain block.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."


--------------------------DECOMPRESS--------------------------------------------
decompress :: String -> Int64 -> String ->  IO ()
decompress inFileName offs outFileName = do
  input <- Bs.readFile inFileName
  let
    binaryTail = Bs.drop offs input
    getBlock = G.runGetOrFail commandsParser binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, position, commands) -> do
      let output = decompressCommands commands
      Bs.writeFile  outFileName $ Bs.pack output
      putStrLn $ printf "Compressed block size was 0x%X" position

commandsParser :: G.Get [CompressCommand] --deserialize decompression commands
commandsParser = do
  dstSize <- G.getWord32be --size in bytes, but parser operates on scanlines (x4)
  let
    nextCommand commands = (commands ++) <$> runBitGet parseCommand --append com
    isDstSizeReached xs =  length xs * 4 >= fromIntegral dstSize --dst size in bytes
  --start with empty buffer until target size reached
  iterateUntilM isDstSizeReached nextCommand []

parseCommand:: BitGet [CompressCommand]
parseCommand = do --parse one command
  let getNybble = getWord8 4
  command <- getNybble
  param <- getNybble
  let paramInt = fromIntegral param :: Int
  case command of
    0x0 -> return $ replicate (paramInt + 1) Same
    0x1 -> return  [OneColor param]
    0x2 -> return  [MirrorBack (paramInt + 1)]
    0x3 -> do --extended MirrorBack
      offsLo <- getWord8 8
      let offs = paramInt * 0x100 + fromIntegral offsLo + 1
      return [MirrorBack offs]
    0x4 -> do --read Raw scanline
      scanlines <- replicateM (paramInt + 1) getScanline
      return  $ map Raw scanlines
      where getScanline = replicateM 4 $ getWord8 8
    0x5 -> do
      mask <- getWord8 8
      return [SetByMask param mask]
    0x6 -> do --Set3ByMask
      mCol <- getNybble
      lCol <- getNybble
      mask <- getWord8 8
      return [Set3ByMask  param mCol lCol mask]
    0x7 -> return [Shl param]
    0x8 -> return [Shr param]
    0x9 -> do --OneBorder
      lCol <- getNybble
      rCount <- getNybble
      return [OneBorder lCol param rCount]
    0xA -> do --TwoBorder
      mCol <- getNybble
      lCol <- getNybble
      rCount <- getNybble
      mCount <- getNybble
      return [TwoBorder  lCol mCol param mCount rCount]
    0xB -> do --extension
      p <- getWord8 8
      if param < 4
        then return [SetPair param p]
        else do
          p2 <- getWord8 8
          case param of
            4 -> return [SetPairs 0 p 1 p2]
            5 -> return [SetPairs 0 p 2 p2]
            6 -> return [SetPairs 0 p 3 p2]
            7 -> return [SetPairs 1 p 2 p2]
            8 -> return [SetPairs 1 p 3 p2]
            9 -> return [SetPairs 2 p 3 p2]
            _ -> return [Same] --BA-BF - copy. Just save previous scanline
    _ -> return [Raw []] --CX-FF - next. Read next command.

decompressCommands :: [CompressCommand] -> [Word8] --commands to plain tiles
decompressCommands xs = concat . tail $ foldl go [[0,0,0,0]] xs
  where --do not count initializing empty scanline
    go buffer com = buffer ++ [decompressed] --append decomrpessed chunk to buf
      where
        curScan = last buffer --affects performance, but short
        curColors = toNybbles curScan
        decompressed = case com of
          Same -> curScan
          OneColor color -> fromNybbles (replicate 8 color)
          MirrorBack off -> reverse indexScanline
            where indexScanline = reverse buffer !! off
          Raw scan -> scan
          SetByMask color mask -> fromNybbles changedColors
            where
              changedColors = zipWith (curry replaceByFlag) curColors [7, 6.. 0]
              replaceByFlag (nyb, bitNum) = if testBit mask bitNum then color else nyb
          Set3ByMask rCol mCol lCol mask -> fromNybbles $ reverse revChangedColors
            where
              revChangedColors = go' (zip (reverse curColors) [0..7])  [rCol, mCol, lCol]
              go' [] _ = [] --all bits are tested
              go' ((curCol, _): tuples) [] = curCol : go' tuples [] --3 colors are used
              go' ((curCol, bitNum):tuples) news@(newCol: newCols) =
                if mask `testBit` bitNum
                  then newCol : go' tuples newCols --replace color
                  else curCol : go' tuples news --don't replace, bit is not set
          Shl color -> fromNybbles changedColors
            where changedColors = tail curColors ++ [color]
          Shr color ->  fromNybbles changedColors
            where changedColors = color : init curColors
          OneBorder lCol rCol rCount -> fromNybbles (reverse revBuiltColors)
            where revBuiltColors = take 8 $ replicate (fromIntegral rCount) rCol
                                    ++ repeat lCol
          TwoBorder lCol mCol rCol mCount rCount -> fromNybbles (reverse revBuiltColors)
            where
              revBuiltColors = take 8 $ replicate (fromIntegral rCount) rCol
                              ++ replicate (fromIntegral mCount) mCol
                              ++ repeat lCol
          SetPair n newPair -> replace n newPair curScan
          SetPairs n1 newPair1 n2 newPair2 -> replace n2 newPair2 firstChange
            where firstChange = replace n1 newPair1 curScan

toNybbles :: [Word8] -> [Nybble] --break list of bytes to list of nybbles
toNybbles  = concatMap (\b -> [b `shiftR` 4, b .&. 0xF])

fromNybbles :: [Nybble] -> [Word8] --merge list of nybbles to list of bytes
fromNybbles xs = map go $ chunksOf 2 xs
  where
    go :: [Nybble] -> Word8
    go [h]    = h * 0x10
    go [h, l] = h * 0x10 + l
    go _      = error "Empty or excess fromNybbles"

replace :: Word8 -> Word8 -> Scanline -> Scanline --replace color at position
replace position newVal list = take posInt list ++ newVal : drop (posInt + 1) list
  where posInt = fromIntegral position

--------------------------COMPRESS----------------------------------------------
compress :: String -> String -> IO ()
compress inFileName outFileName = do
  input <- Bs.readFile inFileName
  let
    commands = compressPlain $ Bs.unpack input
    sizeBs = P.runPut . P.putWord32be $ fromIntegral (Bs.length input)
    output = serializeCommands commands
  Bs.writeFile outFileName $ sizeBs `Bs.append` output

compressPlain :: [Word8] -> [CompressCommand] --get compression commands
compressPlain plain = snd $ foldl go ([[0,0,0,0]], []) $ chunksOf 4 plain
  where --start with initting black scanline and empty commands buffer
  --carry reversed buffer (for MirrorBack) and accumulated commands in params
    go (revBuffer, commands) cur = (cur:revBuffer, commands ++ [command])
      where --emit command analysing previous and current scanlines
        command
          | prev == cur = Same
          | tail prevColors == init curColors = Shl (last curColors)
          | init prevColors == tail curColors = Shr (head curColors)
          | isUniform cur = OneColor (head curColors)
          | Just foundOffset <- elemIndex (reverse cur) $ take 0x1000 revBuffer
            --0x1000 bytes search window, as 24bit on offset at max
              = MirrorBack foundOffset
          | isUniform diffNewColors, --mask is indexed from MSB to LSB (7 -)
              let mask = foldl setBit 0 $ map (fromIntegral . (7 -)) diffPlaces
              = SetByMask (head diffNewColors) mask
          | [diffPairPlace] <- diffPairPlaces = SetPair diffPairPlace (head diffNewPairs)
          | [lCols, rCols] <- group curColors =
              OneBorder (head lCols) (head rCols) (lengthAsWord rCols)
          | [lCols, mCols, rCols] <- group curColors =
              TwoBorder (head lCols) (head mCols) (head rCols)
                (lengthAsWord mCols) (lengthAsWord rCols)
          | [place1, place2] <- diffPairPlaces =
              SetPairs place1 (head diffNewPairs) place2 (diffNewPairs !! 1)
          | [lCol, mCol, rCol] <- diffNewColors, --mask is indexed from MSB to LSB (7 -)
              let mask = foldl setBit 0 $ map (fromIntegral . (7 -)) diffPlaces
              = Set3ByMask lCol mCol rCol mask
          | otherwise = Raw cur
          where
            prev = head revBuffer
            prevColors = toNybbles prev
            curColors = toNybbles cur
            (diffPlaces, diffNewColors) = diff prevColors curColors
            (diffPairPlaces, diffNewPairs) = diff prev cur

lengthAsWord :: [a] -> Word8
lengthAsWord xs =  fromIntegral (length xs)

--return differ positions and new values between two lists
diff :: [Word8] -> [Word8] -> ([Word8], [Word8])
diff prev cur = (map fromIntegral diffPlaces, diffNewColors)
  where
    diffNewColors = map (cur !!) diffPlaces
    diffPlaces = elemIndices True $ zipWith (/=) prev cur

isUniform :: [Nybble] -> Bool
isUniform [] = error "Empty list in isUniform"
isUniform xs = all (== head xs) xs

serializeCommands :: [CompressCommand] -> Bs.ByteString --write commands to binary
serializeCommands commands = P.runPut . runBitPut $ mapM_ putCommand (group commands)
  where --group for accumulating RLE (Same) and Raws
    putNybble :: Nybble -> BitPut()
    putNybble = putWord8 4
    putByte:: Word8 -> BitPut()
    putByte = putWord8 8
    putCommand :: [CompressCommand] -> BitPut ()
    putCommand grp@(Same : rest) = if length grp > 0x10 --4 bits on RLE count
        then putNybble 0 >> putNybble 0xF >> putCommand (drop 0x10 grp)
        else putNybble 0 >> putNybble (lengthAsWord rest) --length - 1
    putCommand [OneColor color] = putNybble 1 >> putNybble color
    putCommand [MirrorBack offs]
      | encodeOffs <= 0xF  = putNybble 2 >> putNybble (fromIntegral encodeOffs)
      | encodeOffs <= 0xFFF,
        let
          hi = fromIntegral $ encodeOffs `shiftR` 8
          lo = fromIntegral $ encodeOffs .&. 0xFF
        = putNybble 3 >> putNybble hi >> putByte lo
      | otherwise = error "MirrorBack more than 0x1000" --should not happen
      where encodeOffs = offs - 1
    putCommand grp@(Raw _ : _) = if length grp > 0x10
        then putRaws (take 0x10 grp) >> putCommand (drop 0x10 grp)
        else putRaws grp
          where --put list of Raws
            putRaws :: [CompressCommand] -> BitPut()
            putRaws rs = putNybble 4 >> putNybble (lengthAsWord (tail rs))
                          >> mapM_ putScan rs
              where --extract scan from each Raw and put it to binary
                putScan (Raw scan) = mapM_ putByte scan
                putScan _          = error "Not Raw in putRaw"
    putCommand [SetByMask color mask] = putNybble 5 >> putNybble color >> putByte mask
    putCommand [Set3ByMask lCol mCol rCol mask] = putNybble 6 >>
      putNybble rCol >> putNybble mCol >> putNybble lCol >> putByte mask
    putCommand [Shl color] = putNybble 7 >> putNybble color
    putCommand [Shr color] = putNybble 8 >> putNybble color
    putCommand [OneBorder lCol rCol cnt] = putNybble 9 >> putNybble rCol
      >> putNybble lCol >> putNybble cnt
    putCommand [TwoBorder lCol mCol rCol mCnt rCnt] = putNybble 0xA
      >> putNybble rCol >> putNybble mCol >> putNybble lCol
      >> putNybble rCnt >> putNybble mCnt
    putCommand [SetPair p pr] = putNybble 0xB >> putNybble p >> putByte pr
    putCommand [SetPairs 0 pr1 1 pr2] = putByte 0xB4 >> putByte pr1 >> putByte pr2
    putCommand [SetPairs 0 pr1 2 pr2] = putByte 0xB5 >> putByte pr1 >> putByte pr2
    putCommand [SetPairs 0 pr1 3 pr2] = putByte 0xB6 >> putByte pr1 >> putByte pr2
    putCommand [SetPairs 1 pr1 2 pr2] = putByte 0xB7 >> putByte pr1 >> putByte pr2
    putCommand [SetPairs 1 pr1 3 pr2] = putByte 0xB8 >> putByte pr1 >> putByte pr2
    putCommand [SetPairs 2 pr1 3 pr2] = putByte 0xB9 >> putByte pr1 >> putByte pr2
    --all other variants should not happen:
    putCommand x = error $ "Can't pattern match in putCommand: "++ show x
