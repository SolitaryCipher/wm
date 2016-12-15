module Local.Color ( ColorSet(..)
                   , RGB(..)
                   , hexToRGB
                   , colorize 
                   , interpolate
                   , getB16Colors
                   , rgbTuple
                   ) where

import Numeric
import System.Environment (getEnv)
import Text.Printf (printf)

type Color = ColorSet -> RGB
data RGB = RGB [Integer]

data ColorSet = ColorSet { red       :: RGB
                         , orange    :: RGB
                         , yellow    :: RGB
                         , green     :: RGB
                         , cyan      :: RGB
                         , blue      :: RGB
                         , magenta   :: RGB
                         , brown     :: RGB
                         , black     :: RGB
                         , lightGrey :: RGB
                         , darkGrey  :: RGB
                         , white     :: RGB
                         }

instance Show RGB where
  show (RGB a) = (:) '#' $ concatMap (printf "%s" . showHex') a
    where showHex' c = pad $ showHex c ""
          pad [x] = '0':[x]
          pad x = x


colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> String
colorize fg bg = printf "<span%s%s>%s</span>" (attr "fg" fg) (attr "bg" bg)
  where attr name value
          | null value = ""
          | otherwise  = printf " %scolor=\"%s\"" name value

interpolate :: Double -> Color -> Color -> Color 
interpolate p a b = interp p <$> a <*> b
  where interp p' (RGB a') (RGB b') = RGB $ map (interpolate' p') $ zip a' b'

interpolate' :: Double -> (Integer, Integer) -> Integer
interpolate' p (a,b) = round $ p * fromIntegral b + (1-p) * fromIntegral a

hexToRGB :: String -> RGB
hexToRGB s = RGB $ map readHex' [take 2 s,take 2 $ drop 2 s,drop 4 s]
  where readHex' s' = fst $ head $ readHex s'

rgbTuple :: RGB -> (Double,Double,Double)
rgbTuple (RGB (a:b:c:_)) = (f a,f b,f c) --((a !! 0),(a !! 1), (a !! 2))
  where f i = fromIntegral i / 255
rgbTuple _ = (0,0,0)


getB16Colors :: IO ColorSet
getB16Colors = do
    black'     <- getEnv "base01" -- bar bg ...
    darkGrey'  <- getEnv "base02"
    --lightGrey' <- getEnv "base03"
    white'     <- getEnv "base06"

    red'       <- getEnv "base08"
    orange'    <- getEnv "base09"
    yellow'    <- getEnv "base0A"
    green'     <- getEnv "base0B"
    blue'      <- getEnv "base0D"
    cyan'      <- getEnv "base0C"
    magenta'   <- getEnv "base0E"
    brown'     <- getEnv "base0F"
    return ColorSet { blue      = hexToRGB blue'
                    , darkGrey  = hexToRGB darkGrey'
                    , lightGrey = hexToRGB "5F5960" --lightGrey
                    , green     = hexToRGB green'
                    , black     = hexToRGB black'
                    , red       = hexToRGB red'
                    , magenta   = hexToRGB magenta'
                    , yellow    = hexToRGB yellow'
                    , orange    = hexToRGB orange'
                    , cyan      = hexToRGB cyan'
                    , brown     = hexToRGB brown'
                    , white     = hexToRGB white'
                    }

