module Image(Color,Pixel,Image,ppmEncode,saveImage,newImage,draw,rect,line,bigLine,circle,ellipse,comb,difference) where

import Data.List
import Data.List.Extra(trim)
import Data.List.Split(chunksOf)
import Control.Applicative
import Control.Exception
import Data.Array
import Data.Either.Combinators(rightToMaybe)


type Pixel = (Int,Int,Int)
type Color = (Int,Int,Int)

data Image = Image {size::(Int,Int) , imageData::Array Int Pixel}

instance Show Image where
    show img = "Image: size = " ++ (show $ size $ img)


ppmEncode :: Image -> String
ppmEncode img = header ++ imgData
    where 
        header            =   intercalate " " ["P3",(show $ fst $ size img),(show $ snd $ size img),"255\n"]
        imgData           =   unlines . map showPixel $ elems $ imageData img
        showPixel (a,b,c) =   (intercalate " ") . (map show) $ [a,b,c]


------------------Read Image---------------------

ppmInterprete :: String -> Image
ppmInterprete str = createImage (read (list!!1)::Int) (read (list!!2)::Int) (read (list!!3)::Int) (drop 4 $list)
    where
        list            = words $ unwords $ filter (/="") $ filterComments $ lines str
        filterComments  = map (trim . fst . break (=='#'))

createImage :: Int -> Int -> Int -> [String] -> Image
createImage wid hei _ pix = Image (wid,hei) (listArray (0,wid*hei-1) $ map toTuble3 $ chunksOf 3 pix)
    where
        toTuble3 [a,b,c] = (read a::Int,read b::Int,read c::Int)
        
readImage::String -> IO(Image)
readImage path = ppmInterprete <$> (readFile path)
    
------------------Read Image End--------------------

saveImage::String -> Image -> IO()
saveImage = flip $ (flip writeFile) . ppmEncode

newImage :: (Int,Int) -> Color -> Image
newImage (width, height) bg = Image (width, height) $ listArray (0,height*width-1) (replicate (height*width) bg)

--for Testing
img100 = newImage (100,100) (255,255,255)
preSave = saveImage "out.ppm"


---------------------------------Drawing------------------------------

draw:: [(Int,Int)] -> Color -> Image -> Image
draw px col (Image (width,heigth) imgData) = Image (width,heigth) $ imgData//(zip (map (conv2Dto1D width) px) $ repeat col)

conv2Dto1D :: Int -> (Int,Int) -> Int
conv2Dto1D width (x,y) = y*width+x

-----------------------------FORMS---------------------------

---------------------Rect---------------------------

rect :: (Int,Int) ->  Int -> Int -> [(Int,Int)]
rect (x,y) width height = [ (i+x,j+y) | i <- take width [0..], j <- take height [0..]]


---------------------LINE---------------------------
line :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
line (x1,y1) (x2,y2) 
    | (abs $ slope (x1,y1) (x2,y2)) <= 1 = map (\x->(x1+(ceiling x), (ceiling $ x*(slope (x1,y1) (x2,y2)))+y1)) $ take (x2-x1+1) [0..]
    | otherwise = map (\y->((ceiling $ y*(slope (y1,x1) (y2,x2)))+x1,y1+(ceiling y))) $ take (y2-y1+1) [0..]

bigLine :: (Int,Int) -> (Int,Int) -> Int -> [(Int,Int)]
bigLine (x1,y1) (x2,y2) width = foldl1 (++) (map func [0..width])
    where func 
            | (abs $ slope (x1,y1) (x2,y2)) <= 1 = \w->line (x1,y1+w) (x2,y2+w)
            | otherwise = \w->line (x1+w,y1) (x2+w,y2)

slope :: (Int,Int) -> (Int,Int) -> Double
slope (x1,y1) (x2,y2) = (fromIntegral (y2 - y1)) / (fromIntegral (x2-x1))

----------------------Cicle------------------------
circle :: (Int,Int) -> Int -> [(Int,Int)]
circle (x,y) rad = filter (isInCircle (x,y) rad) $ rect (x,y) (2*rad) (2*rad)

isInCircle:: (Int,Int) -> Int -> (Int,Int) -> Bool
isInCircle (posX,posY) rad (x,y) = sqrt((fromIntegral x - cX)^2 + (fromIntegral y - cY)^2) <= fromIntegral rad
    where 
        cX = (fromIntegral posX + fromIntegral rad) -0.5 
        cY = (fromIntegral posY + fromIntegral rad) -0.5


----------------------Ellipse------------------------
ellipse :: (Int,Int) -> Int -> Int -> [(Int,Int)]
ellipse (x,y) xRad yRad = filter (isInEllipse (x,y) xRad yRad) $ rect (x,y) (2*xRad) (2*yRad)

isInEllipse:: (Int,Int) -> Int -> Int -> (Int,Int) -> Bool
isInEllipse (posX,posY) xRad yRad (x,y) = (fromIntegral x - cX)^2/((fromIntegral xRad/2)^2) + (fromIntegral y - cY)^2/((fromIntegral yRad/2)^2) <= 1.0
    where 
        cX = (fromIntegral posX + (fromIntegral xRad/2)) -0.5 
        cY = (fromIntegral posY + fromIntegral yRad/2) -0.5


-----------------------------FORMCOMBINATORS---------------------------
comb :: Eq a => [a] -> [a] -> [a]
comb = blackbird nub (++)

difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (not . flip elem b) a


---------------------------------Birds/Helpers------------------------------
blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird f g x y = f (g x y)

if' :: Bool -> a -> a -> a
if' cond a1 a2 = if cond then a1 else a2