import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Tree as Tree

find :: (Eq a) => [a] -> a -> Bool
find [] _ = False
find (x:xs) y = if x == y then True else find xs y

--read "2" + 1 reads and casts "2" to 2 (Integer) and adds it to 1
--read "5" :: Int type annotation, read "5" as an Int
--show 2 casts it to "2" and prints it
--all@(x:xs) names a list 'all' and declares its pattern 'x:xs' 

--guards:
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise                 = "You're a whale, congratulations!"  


--guards + where
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
  | bmi <= skinny = "You're underweight, you emo, you!"  
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
  | otherwise     = "You're a whale, congratulations!"  
  where bmi = weight / height ^ 2  
        skinny = 18.5  
        normal = 25.0  
        fat = 30.0  
--where bindings aren't shared across function bodies of different patterns. If you want several patterns of one function to access some shared name, you have to define it globally.

--se puede usar pattern matching en el where
--where bmi = weight / height ^ 2  
--      (skinny, normal, fat) = (18.5, 25.0, 30.0)

--se puede usar where para definir una funcion, como si fuera auxiliar, se pueden anidar
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
        where bmi weight height = weight / height ^ 2
              --where wheight ...

--let no propaga los bindings a traves de guardas, al contrario q where, sintaxis diferente
--(let (a,b,c) = (1,2,3) in a+b+c) * 100  

--The in part can also be omitted when defining functions and constants directly in GHCi. If we do that, then the names will be visible throughout the entire interactive session.

--infix function  
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
  | a > b     = GT  
  | a == b    = EQ  
  | otherwise = LT

--ghci> 3 `myCompare` 2

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
  let smallerSorted = quicksort [a | a <- xs, a <= x]  
      biggerSorted = quicksort [a | a <- xs, a > x]  
  in  smallerSorted ++ [x] ++ biggerSorted  

--composition
--map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
--[-5,-3,-6,-7,-3,-2,-19,-24]  

-- #################
--heap queue implementation
--(tree, node, tree)
--makechild :: (Maybe (Maybe a, b, Maybe c) , b ,Maybe (Maybe a, b, Maybe c)) -> (a,b,a) -> ((a,b,a),b,a)
--makechild (ltree, node, rtree) (ltree2, node2, rtree2) = ((ltree2, node2, rtree2), node, rtree)

  
data MyTree = MyTree { lTree :: Maybe MyTree
                     , node :: Int
                     , rTree :: Maybe MyTree
                     } deriving (Show, Eq, Ord) --instancia derivada de los Typeclasses, compara campo a campo si dichos campos son a su vez miembros de las typeclasses se podra usar ==, /=, <, > ... entre valores de este tipo

-- let root = MyTree {lTree = Nothing, node = 2, rTree = Nothing}
-- > root
--MyTree {lTree = Nothing, node = 2, rTree = Nothing}
-- > lTree root
--Nothing

changeLTree :: MyTree -> Maybe MyTree -> MyTree
--changeLTree tree@MyTree {lTree = l, node = n, rTree = r} Nothing = tree
changeLTree tree@MyTree {lTree = l, node = n, rTree = r} x = MyTree {lTree = x, node = n, rTree = r}

--changeLTree root (Just MyTree {lTree = Nothing, node = 4, rTree = Nothing})
--MyTree {lTree = Just (MyTree {lTree = Nothing, node = 4, rTree = Nothing}), node = 2, rTree = Nothing}


--reserved word 'type' is just an alias for an already existing type
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook  


--error handling, could also be coded with Maybe
data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of   
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken   
                          then Right code  --Right for success
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!" --Left for failure

lockers :: LockerMap  
lockers = Map.fromList   
  [(100,(Taken,"ZD39I"))  
  ,(101,(Free,"JAH3I"))  
  ,(103,(Free,"IQSA9"))  
  ,(105,(Free,"QOTSA"))  
  ,(109,(Taken,"893JJ"))  
  ,(110,(Taken,"99292"))  
  ]  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

--arbol de busqueda                           
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
    
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)
  | x == a = Node x left right  
  | x < a  = Node a (treeInsert x left) right  
  | x > a  = Node a left (treeInsert x right)  

--if Person deriving Read
-- >read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  
-- Person {firstName = "Michael", lastName = "Diamond", age = 43} 

oddSquareSum :: Integer  
oddSquareSum =   
  let oddSquares = filter odd $ map (^2) [1..]  
      belowLimit = takeWhile (<10000) oddSquares  
  in  sum belowLimit
  


-- :info in ghci returns the type signature of a function
-- :info map returns the same as :t map adding the module which it is defined in
--map :: (a -> b) -> [a] -> [b] 	-- Defined in `GHC.Base'

--if requested on a Type
{- :info Int
data Int = GHC.Types.I# GHC.Prim.Int# 	-- Defined in `GHC.Types'
instance Bounded Int -- Defined in `GHC.Enum'
instance Enum Int -- Defined in `GHC.Enum'
instance Eq Int -- Defined in `GHC.Classes'
instance Integral Int -- Defined in `GHC.Real'
instance Num Int -- Defined in `GHC.Num'
instance Ord Int -- Defined in `GHC.Classes'
instance Read Int -- Defined in `GHC.Read'
instance Real Int -- Defined in `GHC.Real'
instance Show Int -- Defined in `GHC.Show'
-}

-- if requested on a type constructor returns the definition
{-:info Maybe
data Maybe a = Nothing | Just a 	-- Defined in `Data.Maybe'
instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'
instance Monad Maybe -- Defined in `Data.Maybe'
instance Functor Maybe -- Defined in `Data.Maybe'
instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
instance Show a => Show (Maybe a) -- Defined in `GHC.Show'
-}

--for type constructors (as they are functions over types) and types  we can ask the kind (type of the type)
--'*' means 'concrete type' 
{-:kind Int
Int :: *
*Main> :kind Maybe
Maybe :: * -> *
-}

-- Stream monitor naive
monitor :: String -> [Char] -> (Char -> [Char] -> Bool) -> Bool
monitor [] _ _ = True
monitor (i:input) acc f = (f i acc) && (monitor input (i:acc) f) --first call

func :: Char -> [Char] -> Bool
func a acc = a == 'a' &&  goodAcc || a == 'b' && goodAcc
     where  goodAcc = acc == [] || acc == ['a'] || acc == ['a','a']


data Know = Know {t :: Int
                 ,process :: Int
                 ,seq:: Int
                 ,matrix :: [[Bool]]
                 }

--main for the program
main = do
  putStrLn "hello, world!"
  putStrLn "Hello, what's your name?"  
  name <- getLine  --reads a line from stdIn and stores its value in the variable name; getLine :: IO -> String; the ,- construct is used to extract the String from the IO action 'getLine' note that this can only be performed inside an IO action or IO block (main) thus separating the pure part from the impure.
  putStrLn ("Hey " ++ name ++ ", you rock!")
  
--for compiling use
-- $ ghc --make haskell-test.hs
-- $ ./haskell-test.hs

{-We can read the type of putStrLn like this: putStrLn takes a string and returns an I/O action that has a result type of ()
(i.e. the empty tuple, also know as unit). An I/O action is something that, when performed,
will carry out an action with a side-effect (that's usually either reading from the input or printing stuff to the screen)
and will also contain some kind of return value inside it. Printing a string to the terminal doesn't really have any kind of meaningful return value,
so a dummy value of () is used.
-}



--functor Functor f where
    --fmap :: (a->b) -> f a -> f b
--what this means is that it takes a function from a to b
--and a type constructor f
--and it will apply the function over the elements of that type
-- it is used to apply a function over a data structure in a generic way: the function takes the type of the node in the data structure a and then returns b and the functor keeps applying it over the data structure.
--functor is not implemented, just its type is defined.
-- it can be seen as a generalization of map (lists) but for all data structures

--example
class Functor f where
  fmap :: (a->b) -> f a -> f b

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x lTree rTree) = Node (f x) (fmap f lTree) (fmap f rTree)
