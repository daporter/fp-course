-- line comment

{-
  block comment
-}

x :: Integer
x = 99

f :: Integer -> Integer
f a = a + 10

g :: Integer -> Integer -> Integer
g a b = (a + b) * 2

-- these two are equivalent.

h :: Integer -> Integer -> Integer
h = \a b -> (a + b) * 2

-- higher-order function (HOF):

i :: (Integer -> Integer) -> Integer
i k = k 100

(.+.) :: Integer -> Integer -> Integer
(.+.) a b = (a + b) * 2

-- polymorphism

j :: (Integer -> a) -> a
j k = k 100

z :: anything -> anything
z c = c

y :: a -> b -> a
y    p    _ =  p    -- there is no other function that inhabits this type

-- datatypes

pie = 3

data Shape =                            -- data type
  Circle Integer                        -- data constructor
  | Rectangle Integer Integer           -- data constructor
  | Triangle Integer Integer Integer    -- data constructor
  deriving (Eq, Show)

perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Circle r -> r * 2 * pie
  Rectangle w h -> (w + h) * 2
  Triangle a b c -> a + b + c

perimeter2 :: Shape -> Integer
perimeter2 (Circle r) = r * 2 * pie
perimeter2 (Rectangle w h) = (w + h) * 2
perimeter2 (Triangle a b c) = a + b + c

data Three a = T a a a
  deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (T a b c) = a * b * c

m :: (a -> b) -> Three a -> Three b
m = \q -> \e -> case e of
  T a1 a2 a3 -> T (q a1) (q a2) (q a3)

-- isInList :: Eq a => a -> [a] -> Bool
-- isInList e list = 
--   case list of 
--     [] -> False
--     (x:xs) -> (e == x) || isInList e xs

data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)

addList :: List Integer -> Integer
addList = \w -> case w of
  Nil -> 0
  Cons h t -> h + addList t

-- Typeclasses

class Equal x where
  isEqual :: x -> x -> Bool

instance Equal TwoInts where
-- isEqual :: TwoInts -> TwoInts -> Bool
  isEqual (TI a1 b1) (TI a2 b2) = 
    (a1 == a2) && (b1 == b2)

instance Equal Bool where
-- isEqual :: Bool -> Bool -> Bool
  isEqual True True = True
  isEqual False False = True
  isEqual True False = False
  isEqual False True = False

data TwoInts = TI Int Int 
  deriving Show

isInList :: Equal a => a -> [a] -> Bool
isInList _ [] = False
isInList e (h:t) = (isEqual e h) || isInList e t
