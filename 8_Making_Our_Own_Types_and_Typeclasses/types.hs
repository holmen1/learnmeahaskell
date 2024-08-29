-- 8 Making Our Own Types and Typeclasses

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


-- Use record syntax when a constructor has several fields and it's not obvious which field is which
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)


-- 3d vector
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- Derived instances
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)


{- Type parameters
A value constructor can take some values parameters and then produce a new value.
In a similar manner, type constructors can take types as parameters to produce new types.
To get a clear picture of what type parameters work like in action, let's take a look at
how a type we've already met is implemented. -}
data Maybe a = Nothing | Just a
-- The a here is the type parameter. And because there's a type parameter involved,
-- we call Maybe a type constructor.


{- The Functor typeclass
which is basically for things that can be mapped over.
What better way to get to know the Functor typeclass than to see how it's implemented? -}

class Functor f where
    fmap :: (a -> b) -> f a -> f b

{- we see that fmap takes a function from one type to another and a functor applied
with one type and returns a functor applied with another type.
Hmm, this type declaration for fmap reminds me of something. If you don't know what
the type signature of map is, it's: map :: (a -> b) -> [a] -> [b].
In fact, map is just a fmap that works only on lists. Here's how the list is an instance
of the Functor typeclass.

instance Functor [] where
    fmap = map -}

{- The Functor typeclass wants a type constructor that takes only one type parameter but
Either takes two. Hmmm! I know, we'll partially apply Either by feeding it only one
parameter so that it has one free parameter. Here's how Either a is a functor in the
standard libraries:

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
Well well, what did we do here? You can see how we made Either a an instance instead of
just Either. That's because Either a is a type constructor that takes one parameter,
whereas Either takes two. If fmap was specifically for Either a, the type signature would
then be (b -> c) -> Either a b -> Either a c because that's the same as
(b -> c) -> (Either a) b -> (Either a) c. In the implementation, we mapped in the case of
a Right value constructor, but we didn't in the case of a Left. Why is that? Well, if we
look back at how the Either a b type is defined, it's kind of like:

data Either a b = Left a | Right b
Well, if we wanted to map one function over both of them, a and b would have to be the same type.
Also, from seeing what fmap's type would be if it operated only on Either values, we see that the
first parameter has to remain the same while the second one can change and the first parameter is
actualized by the Left value constructor.
This also goes nicely with our box analogy if we think of the Left part as sort of an empty box
with an error message written on the side telling us why it's empty. -}

-- ghci> fmap (*2) $ Left 4
-- Left 4
-- ghci> fmap (*2) $ Right 4
-- Right 8