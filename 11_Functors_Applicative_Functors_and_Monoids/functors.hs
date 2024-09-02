{- Functors
If we want to make a type constructor an instance of Functor,
============================
IMPORTANT: it has to have a kind of * -> *,
which means that it has to take exactly one concrete type as a type parameter.
============================
For example,
Maybe can be made an instance because it takes one type parameter to produce a concrete type,
like Maybe Int or Maybe String. If a type constructor takes two parameters, like Either, we have
to partially apply the type constructor until it only takes one type parameter. So we can't write
instance Functor Either where, but we can write instance Functor (Either a) where and then if we
imagine that fmap is only for Either a, it would have a type declaration of
fmap :: (b -> c) -> Either a b -> Either a c.
As you can see, the Either a part is fixed, because Either a takes only one type parameter,
whereas just Either takes two so fmap :: (b -> c) -> Either b -> Either c wouldn't really make sense.

Just like when we fmap reverse over Just "blah" to get Just "halb", we can fmap reverse over getLine.
getLine is an I/O action that has a type of IO String and mapping reverse over it gives us an I/O
action that will go out into the real world and get a line and then apply reverse to its result.
Like we can apply a function to something that's inside a Maybe box, we can apply a function to what's
inside an IO box. Then when we bind it to a name by using <-.

If we look at what fmap's type would be if it were limited to IO, it would be
fmap :: (a -> b) -> IO a -> IO b. fmap takes a function and an I/O action and returns a new I/O action
that's like the old one, except that the function is applied to its contained result. -}

import Data.Char
import Data.List
import qualified Control.Applicative as it

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
-- $ runhaskell functors.hs
-- hello there
-- E-R-E-H-T- -O-L-L-E-H

{- Reader Functor
Another instance of Functor that we've been dealing with all along but didn't know was a Functor is (->) r.

We usually mark functions that take anything and return anything as a -> b. r -> a is the same thing,
we just used different letters for the type variables.

instance Functor f where
    fmap :: (a -> b) -> f a -> f b.
    ...

instance Functor ((->) r) where
    fmap :: (a -> b) -> (->) r a -> (->) r b = (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = (\x -> f (g x))

Does this remind you of anything? Yes! Function composition! We pipe the output of r -> a into the input of
a -> b to get a function r -> b, which is exactly what function composition is about. If you look at how the
instance is defined above, you'll see that it's just function composition.
Another way to write this instance would be:

instance Functor ((->) r) where
    fmap = (.)

This makes the revelation that using fmap over functions is just composition sort of obvious.

ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (+100) 1
"303"

We can call fmap as an infix function so that the resemblance to . is clear. In the second input
line, we're mapping (*3) over (+100), which results in a function that will take an input, call
(+100) on that and then call (*3) on that result. We call that function with 1.

The function being mapped over a computation results in the same computation but the result of that
computation is modified with the function.

If we write fmap :: (a -> b) -> (f a -> f b), we can think of fmap not as a function
that takes one function and a functor and returns a functor, but as a function that
takes a function and returns a new function that's just like the old one, only it takes
a functor as a parameter and returns a functor as the result. It takes an a -> b function
and returns a function f a -> f b. This is called lifting a function. -}

{- Functor Laws
The first functor law states that if we map the id function over a functor, the functor
that we get back should be the same as the original functor.

fmap id = id

The second law says that composing two functions and then mapping the resulting function
over a functor should be the same as first mapping one function over the functor and then
mapping the other one.

fmap (f . g) = fmap f . fmap g
 -}


{- Applicative Functors
So far, when we were mapping functions over functors, we usually mapped functions
that take only one parameter. But what happens when we map a function like *, which
takes two parameters, over a functor? Let's take a look at a couple of concrete
examples of this. If we have Just 3 and we do fmap (*) (Just 3), what do we get?
From the instance implementation of Maybe for Functor, we know that if it's a
Just something value, it will apply the function to the something inside the Just.
Therefore, doing fmap (*) (Just 3) results in Just ((*) 3), which can also be written
as Just (* 3) if we use sections. Interesting! We get a function wrapped in a Just!

But what if we have a functor value of Just (3 *) and a functor value of Just 5 and
we want to take out the function from Just (3 *) and map it over Just 5? With normal
functors, we're out of luck, because all they support is just mapping normal functions
over existing functors. But we can't map a function that's inside a functor over
another functor with what fmap offers us. We could pattern-match against the Just
constructor to get the function out of it and then map it over Just 5, but we're
looking for a more general and abstract way of doing that, which works across functors.

Meet the Applicative typeclass. It lies in the Control.Applicative module and it defines
two methods, pure and <*>. It doesn't provide a default implementation for any of them,
so we have to define them both if we want something to be an applicative functor.
The class is defined like so:

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

The first method it defines is called pure. pure should take a value of any type and
return an applicative functor with that value inside it.

The <*> function is really interesting.
Whereas fmap takes a function and a functor and applies the function inside the functor,
<*> takes a functor that has a function in it and another functor and sort of extracts
that function from the first functor and then maps it over the second one.

Let's take a look at the Applicative instance implementation for Maybe.

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

With normal functors, you can just map a function over a functor and then you can't get
the result out in any general way, even if the result is a partially applied function.
Applicative functors, on the other hand, allow you to operate on several functors with
a single function. Check out this piece of code:

ghci> pure (+) <*> Just 3 <*> Just 5
Just 8

Let's take a look, step by step. <*> is left-associative, which means that
pure (+) <*> Just 3 <*> Just 5 is the same as (pure (+) <*> Just 3) <*> Just 5. First,
the + function is put in a functor, which is in this case a Maybe value that contains
the function. So at first, we have pure (+), which is Just (+). Next, Just (+) <*> Just 3
happens. The result of this is Just (3+). This is because of partial application. Only
applying 3 to the + function results in a function that takes one parameter and adds 3
to it. Finally, Just (3+) <*> Just 5 is carried out, which results in a Just 8.

Applicative functors and the applicative style of doing pure f <*> x <*> y <*> ... allow
us to take a function that expects parameters that aren't necessarily wrapped in functors
and use that function to operate on several values that are in functor contexts.
The function can take as many parameters as we want, because it's always partially applied
step by step between occurences of <*>.

Using the applicative style on lists is often a good replacement for list comprehensions.
In the second chapter, we wanted to see all the possible products of [2,5,10] and [8,10,11],
so we did this:

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]   
[16,20,22,40,50,55,80,100,110]   

This can be done in the applicative style as well:

ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
-}

{- Applicative Functor Laws
Like normal functors, applicative functors come with a few laws:

pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
 -}

{- In conclusion, applicative functors aren't just interesting, they're also useful,
because they allow us to combine different computations, such as I/O computations,
non-deterministic computations, computations that might have failed, etc. by using
the applicative style. Just by using <$> and <*> we can use normal functions to
uniformly operate on any number of applicative functors and take advantage of the
semantics of each one. -}

