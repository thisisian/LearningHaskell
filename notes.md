## Misc

'ys@(x:xs)' binds ys to (x:xs).

## `seq`

'seq' is a function used to enforce strict evaluation. It evaluates both arguments, returns bottom if first argument is bottom, and otherwise returns second argument.

Needs to be first thing in expression.
Need to use a 'let' to give the expression in the first argument a variable so it can be evaluated and reused in the second argument.

## Modules

Define a module and export functions `f1` and `f2` and all data contructors of type `T`:

```module (f1, f2, T(..))```

Import functions `f1` and `f2` from module `M`

```import M (f1, f2)```

## Types

Example: A `Just` type:

```
data Maybe a = Just a
             | Nothing
```

`Maybe` is a *type constructor*
`Just` and `Nothing` are *data constructors*.

## `typeclass`

Defines an interface.

Note: Easy to define a new interface for a type outside your module, but difficult to define a new subtype. This differs from other languages like C++ where it's easy to subclass an abstract class, but difficult to  (This is an example of the Expression Problem)

## `newtype`
Easy way to wrap a type in another type:
```newtype MyType a = MyType { getValue:: a }```

Faster than using `data`. Allow for type enforcement, but bypasses the need to unwrap the value from its container.

## 'Functor' typeclass:

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

`fmap` maps a function to a type's value and retains the type in its output.

```
> fmap (+1) (Just 1)
Just 2
```

Functors must obey two laws:
1. `fmap (id) (f a) = f a`
2. `fmap (a . b) (f a) = fmap a (fmap b (f a))`

## `Applicative` typeclass:

```
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

`pure` - take a value and return the value inside a functor.
`<*>` - Take a functor with a function in it and a the functor with a in it and return the functor with b in it.

We can use like so:
```
> pure (+) <*> Just 5 <*> Just 9
Just 14
```

Can be used for sequencing IO actions:
```
> (++) <*> IO getStr <*> IO getStr
```
Asks for two lines of text and concatenates them.

Functions can be `Applicative`s:
```
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
```

An example computation using functions as `Applicative`:

```
(+) <$> (+ 3) <*> (* 100) $ 5
=> (\_ -> (+)) <*> (+ 3) <*> (* 100) $ 5
=> \x -> (\_ -> (+)) x (+ 3 x) <*> (* 100) $ 5
=> \x -> (+) (+ 3 x) <*> (* 100) $ 5
=> \y -> (\x -> (+) (+ 3 x)) y (* 100 y) $ 5
=> (\x -> (+) (+ 3 x)) 5 (* 100 5)
=> (+) (+ 3 5) (* 100 5)
=> (+) 8 500
=> 508
```

## `Monad` typeclass

Allows us to compose computations together and carry extra data resulting from the computation.

```
class Monad m where
  (>>=)  :: m a -> (  a -> m b) -> m b
  (>>)   :: m a ->  m b         -> m b
  return ::   a                 -> m a
```

`do` notation is syntactic sugar:

```
do {
  x <- thing1 ;
  y <- func1 x ;
  thing2 ;
  z <- func2 y ;
  return z
  }
```

is equivalent to:

```
thing1  >>= \x ->
func1 x >>= \y ->
thing2  >>= \_ ->
 func2 y >>= \z ->
return z
```