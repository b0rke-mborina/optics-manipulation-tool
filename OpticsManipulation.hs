module OpticsManipulation
  ( Lens'(..)
  , Prism'(..)
  , Traversal'(..)
  , Iso(..)
  , Person(..)  -- exporting the data constructor
  , Company(..)  -- exporting the data constructor
  , Celsius(..)  -- exporting the data constructor
  , Fahrenheit(..)  -- exporting the data constructor
  , Kelvin(..)  -- exporting the data constructor
  , omNameLens
  , omAgePrism
  , omTraverseEmployees
  , composeLens
  , composePrism
  , composeTraversal
  , modifyOfLens
  , modifyOfPrism
  , modifyOfTraversal
  , modifyOf  -- exporting the helper function
  , modifyOfIso
  , omCelsiusToFahrenheit
  , omFahrenheitToKelvin
  , composeIsos
  , roundTo  -- exporting the helper function
  ) where

-- lens, prism and traversal: sample data types
data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving (Show)

data Company = Company
  { _companyName :: String
  , _employees   :: [Person]
  } deriving (Show)

-- lens, prism and traversal types
data Lens' s a = Lens' { getter :: s -> a, setter :: s -> a -> s }

data Prism' s a = Prism' { matcher :: s -> Either String a, builder :: a -> s }

data Traversal' s a = Traversal' { traverser :: s -> [a], modifier :: s -> [a] -> s }

-- lens, prism and traversal creation
omNameLens :: Lens' Person String
omNameLens = Lens'
  { getter = _name
  , setter = \person newName -> person { _name = newName }
  }

omAgePrism :: Prism' Int Int
omAgePrism = Prism'
  { matcher = \x -> if x >= 0 then Right x else Left "Age cannot be negative"
  , builder = id
  }

omTraverseEmployees :: Traversal' Company Person
omTraverseEmployees = Traversal'
  { traverser = _employees
  , modifier = \company people -> company { _employees = people }
  }

-- composition and chaining (lens, prism and traversal)
composeLens :: Lens' a b -> Lens' b c -> Lens' a c
composeLens l1 l2 = Lens'
  { getter = getter l2 . getter l1
  , setter = \a c -> setter l1 a (setter l2 (getter l1 a) c)
  }

composePrism :: Prism' a b -> Prism' b c -> Prism' a c
composePrism p1 p2 = Prism'
  { matcher = \a -> matcher p1 a >>= matcher p2
  , builder = builder p1 . builder p2
  }

composeTraversal :: Traversal' a b -> Traversal' b c -> Traversal' a c
composeTraversal t1 t2 = Traversal'
  { traverser = concatMap (traverser t2) . traverser t1
  , modifier = \a cs -> let
      traversedValues = traverser t1 a
      modifiedValues = traverser t2 =<< traversedValues
    in modifier t1 a traversedValues
  }

-- traversal and modification (lens, prism and traversal)
modifyOfLens :: Lens' s a -> (a -> Either String a) -> s -> Either String s
modifyOfLens lens f s = do
  oldValue <- Right (getter lens s)
  newValue <- f oldValue
  pure (setter lens s newValue)

modifyOfPrism :: Prism' s a -> (a -> Either String a) -> s -> Either String s
modifyOfPrism prism f s = matcher prism s >>= f >>= pure . builder prism

modifyOfTraversal :: Traversal' s a -> (a -> Either String a) -> s -> Either String s
modifyOfTraversal traversal f s =
  modifier traversal s <$> traverse f (traverser traversal s)

-- helper function for traversing and modifying a structure
modifyOf :: Traversal' s a -> (a -> Either String a) -> s -> Either String s
modifyOf traversal f s =
  let
    traverseOf' :: Traversal' s a -> (a -> Either String a) -> [a] -> Either String [a]
    traverseOf' _ _ [] = Right []
    traverseOf' traversal' g (x:xs) = do
      y <- g x
      ys <- traverseOf' traversal' g xs
      pure (y : ys)
  in
    modifier traversal s <$> traverseOf' traversal f (traverser traversal s)


-- iso: sample data types
data Celsius = Celsius Double deriving (Show)
data Fahrenheit = Fahrenheit Double deriving (Show)
data Kelvin = Kelvin Double deriving (Show)

-- iso type
data Iso s a = Iso { to :: s -> a, from :: a -> s }

-- Rounding function
roundTo :: Int -> Double -> Double
roundTo n f = fromIntegral (round $ f * 10^n) / 10^n

-- traversal and modification with error handling (iso)
modifyOfIso :: Iso s a -> (a -> Either String a) -> s -> Either String s
modifyOfIso iso f s = case f (to iso s) of
   Left err -> Left err
   Right newValue -> Right (from iso newValue)

-- iso creation
omCelsiusToFahrenheit :: Iso Celsius Fahrenheit
omCelsiusToFahrenheit = Iso
   { to = \(Celsius c) -> Fahrenheit (c * 9 / 5 + 32)
   , from = \(Fahrenheit f) -> Celsius ((f - 32) * 5 / 9)
   }

omFahrenheitToKelvin :: Iso Fahrenheit Kelvin
omFahrenheitToKelvin = Iso
   { to = \(Fahrenheit f) -> Kelvin ((f - 32) * 5 / 9 + 273.15)
   , from = \(Kelvin k) -> Fahrenheit (k * 9 / 5 - 459.67)
   }

-- composition and chaining (iso)
composeIsos :: Iso a b -> Iso b c -> Iso a c
composeIsos iso1 iso2 = Iso
   { to = to iso2 . to iso1
   , from = from iso1 . from iso2
   }
