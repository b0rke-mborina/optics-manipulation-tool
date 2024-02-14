import OpticsManipulation

-- lens example: modifying the name of a Person using nameLens
exampleLens :: IO ()
exampleLens = do
  let person = Person "John" 25
  putStrLn $ "Original Person: " ++ show person

  let modifiedPerson = modifyOfLens omNameLens (\name -> Right $ "Dr. " ++ name) person
  case modifiedPerson of
    Right newPerson -> putStrLn $ "Modified Person: " ++ show newPerson
    Left error       -> putStrLn $ "Error: " ++ error

-- prism example: modifying the age of a Person using agePrism
examplePrism :: IO ()
examplePrism = do
  let age = 30
  putStrLn $ "Original Age: " ++ show age

  let modifiedAge = modifyOfPrism omAgePrism (\x -> if x >= 18 then Right (x + 5) else Left "Must be 18 or older") age
  case modifiedAge of
    Right newAge -> putStrLn $ "Modified Age: " ++ show newAge
    Left error   -> putStrLn $ "Error: " ++ error

-- traversal example: modifying the age of all employees in a Company using traverseEmployees
exampleTraversal :: IO ()
exampleTraversal = do
  let employees = [Person "Alice" 25, Person "Bob" 30, Person "Charlie" 22]
  let company = Company "TechCorp" employees

  putStrLn $ "Original Company: " ++ show company

  let modifiedCompany = modifyOfTraversal omTraverseEmployees (\person -> Right $ person { _age = _age person + 1 }) company
  case modifiedCompany of
    Right newCompany -> putStrLn $ "Modified Company: " ++ show newCompany
    Left error       -> putStrLn $ "Error: " ++ error

-- iso example: converting between Celsius, Fahrenheit and Kelvin
exampleIso :: IO ()
exampleIso = do
  let temperatureCelsius = Celsius 25.0
  putStrLn $ "Original Temperature in Celsius: " ++ show temperatureCelsius

  -- converting Celsius to Fahrenheit using iso
  let temperatureFahrenheit = to omCelsiusToFahrenheit temperatureCelsius
  putStrLn $ "Converted Temperature in Fahrenheit: " ++ show temperatureFahrenheit

  -- converting from Fahrenheit back to Celsius using iso
  let temperatureCelsius' = from omCelsiusToFahrenheit temperatureFahrenheit
  putStrLn $ "Converted Temperature back to Celsius: " ++ show temperatureCelsius'

  putStrLn $ "Original Temperature in Celsius: " ++ show temperatureCelsius

  putStrLn "----------------------------"

  -- converting Celsius to Kelvin using composed isos
  let celsiusToKelvin = composeIsos omCelsiusToFahrenheit omFahrenheitToKelvin
  let temperatureKelvin = to celsiusToKelvin temperatureCelsius
  putStrLn $ "Converted Temperature in Kelvin: " ++ show temperatureKelvin

  -- converting Kelvin back to Celsius using chained isos
  let kelvinToCelsius = composeIsos omCelsiusToFahrenheit omFahrenheitToKelvin
  let temperatureCelsius'' = from kelvinToCelsius temperatureKelvin
  putStrLn $ "Converted Temperature back to Celsius: " ++ show (roundTo 2 $ case temperatureCelsius'' of Celsius c -> c)

  putStrLn $ "Original Temperature in Celsius: " ++ show temperatureCelsius

  -- modifying the temperature using iso
  let modifyTemperature = modifyOfIso omCelsiusToFahrenheit (\(Fahrenheit f) -> Right $ Fahrenheit (f + 10))
  case modifyTemperature temperatureCelsius of
    Left err -> putStrLn $ "Error: " ++ err
    Right modifiedTemperature -> putStrLn $ "Modified Temperature in Celsius: " ++ show modifiedTemperature

  putStrLn $ "Original Temperature in Celsius: " ++ show temperatureCelsius

  -- modifying the temperature with error handling using iso
  let modifyTemperatureWithErr = modifyOfIso omCelsiusToFahrenheit $ \(Fahrenheit f) ->
       if f + 10 < -459.67 then Left "Invalid Fahrenheit temperature" else Right $ Fahrenheit (f + 10)

  case modifyTemperatureWithErr temperatureCelsius of
    Left err -> putStrLn $ "Error: " ++ err
    Right modifiedTemperature -> putStrLn $ "Modified Temperature in Celsius: " ++ show modifiedTemperature

-- prism example: modifying the age of a Person using composed prisms
exampleChainedPrisms :: IO ()
exampleChainedPrisms = do
  let age = 30
  putStrLn $ "Original Age: " ++ show age

  let composedPrism = composePrism omAgePrism (Prism' { matcher = \x -> if even x then Right x else Left "Must be even", builder = (*2) })
  let modifiedAge = modifyOfPrism composedPrism (\x -> Right (x + 1)) age

  case modifiedAge of
    Right newAge -> putStrLn $ "Modified Age with Composed Prism: " ++ show newAge
    Left error   -> putStrLn $ "Error: " ++ error

main :: IO ()
main = do
  putStrLn "Lens Example:"
  exampleLens

  putStrLn "\nPrism Example:"
  examplePrism

  putStrLn "\nTraversal Example:"
  exampleTraversal

  putStrLn "\nIso Example:"
  exampleIso

  putStrLn "\nChained Prisms Example:"
  exampleChainedPrisms
