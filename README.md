# Aeson from Example

## Generate Haskell data type definitions based in json examples.

Writing haskell types based on API definitions is tedious. This program takes a
JSON example and generates data types based on it for you.  It is pretty simple
and won't handle many edgecases, but it is good enough as a starting point in
many situations

## Example usage

* Input file

```json
{ "Status": "running",
  "Running": true,
  "Paused": false,
  "Restarting": false,
  "OOMKilled": false,
  "Dead": [false, true],
  "Pids": [],
  "ExitCode": 10,
  "Error": null,
  "StartedAt": "2016-06-14T11:28:33.270113396Z",
  "FinishedAt": "0001-01-01T00:00:00Z"
}
```

invocation

```
aeson-from-example -i infile.json -r MyType
```

output


```haskell
data MyType = MyType
  { myTypeStatus     :: Text
  , myTypeDead       :: [Bool]
  , myTypeRestarting :: Bool
--  , myTypeError      :: Unknown
--  , myTypePids       :: [Unknown]
  , myTypeStartedAt  :: Text
  , myTypeFinishedAt :: Text
  , myTypeRunning    :: Bool
  , myTypeExitCode   :: Scientific
  , myTypePaused     :: Bool
  , myTypeOOMKilled  :: Bool
  }
  deriving (Show, Typeable, Data, Generic)

deriveJSON (aesonOptions "myType") ''MyType
makeLensesWith camelCaseFields ''MyType
```

## Limitations

* Types of empty lists and null fields can't be guessed, so they are commented out
* Will only consider the first element of a list for inferring the type (can't
  handle heterogenous lists)