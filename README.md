# Aeson from Example

## Generate Haskell data type definitions based in json examples.

Writing Haskell types based on API definitions is tedious boilerplate. This
program takes a JSON example and generates data types based on it for you.  It
is pretty simple and won't handle many edgecases, but it is good enough as a
starting point in many situations. It will also automatically create the
{To|From}JSON instances.

The output is meant to be used as a starting point for editing.


## Example usage

* Input file

```json
{ "Status": "running",
  "Running": true,
  "GraphDriver": {
        "Name": "btrfs",
        "Data": null
    },
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
data GraphDriver = GraphDriver
  { -- graphDriverData :: Unknown
    graphDriverName :: Text
  }
  deriving (Show, Typeable, Data, Generic)

instance ToJSON GraphDriver where
  toJSON GraphDriver
    { -- graphDriverData = graphDriverData
      graphDriverName = graphDriverName
    } =
    object
    [ -- "Data" .= graphDriverData
      "Name" .= graphDriverName
    ]


instance FromJSON GraphDriver where
  parseJSON = withObject "GraphDriver" $ \o -> do
    -- graphDriverData <- o .: "Data"
     graphDriverName <- o .: "Name"
    return GraphDriver
      { -- graphDriverData = graphDriverData
        graphDriverName = graphDriverName
      }

makeLensesWith camelCaseFields ''GraphDriver


data MyType = MyType
  { myTypeStatus      :: Text
  , myTypeDead        :: [Bool]
  , myTypeRestarting  :: Bool
--  , myTypeError       :: Unknown
--  , myTypePids        :: [Unknown]
  , myTypeStartedAt   :: Text
  , myTypeFinishedAt  :: Text
  , myTypeRunning     :: Bool
  , myTypeGraphDriver :: GraphDriver
  , myTypeExitCode    :: Scientific
  , myTypeOOMKilled   :: Bool
  }
  deriving (Show, Typeable, Data, Generic)

instance ToJSON MyType where
  toJSON MyType
    { myTypeStatus      = myTypeStatus
    , myTypeDead        = myTypeDead
    , myTypeRestarting  = myTypeRestarting
--    , myTypeError       = myTypeError
--    , myTypePids        = myTypePids
    , myTypeStartedAt   = myTypeStartedAt
    , myTypeFinishedAt  = myTypeFinishedAt
    , myTypeRunning     = myTypeRunning
    , myTypeGraphDriver = myTypeGraphDriver
    , myTypeExitCode    = myTypeExitCode
    , myTypeOOMKilled   = myTypeOOMKilled
    } =
    object
    [ "Status"      .= myTypeStatus
    , "Dead"        .= myTypeDead
    , "Restarting"  .= myTypeRestarting
--    , "Error"       .= myTypeError
--    , "Pids"        .= myTypePids
    , "StartedAt"   .= myTypeStartedAt
    , "FinishedAt"  .= myTypeFinishedAt
    , "Running"     .= myTypeRunning
    , "GraphDriver" .= myTypeGraphDriver
    , "ExitCode"    .= myTypeExitCode
    , "OOMKilled"   .= myTypeOOMKilled
    ]


instance FromJSON MyType where
  parseJSON = withObject "MyType" $ \o -> do
    myTypeStatus      <- o .: "Status"
    myTypeDead        <- o .: "Dead"
    myTypeRestarting  <- o .: "Restarting"
--    myTypeError       <- o .: "Error"
--    myTypePids        <- o .: "Pids"
    myTypeStartedAt   <- o .: "StartedAt"
    myTypeFinishedAt  <- o .: "FinishedAt"
    myTypeRunning     <- o .: "Running"
    myTypeGraphDriver <- o .: "GraphDriver"
    myTypeExitCode    <- o .: "ExitCode"
    myTypeOOMKilled   <- o .: "OOMKilled"
    return MyType
      { myTypeStatus      = myTypeStatus
      , myTypeDead        = myTypeDead
      , myTypeRestarting  = myTypeRestarting
--      , myTypeError       = myTypeError
--      , myTypePids        = myTypePids
      , myTypeStartedAt   = myTypeStartedAt
      , myTypeFinishedAt  = myTypeFinishedAt
      , myTypeRunning     = myTypeRunning
      , myTypeGraphDriver = myTypeGraphDriver
      , myTypeExitCode    = myTypeExitCode
      , myTypeOOMKilled   = myTypeOOMKilled
      }

makeLensesWith camelCaseFields ''MyType

```

Note how it created an addition record type for the nested object. The name of
that type is based on the field name where the nested object appeared.

## Limitations

* Types of empty lists and null fields can't be guessed, so they are commented out
* Will only consider the first element of a list for inferring the type (can't
  handle heterogenous lists)
* Makes no effort to detect duplicate type definitions, name collisions etc.
