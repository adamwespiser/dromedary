# dromedary

What we want client code to look like:

```

data Task start stop = Task {
  start :: (start -> IO ())
, scale :: IO PassFail
, stop  :: (stop -> IO ())
}




main = IO ()
main = h


```
