module Debug.DumbTrace (trace) where
trace :: a -> b -> b
trace _ = id
