{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.Tuples (
    TupleList(..)
  ) where

class TupleList xs where
  type RevTupleListFlatten xs

  revTupleListFlatten :: xs -> RevTupleListFlatten xs

instance TupleList () where
  type RevTupleListFlatten () =
    ()

  revTupleListFlatten _ =
    ()

instance TupleList (a, ()) where
  type RevTupleListFlatten (a, ()) =
    a

  revTupleListFlatten (a, ()) =
    a

instance TupleList (a, (b, ())) where
  type RevTupleListFlatten (a, (b, ())) =
    (b, a)

  revTupleListFlatten (a, (b, ())) =
    (b, a)

instance TupleList (a, (b, (c, ()))) where
  type RevTupleListFlatten (a, (b, (c, ()))) =
    (c, b, a)

  revTupleListFlatten (a, (b, (c, ()))) =
    (c, b, a)
