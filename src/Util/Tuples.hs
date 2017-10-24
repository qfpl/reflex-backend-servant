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

instance TupleList (a, (b, (c, (d, ())))) where
  type RevTupleListFlatten (a, (b, (c, (d, ())))) =
    (d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, ())))) =
    (d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, ()))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, ()))))) =
    (e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, ()))))) =
    (e, d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, (f, ())))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, (f, ())))))) =
    (f, e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, (f, ())))))) =
    (f, e, d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, (f, (g, ()))))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, (f, (g, ()))))))) =
    (g, f, e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, (f, (g, ()))))))) =
    (g, f, e, d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, (f, (g, (h, ())))))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, ())))))))) =
    (h, g, f, e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, ())))))))) =
    (h, g, f, e, d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, (f, (g, (h, (i, ()))))))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, (i, ()))))))))) =
    (i, h, g, f, e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, (i, ()))))))))) =
    (i, h, g, f, e, d, c, b, a)

instance TupleList (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, ())))))))))) where
  type RevTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, ())))))))))) =
    (j, i, h, g, f, e, d, c, b, a)

  revTupleListFlatten (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, ())))))))))) =
    (j, i, h, g, f, e, d, c, b, a)
