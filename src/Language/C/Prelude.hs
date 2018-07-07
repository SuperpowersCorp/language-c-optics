module Language.C.Prelude
    ( module Exports
    ) where

import Control.Lens as Exports
import Protolude    as Exports hiding ( (&)
                                      , (<.>)
                                      , (<&>)  -- sigh
                                      , Strict
                                      , from
                                      , to
                                      , uncons
                                      , unsnoc
                                      )

