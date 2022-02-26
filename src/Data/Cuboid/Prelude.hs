{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Data.Cuboid.Prelude
  ( module X
  , addToEnum
  ) where

import Protolude                    as X hiding ( from
                                                , to
                                                )
import Control.Arrow                as X        ( (>>>) )
import Control.Lens                 as X        ( (%~)
                                                , (*~)
                                                , (+~)
                                                , (-~)
                                                , (.~)
                                                , (//~)
                                                , (?~)
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , Each
                                                , Getter
                                                , Iso
                                                , Iso'
                                                , Lens
                                                , Lens'
                                                , Prism
                                                , Prism'
                                                , _Just
                                                , _1
                                                , _2
                                                , ala
                                                , coerced
                                                , each
                                                , from
                                                , iso
                                                , ix
                                                , lens
                                                , preview
                                                , prism
                                                , review
                                                , set
                                                , to
                                                , view
                                                )
import Data.Finite                  as X        ( Finite
                                                , getFinite
                                                , packFinite
                                                )
import GHC.Natural                  as X        ( Natural )
import GHC.TypeNats                 as X        ( type (+)
                                                , natVal'
                                                )

addToEnum :: Enum k => Int -> k -> k
addToEnum n = toEnum . (+n) . fromEnum

-- ( (...)
--                                                 , (|>)
--                                                 , cs
--                                                 , nl
--                                                 , onCrash
--                                                 , orCrash
--                                                 )
