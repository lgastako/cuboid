{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Cuboid.Prelude
  ( module X
  , (|>)
  , addToEnum
  , cs
  , underEnum
  ) where

import Protolude                    as X hiding ( from
                                                , to
                                                )
import Control.Arrow                as X        ( (>>>)
                                                , (&&&)
                                                )
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
import Data.String                  as X        ( IsString( fromString )
                                                , String
                                                )
import GHC.Natural                  as X        ( Natural )
import GHC.TypeNats                 as X        ( type (+)
                                                , natVal'
                                                )
import Protolude.Conv                           ( Leniency( Lenient )
                                                , StringConv( strConv )
                                                )

infixr 9 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)

addToEnum :: forall k. Enum k => Int -> k -> k
addToEnum n = underEnum (+n)

cs :: forall a b. StringConv a b => a -> b
cs = strConv Lenient

underEnum :: Enum k => (Int -> Int) -> k -> k
underEnum f = toEnum . f . fromEnum
