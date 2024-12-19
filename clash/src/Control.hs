-----------------------------------------------------------------------------
-- |
-- Module : Control
--
-- Clash Interface for control.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , DuplicateRecordFields

  #-}

module Control
  ( Input(..)
  , Output(..)
  , Functions(..)
  , InitialState(..)
  , control
  ) where

-----------------------------------------------------------------------------

import Clash.Prelude

-----------------------------------------------------------------------------

data Input domain a =
  Input
    { dat_i :: Signal domain a
    , cyc_i :: Signal domain Bool
    , stb_i :: Signal domain Bool
    }

-----------------------------------------------------------------------------

data Output domain b =
  Output
    { ack_o :: Signal domain Bool
    , odd :: Signal domain b
    }

-----------------------------------------------------------------------------

data Functions a b =
  Functions
    { checkOdd :: a -> b
    }

-----------------------------------------------------------------------------

data InitialState b =
  InitialState
    { ack_o :: Bool
    , odd :: b
    }

-----------------------------------------------------------------------------

data ControlIn domain =
  ControlIn
    { controlIn0 :: Signal domain Bit
    , controlIn1 :: Signal domain Bit
    }

-----------------------------------------------------------------------------

data ControlOut domain =
  ControlOut
    { controlOut0 :: Signal domain Bit
    , controlOut1 :: Signal domain Bit
    , controlOut2 :: Signal domain Bit
    , controlOut3 :: Signal domain Bit
    }

-----------------------------------------------------------------------------

control
  :: HiddenClockResetEnable domain
  => ( NFDataX b)
  => Functions a b
  -> InitialState b
  -> Input domain a
  -> Output domain b

control Functions{..} InitialState{..} Input{..} =
  let
    ack_oCell = register ack_o ack_oOut
    oddCell = register odd oddOut

    w3 = checkOdd <$> dat_i
    w4 = pure True

    ControlOut{..} =
      controlCircuit
        ControlIn
          { controlIn0 = ((! 0) . pack) <$> cyc_i
          , controlIn1 = ((! 0) . pack) <$> stb_i
          }

    ack_oOut =
      ack_oSwitch
        ( liftA2 (++#) (fmap pack controlOut0)
        $ fmap pack controlOut1
        )
        ( liftA2 (:>) w4
        $ liftA2 (:>) ack_oCell
        $ pure Nil
        )
    oddOut =
      oddSwitch
        ( liftA2 (++#) (fmap pack controlOut2)
        $ fmap pack controlOut3
        )
        ( liftA2 (:>) w3
        $ liftA2 (:>) oddCell
        $ pure Nil
        )
  in
    Output
      { ack_o = ack_oOut
      , odd = oddOut
      }

-----------------------------------------------------------------------------

ack_oSwitch
  :: HiddenClockResetEnable domain
  => Signal domain (BitVector 2)
  -> Signal domain (Vec 2 a)
  -> Signal domain a

ack_oSwitch = liftA2 select
  where
    select bs vs
      | bs ! 0 == high = vs !! 1
      | otherwise      = vs !! 0

-----------------------------------------------------------------------------

oddSwitch
  :: HiddenClockResetEnable domain 
  => Signal domain (BitVector 2)
  -> Signal domain (Vec 2 a)
  -> Signal domain a

oddSwitch = liftA2 select
  where
    select bs vs
      | bs ! 0 == high = vs !! 1
      | otherwise      = vs !! 0

-----------------------------------------------------------------------------

controlCircuit
 :: HiddenClockResetEnable domain
 => ControlIn domain -> ControlOut domain

controlCircuit ControlIn{..} = 
    ControlOut
      { controlOut0 = pure high
      , controlOut1 = pure low
      , controlOut2 = pure high
      , controlOut3 = pure low
      }

-----------------------------------------------------------------------------