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
    , rst_i :: Signal domain Bool
    , stb_i :: Signal domain Bool
    }

-----------------------------------------------------------------------------

data Output domain  =
  Output
    { ack_o :: Signal domain Bool
    , odd :: Signal domain Bool
    }

-----------------------------------------------------------------------------

data Functions a =
  Functions
    { checkOdd :: a -> Bool
    }

-----------------------------------------------------------------------------

data InitialState  =
  InitialState
    { ack_o :: Bool
    , odd :: Bool
    }

-----------------------------------------------------------------------------

data ControlIn domain =
  ControlIn
    { controlIn0 :: Signal domain Bit
    , controlIn1 :: Signal domain Bit
    , controlIn2 :: Signal domain Bit
    }

-----------------------------------------------------------------------------

data ControlOut domain =
  ControlOut
    { controlOut0 :: Signal domain Bit
    , controlOut1 :: Signal domain Bit
    , controlOut2 :: Signal domain Bit
    , controlOut3 :: Signal domain Bit
    , controlOut4 :: Signal domain Bit
    }

-----------------------------------------------------------------------------

control
  :: HiddenClockResetEnable domain 
  => Functions a
  -> InitialState 
  -> Input domain a
  -> Output domain 

control Functions{..} InitialState{..} Input{..} =
  let
    ack_oCell = register ack_o ack_oOut
    oddCell = register odd oddOut

    w3 = pure True
    w4 = pure False
    w8 = checkOdd <$> dat_i

    ControlOut{..} =
      controlCircuit
        ControlIn
          { controlIn0 = ((! 0) . pack) <$> cyc_i
          , controlIn1 = ((! 0) . pack) <$> stb_i
          , controlIn2 = ((! 0) . pack) <$> rst_i
          }

    ack_oOut =
      ack_oSwitch
        ( liftA2 (++#) (fmap pack controlOut0)
        $ fmap pack controlOut1
        )
        ( liftA2 (:>) w3
        $ liftA2 (:>) ack_oCell
        $ pure Nil
        )
    oddOut =
      oddSwitch
        ( liftA2 (++#) (fmap pack controlOut2)
        $ liftA2 (++#) (fmap pack controlOut3)
        $ fmap pack controlOut4
        )
        ( liftA2 (:>) w8
        $ liftA2 (:>) w4
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
  => Signal domain (BitVector 3)
  -> Signal domain (Vec 3 a)
  -> Signal domain a

oddSwitch = liftA2 select
  where
    select bs vs
      | bs ! 0 == high = vs !! 2
      | bs ! 1 == high = vs !! 1
      | otherwise      = vs !! 0

-----------------------------------------------------------------------------

controlCircuit
 :: HiddenClockResetEnable domain 
 => ControlIn domain -> ControlOut domain

controlCircuit ControlIn{..} = 
  let
    o2 = _not_ controlIn2
  in
    ControlOut
      { controlOut0 = pure high
      , controlOut1 = pure low
      , controlOut2 = o2
      , controlOut3 = controlIn2
      , controlOut4 = pure low
      }

  where
    _not_ = fmap complement

-----------------------------------------------------------------------------
