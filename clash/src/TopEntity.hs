-----------------------------------------------------------------------------
-- |
-- Module      :  TopEntity
-- Maintainer  :  Sparsa Roychowdhury (sparsa.roychowdhury@tu-clausthal.de, Felix Klein (klein@react.uni-saarland.de)
--
-- Kitchen Timer Control
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds, LambdaCase, MultiWayIf, RecordWildCards, CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------

module TopEntity where

-----------------------------------------------------------------------------

import Clash.Prelude
import Add (Input(..), Functions(..), InitialState (..), add)
import Cube (Input(..), Functions(..), InitialState (..), cube)
import Sqroot (Input(..), Functions(..), InitialState (..),sqroot )

import qualified Add as A (Output (..))
import qualified Cube as C(Output (..))
import qualified Sqroot as S (Output (..))

-----------------------------------------------------------------------------

{-# ANN topEntity (
  Synthesize
    { t_name    = "wishbone_reciever"
    , t_inputs  =
        [ PortName "CLOCK"
        , PortName "RESET"
        , PortName "STB_I"
        , PortName "CYC_I"
        , PortName "DAT_I_1"
        ]
    , t_output =
        PortProduct ""
          [ 
          PortName "ack_o"
          ,PortName "result"
          ]
    }
  )
  #-}

-----------------------------------------------------------------------------

--type Digit = Unsigned 7

-----------------------------------------------------------------------------
--type Width = BitVector (BitSize Width)
type Width = Unsigned 32

--------------------------------

topEntity
  :: Clock System
  -> Reset System
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System Width
  -> Signal System (Bool,  Width  )

topEntity clk rst stb_i cyc_i dat_i_1   = withClockResetEnable clk rst enableGen $
  let
   -- stb_i' = debounce stb_i 
   -- cyc_i' = debounce cyc_i 
   --  dat_i' = debounce dat_i
    functions_add = Add.Functions  TopEntity.first TopEntity.second TopEntity.sum
    input_add = Add.Input  dat_i_1 (S.dout output_sq) (C.dout output_cube) cyc_i stb_i
    initialstate_add = Add.InitialState False 0 0 0 False False
    output_add = Add.add functions_add initialstate_add input_add

    functions_cube = Cube.Functions TopEntity.cube
    input_cube = Cube.Input (A.dat_o_2 output_add) (A.enb2 output_add)
    initialstate_cube = Cube.InitialState (False,0)
    output_cube = Cube.cube functions_cube initialstate_cube input_cube

    functions_sq = Sqroot.Functions TopEntity.sqroot
    input_sq = Sqroot.Input (A.dat_o_1 output_add) (A.enb1 output_add)
    initialstate_sq = Sqroot.InitialState (False,0)
    output_sq = Sqroot.sqroot functions_sq initialstate_sq input_sq
  in bundle ((A.ack_o output_add), (A.dat_o output_add))
   -- bundle (ack_o', odd')
   -- (d0, d1, d2, d3) =  unbundle (Co.dsp output )
   -- in
   -- bundle (d0, d1, d2, d3, beeper  (Co.beep output))

{-# NOINLINE topEntity #-}
-- Here you have to define the functions
-----------------------------------------------------------------------------
-- | FSM state for the square root computation.
cube :: Width -> (Bool,Width)
cube x = (True, x*x*x)
zero::Width
zero  = 0
-- implementing square root using newton rapson methodintSqrt :: Unsigned 32 -> Unsigned 16
intSqrt n = go n 0 0 16
  where
    go :: Unsigned 32 -> Unsigned 32 -> Unsigned 16 -> Int -> Unsigned 32
    go remainder root bitPosition 0 =  root  -- Return the result after all bits are processed
    go remainder root bitPosition count =
      let
        -- Calculate the next bit to test
        bit = 1 `shiftL` (2 * (count - 1))

        -- Try to subtract (root << 1 + 1) * bit from the remainder
        trial = (root `shiftL` 1 + 1) * bit
        remainder' = if remainder >= trial
                     then remainder - trial
                     else remainder

        -- Update root if subtraction was successful
        root' = if remainder >= trial
                then (root `shiftL` 1) + 1
                else root `shiftL` 1
      in
        go remainder' root' bitPosition (count - 1)

sqroot :: Width -> (Bool, Width)
sqroot n = (True, m)
  where m = intSqrt n
--sqroot x = x*x
sum :: Width -> Width -> Width
sum a b = a + b
first  = fst
second = snd
