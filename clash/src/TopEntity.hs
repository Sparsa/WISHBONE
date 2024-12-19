-----------------------------------------------------------------------------
-- |
-- Module      :  TopEntity
-- Maintainer  :  Sparsa Roychowdhury (sparsa.roychowdhury@tu-clausthal.de, Felix Klein (klein@react.uni-saarland.de)
--
-- Kitchen Timer Control
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds, LambdaCase, MultiWayIf, RecordWildCards, CPP #-}

-----------------------------------------------------------------------------

module TopEntity where

-----------------------------------------------------------------------------

import Clash.Prelude
import Control (Input(..), Functions(..), InitialState (..), control)
import qualified Control as Co (Output (..))


-----------------------------------------------------------------------------

{-# ANN topEntity (
  Synthesize
    { t_name    = "wishbone_reciever"
    , t_inputs  =
        [ PortName "CLOCK"
        , PortName "RESET"
        , PortName "STB_I"
        , PortName "CYC_I"
        , PortName "DAT_I"
        ]
    , t_output =
        PortProduct ""
          [ 
           PortName "ACKv"
          ,PortName "ODD"
          ]
    }
  )
  #-}

-----------------------------------------------------------------------------

--type Digit = Unsigned 7

-----------------------------------------------------------------------------
type Width = BitVector 64
--------------------------------

topEntity
  :: Clock System
  -> Reset System
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System Width
  -> Signal System (Bool, Bool)

topEntity clk rst stb_i cyc_i dat_i  = withClockResetEnable clk rst enableGen $
  let
   -- stb_i' = debounce stb_i 
   -- cyc_i' = debounce cyc_i 
   --  dat_i' = debounce dat_i
    functions = Functions  TopEntity.checkOdd 
    input = Input dat_i cyc_i  stb_i
    initialstate = InitialState False False
 
    output = control
      functions
      initialstate
      input
  in bundle ((Co.ack_o output),(Co.odd output))
   -- bundle (ack_o', odd')
   -- (d0, d1, d2, d3) =  unbundle (Co.dsp output )
   -- in
   -- bundle (d0, d1, d2, d3, beeper  (Co.beep output))
{-# NOINLINE topEntity #-}
-- Here you have to define the functions
-----------------------------------------------------------------------------
checkOdd ::  Width  -> Bool
checkOdd dat_i = (bitToBool . lsb) dat_i
