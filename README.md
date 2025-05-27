# Wishbone BUS design using TSL synthesis
* TSL or Temporal Stream logic is used to specify the system you want to design. Then you can use synthesis tool to synthesize your specification. In general TSL synthsis is undecidable. But, you can abstract the TSL specification to LTL. And if the LTL can be synthesized, then the TSL can also be synthesized. If the LTL is unrealizable, then you might have to add refinement to the LTL abstraction. This may require multiple refinement iteration if it is realizable at all. The complete flow follows the following steps:
1. Writing specification using TSL, it can be in multiple files.
2. Convert the specification to LTL. We use the TSL toolkit to convert that TSL specification to LTL. 
3. Use synthesis tool to check if the synthesis is possible. We use Strix as synthesis tool.
4. The synthesis tool genrates CFM format, which is then converted to Haskell code using "cfm2code" tool. The output is a control code based on Clash HDL named Control.hs. But, this file uses abstract definition of functions used in computation.  So  you have to implement the implementation of the Datapath.

During this process there are a few issues that we had to resolve. The Haskell generation tool code2cfm generated code that are not supported by the latest version of clash. So we had to manually made some changes. The changes can be listed here for example:
1. The previous tool chain generates something called HiddenClockReset, which is non existant in Clash 1.8.1. We had to rename it to HiddenClockResetEnable 
2. Plus HiddenClockReset normally takes domain and two more parameters. But, HiddenClockResetEnable only takes domain as parameter (remove the rest of the parameters, only keep domain)
3. The next issue is to add 
 control
  :: HiddenClockResetEnable domain
  => ( NFDataX b)
  the NFDataX property for the output domain type. 
4. Sometime the genrator adds extra 'let' clause, where it is not needed. I had to remove an `let` from the generated code, 

controlCircuit
 :: HiddenClockResetEnable domain
 => ControlIn domain -> ControlOut domain

controlCircuit ControlIn{..} = 
    **let** ControlOut
      { controlOut0 = pure high
      , controlOut1 = pure low
      , controlOut2 = pure high
      , controlOut3 = pure low
      }
      Here the output is coming directly from the control. But an additional **let** was introduced by the flow which was creating an issue during the compilation. I removed the **let** from the code and then it worked.
       
* TSL is very interesting logic because, the it allows us to clearly distinguish between the Control and Data path. So, the Data paths are open to interpretation using functional abstraction. This allows us to reuse the control path for different situations where the data path are different but they share same control path. 
* 
