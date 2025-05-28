# Wishbone BUS design using TSL synthesis
* TSL or Temporal Stream logic is used to specify the system you want to design. In general TSL synthsis is undecidable.
But, you can abstract the TSL specification to LTL. And if the LTL can be synthesized, then the TSL can also be synthesized. 
If the LTL is unrealizable, then you might have to add refinement to the LTL abstraction. This may require multiple refinement iteration. The complete flow follows the following steps:
1. Writing specification using TSL, it can be in multiple files. In this project We have three specification files under the top directory "Add.tsl, Cube.tsl, Sqroot.tsl"
2. Convert the specification to LTL. We use the TSL tool *tsl2tlsf* to convert that TSL specification to LTL. This tool is provided in the /bin directory 
3. Use synthesis tool to check if the synthesis is possible. We use Strix as synthesis tool. A version of Strix is provided in the /bin directory.
4. The synthesis tool genrates CFM format, which is then converted to Haskell code using *cfm2code* tool. The output is a control code based on Clash HDL named Control.hs. But, this file uses abstract definition of functions used in computation.  So  you have to implement the implementation of the Datapath. The implementation is provided under the /clash/src directory named "TopEntity.hs".

## The process is divided in two makefile
The first makefile present in the top directory. This takes the TSL files, abstracts them to LTL files and use Strix to generate the CFM code. Note that Strix does not directly consume *tlsf* format. So, a small script is written that uses the tool *syfco* to convert the *tlsf* to *ltl* file. To make this automatic, we have created a small script named *tlsf_strix_synth_cfm* that takes *tlsf* converts it to *ltl* and runs Strix. This script is also provided in the /bin directory. Once the synthesis is successfull, it calls the makefile under the *clash* directory. This uses the genrated Haskell code to generate the verilog code. The verilog code can be found under the directory "/clash/build/verilog".


During this process there are a few issues that we had to resolve. The Haskell generation tool *code2cfm* generated code that are not supported by the latest version of clash. So we had to manually made some changes. The changes can be listed here for example:
1. The previous tool chain generates something called HiddenClockReset, which is non existant in Clash 1.8.1. We had to rename it to HiddenClockResetEnable 
2. Plus HiddenClockReset normally takes domain and two more parameters. But, HiddenClockResetEnable only takes domain as parameter (remove the rest of the parameters, only keep domain)
3. The next issue is to add 
 control
  :: HiddenClockResetEnable domain
  => ( NFDataX b)
  the NFDataX property for the output domain type. 
4. Sometime the generator adds extra 'let' clause, where it is not needed. I had to remove an `let` from the generated code, 

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
5. For this project we have added a patch file in the *clash* directory. The makefile patches the generated Haskell files with this patch. *This is so far working with the latest clash version 1.8.2.* 

       
* TSL is very interesting logic because, the it allows us to clearly distinguish between the Control and Data path. So, the Data paths are open to interpretation using functional abstraction. This allows us to reuse the control path for different situations where the data path are different but they share same control path. 
* 
