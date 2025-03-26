-include build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##
SOURCES = Add.tsl Sqroot.tsl Cube.tsl
TLSFFILES= $(patsubst %.tsl,%.tlsf,$(SOURCES))
HASKELLFILES= $(patsubst %.tsl,clash/src/%.hs,$(SOURCES))
CFMFILES= $(patsubst %.tsl,%.cfm,$(SOURCES))
LTLFILES = $(patsubst %.tsl,%.ltl,$(SOURCES))
JSONFILES = $(patsubst %.tsl,%.json,$(SOURCES))
##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: hs

%.tlsf: %.tsl
	@echo "Creating $@"
	@${TSL2TLSF} $< > $@
tlsf : $(TLSFFILES)
%.cfm: %.tlsf
	@echo "Starting LTL Synthesis"
	@${SYNTH} $<  $@ 0  | grep 'REALIZABLE' &> /dev/null 
	@if [ $? == 0 ]; then echo ""; echo "-> REALIZABLE"; echo ""; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* \([0-9]*\) [0-9]* [0-9]*/\1 latches/'; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* [0-9]* [0-9]* \([0-9]*\)/\1 gates/'; else echo ""; echo "UNREALIZABLE"; fi

clash/src/%.hs: %.cfm
	@${CFM2CODE} --clash  $< -o $@

cfm: $(CFMFILES)
hs : $(HASKELLFILES)

##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	@rm -f $(CFMFILES)
	@rm -f $(TLSFFILES)
	@rm -f $(LTLFILES)
	@rm -f $(JSONFILES)
	@rm -f $(HASKELLFILES)
	##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

.PHONY: clean run
.SILENT:
