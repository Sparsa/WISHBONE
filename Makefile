-include build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: hs
	cd clash && mkdir $BUILDDIR && $(MAKE)

clash/src/%.hs: %.cfm
	@${CFM2CODE} --clash  $< -o $@
%.cfm: %.tlsf
	@echo "Starting LTL Synthesis"
	@if ${SYNTH} $<  $@ 0 ; then \
	echo ""; echo "-> REALIZABLE"; echo ""; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* \([0-9]*\) [0-9]* [0-9]*/\1 latches/'; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* [0-9]* [0-9]* \([0-9]*\)/\1 gates/'; else echo ""; echo "UNREALIZABLE"; fi
%.tlsf: %.tsl
	@echo "Creating $@"
	@${TSL2TLSF} $< > $@

tlsf:   $(TLSFFILES)
cfm:  $(CFMFILES)
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
	cd clash && $(MAKE)  clean
	##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

.PHONY: clean run
.SILENT:
