-include build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##

NAME = wishbone_reciever 

##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: clash/src/Control.hs  

${NAME}.tlsf: ${NAME}.tsl
	@echo "Creating $@"
	@${TSL2TLSF} $< > $@

${NAME}.cfm: ${NAME}.tlsf
	@echo "Starting LTL Synthesis"
	@${SYNTH} $< > $@
	@if [ `head -n 1 $@` == "REALIZABLE" ]; then echo ""; echo "-> REALIZABLE"; echo ""; sed -i '1d' $@; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* \([0-9]*\) [0-9]* [0-9]*/\1 latches/'; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* [0-9]* [0-9]* \([0-9]*\)/\1 gates/'; else echo ""; echo "UNREALIZABLE"; fi

clash/src/Control.hs: ${NAME}.cfm
	@${CFM2CODE} clash -m Control -f control -o $@ $<



##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	@rm -f ${NAME}.cfm
	@rm -f ${NAME}.tlsf
	@rm -f clash/src/Control.hs
	##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

.PHONY: clean run
.SILENT:
