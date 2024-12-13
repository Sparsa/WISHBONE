-include build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##


##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: clash/src/Control.hs  

wishbone_reciever.tlsf: wishbone_reciever.tsl
	@echo "Creating $@"
	@${TSL2TLSF} $< > $@

wishbone_reciever.cfm: wishbone_reciever.tlsf
	@echo "Starting LTL Synthesis"
	@${SYNTH} $< > $@
	@if [ `head -n 1 $@` == "REALIZABLE" ]; then echo ""; echo "-> REALIZABLE"; echo ""; sed -i '1d' $@; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* \([0-9]*\) [0-9]* [0-9]*/\1 latches/'; cat $@ | grep aag | sed 's/aag [0-9]* [0-9]* [0-9]* [0-9]* \([0-9]*\)/\1 gates/'; else echo ""; echo "UNREALIZABLE"; fi

clash/src/Control.hs: wishbone_reciever.cfm
	@${CFM2CODE} --clash -m Control -f control -o $@ $<



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
