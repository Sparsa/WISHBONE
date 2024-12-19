-include ../build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##

NAME = wishbone_reciever 
TOPENTITY = TopEntity

##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: ${BUILDDIR}/verilog/${NAME}.v 





${BUILDDIR}/verilog/${NAME}.v: src/${TOPENTITY}.hs src/Control.hs
	mkdir -p ${BUILDDIR}/clash
	${CABAL} run -- wishbone:clash ${CPP_FLAGS} \
    -package-env ./.ghc.environment.* \
    -outputdir ${BUILDDIR}/clash \
    --verilog \
    src/${TOPENTITY}.hs -v
	rm -Rf ${BUILDDIR}/verilog
	mv ${BUILDDIR}/clash/${TOPENTITY}.topEntity ${BUILDDIR}/verilog

src/Control.hs:
	$(MAKE) -C ..
	echo ""

##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	rm -Rf ${BUILDDIR}

##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

