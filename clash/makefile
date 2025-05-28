-include ../build.cfg

##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##

NAME = wishbone_reciever 
TOPENTITY = TopEntity
GHC_ENV := $(firstword $(wildcard ./.ghc.environment.*))

PATCHED_SOURCES = ${BUILDDIR}/.patched
##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: ${BUILDDIR}/verilog/${NAME}.v 


${PATCHED_SOURCES}: src.patch src/Add.hs src/Cube.hs src/Sqroot.hs
	@mkdir -p "${BUILDDIR}/src"
	@cp -r src/* "${BUILDDIR}/src/"
	@cp src.patch "${BUILDDIR}"
	@cd "${BUILDDIR}" && patch -p0 < src.patch
	@touch "$@"   # Mark patching as complete


${BUILDDIR}/verilog/${NAME}.v:  ${PATCHED_SOURCES}
	@mkdir -p ${BUILDDIR}/clash
	${CABAL} run  wishbone:clash -- \
	${CPP_FLAGS} \
    -package-env ${GHC_ENV} \
    -outputdir ${BUILDDIR}/clash \
    --verilog \
    "${BUILDDIR}/src/${TOPENTITY}.hs" -v
	rm -Rf ${BUILDDIR}/verilog
	mv ${BUILDDIR}/clash/${TOPENTITY}.topEntity ${BUILDDIR}/verilog


##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	rm -Rf ${BUILDDIR}

##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

