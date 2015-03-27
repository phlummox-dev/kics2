# Generate the kics2-runtime.cabal and install the package

# Name of the package
PACKAGE      = kics2-runtime
# Name of the cabal file
CABAL_FILE   = $(PACKAGE).cabal
# idsupply directory to use
IDSUPPLYDIR  = idsupplyioref
# Additional flags for compilation of the runtime
RUNTIMEFLAGS =

# the runtime package configuration file
RUNTIME_PKG_CONF = $(LOCALPKG)/runtime.conf

# replacement stuff
comma     := ,
empty     :=
space     := $(empty) $(empty)
# a b c -> a, b, c
comma_sep  = $(subst $(space),$(comma)$(space),$(1))
# runtime dependencies as comma separated list
CABAL_RUNTIMEDEPS  = $(call comma_sep,$(RUNTIMEDEPS))
# GHC options passed to the cabal file
CABAL_GHC_OPTS = $(RUNTIMEFLAGS:%=-D%)

.PHONY: install
install: $(CABAL_FILE)
	$(CABAL_INSTALL_GECODE) $(CABAL_PROFILE)

.PHONY: unregister
unregister:
	-$(GHC_UNREGISTER) $(PACKAGE)-$(VERSION)

.PHONY: clean
clean:
	rm -rf dist/
	rm -f *.hi *.hi-boot */*.hi *.o *.o-boot */*.o
	rm -f $(CABAL_FILE)
	cd Solver && rm -rf *.hi *.o

$(CABAL_FILE): ../Makefile Makefile
ifndef VERSION
	$(error VERSION is not defined. Please use 'make' on top-level)
endif
	echo "Name:           $(PACKAGE)"                        >  $@
	echo "Version:        $(VERSION)"                        >> $@
	echo "Description:    The runtime environment for KiCS2" >> $@
	echo "License:        OtherLicense"                      >> $@
	echo "Author:         The KiCS2 Team"                    >> $@
	echo "Maintainer:     kics2@curry-language.org"          >> $@
	echo "Build-Type:     Simple"                            >> $@
	echo "Cabal-Version:  >= 1.9.2"                          >> $@
	echo ""                                                  >> $@
	echo "Flag Gecode"                                       >> $@
	echo "  Description:  Include Gecode Solver for finite"  >> $@
	echo "                domain constraints. Requires a"    >> $@
	echo "                working Gecode 3.1 installation"   >> $@
	echo "  Default:      False"                             >> $@
	echo ""                                                  >> $@
	echo "Library"                                           >> $@
	echo "  Build-Depends: $(CABAL_RUNTIMEDEPS)"             >> $@
	echo "  Exposed-modules:"                                >> $@
	echo "      Basics, CurryException, Debug, FailInfo"     >> $@
	echo "    , FailTrace, IDSupply, Installation"           >> $@
	echo "    , MonadList, MonadSearch, PrimTypes, SafeExec" >> $@
	echo "    , Solver.Constraints"                          >> $@
	echo "  Other-modules:"                                  >> $@
	echo "      ConstStore, ID, Search"                      >> $@
	echo "    , Solver.EquationSolver"                       >> $@
	echo "    , Solver.ExternalSolver, Solver.MCPUtils"      >> $@
	echo "    , Solver.OvertonSolver, Solver.SatSolver"      >> $@
	echo "    , Solver.SolverControl, Strategies, Types"     >> $@
	echo "  hs-source-dirs: ., $(IDSUPPLYDIR)"               >> $@
	echo "  if flag(gecode)"                                 >> $@
	echo "      Build-Depends: monadiccp-gecode"             >> $@
	echo "      Other-modules: Solver.GecodeSolver"          >> $@
	echo "      CPP-Options:   -DGECODE"                     >> $@
	echo "  ghc-options: $(CABAL_GHC_OPTS)"                  >> $@
