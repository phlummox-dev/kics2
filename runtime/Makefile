# Generate the kics2-runtime.cabal and install the package

# Name of the package
PACKAGE     = kics2-runtime
# Name of the cabal file
CABAL_FILE  = $(PACKAGE).cabal
# idsupply directory to use
IDSUPPLYDIR = idsupplyioref

.PHONY: install
install: $(CABAL_FILE)
	$(CABAL_INSTALL) -O2

.PHONY: unregister
unregister:
	-$(GHC-PKG) unregister $(PACKAGE)-$(VERSION)

.PHONY: clean
clean:
	rm -rf *.hi *.o
	rm -f $(CABAL_FILE)

$(CABAL_FILE): ../Makefile Makefile
ifndef VERSION
	$(error VERSION is not defined. Please use 'make' on top-level)
endif
	echo "Name:           $(PACKAGE)"                        >  $@
	echo "Version:        $(VERSION)"                        >> $@
	echo "Description:    The runtime environment for KiCS2" >> $@
	echo "License:        OtherLicense"                      >> $@
	echo "Author:         Fabian Reck"                       >> $@
	echo "Maintainer:     fre@informatik.uni-kiel.de"        >> $@
	echo "Build-Type:     Simple"                            >> $@
	echo "Cabal-Version:  >= 1.9.2"                          >> $@
	echo ""                                                  >> $@
	echo "Library"                                           >> $@
	echo "  Build-Depends:"                                  >> $@
	echo "      base, containers, ghc, monadiccp"            >> $@
	echo "    , mtl, parallel-tree-search, tree-monad"       >> $@
	echo "  Exposed-modules:"                                >> $@
	echo "      Basics, CurryException, Debug, FDData"       >> $@
	echo "    , IDSupply, Installation, MonadList"           >> $@
	echo "    , MonadSearch, PrimTypes, SafeExec"            >> $@
	echo "  Other-modules:"                                  >> $@
	echo "      ConstStore, ExternalSolver, ID, MCPSolver"   >> $@
	echo "    , Search, Solver, SolverControl, Strategies"   >> $@
	echo "    , Types"                                       >> $@
	echo "  hs-source-dirs: ., $(IDSUPPLYDIR)"               >> $@
