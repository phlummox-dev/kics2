########################################################################
# Makefile for ID compiler improved
########################################################################

.PHONY: all
all: idc
	chmod -R go+rX .

# generate saved state for Curry->FLVM compiler:
idc: FlatCurry2Types.curry Names.curry AbstractHaskell.curry \
	         AbstractHaskellGoodies.curry AbstractHaskellPrinter.curry
	pakcs -m 'main' -s FlatCurry2Types && mv FlatCurry2Types.state idc

.PHONY: clean
clean: 
	cleancurry
	rm -f idc
