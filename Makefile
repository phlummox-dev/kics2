########################################################################
# Makefile for ID compiler
########################################################################

# The major version number:
MAJORVERSION=0
# The minor version number:
MINORVERSION=1
# The version date:
COMPILERDATE=15/11/11
# The Haskell installation info
INSTALLHS=runtime/Installation.hs
# The Curry installation info
INSTALLCURRY=src/Installation.curry
# Logfile for make:
MAKELOG=make.log
BOOTLOG=boot.log
# Directory where local executables are stored:
LOCALBIN=bin/.local

.PHONY: all
all:
	${MAKE} installwithlogging

# bootstrap the compiler using PAKCS
.PHONY: bootstrap
bootstrap: ${INSTALLCURRY}
	@rm -f ${BOOTLOG}
	@echo "Bootstrapping started at `date`" > ${BOOTLOG}
	cd src && ${MAKE} bootstrap 2>&1 | tee -a ../${BOOTLOG}
	@echo "Bootstrapping finished at `date`" >> ${BOOTLOG}
	@echo "Bootstrap process logged in file ${BOOTLOG}"

# install the complete system and log the installation process
.PHONY: installwithlogging
installwithlogging:
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	${MAKE} install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

# install the complete system if the kics2 compiler is present
.PHONY: install
install: ${INSTALLCURRY} installfrontend Compile REPL
	cd cpns  && ${MAKE} # Curry Port Name Server demon
	cd tools && ${MAKE} # various tools
	cd www   && ${MAKE} # scripts for dynamic web pages
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then cd docs/src && ${MAKE} install ; fi
	chmod -R go+rX .

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -r bin/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && ${MAKE} doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

# install the front end if necessary:
.PHONY: installfrontend
installfrontend:
	@if [ ! -d ${LOCALBIN} ] ; then mkdir ${LOCALBIN} ; fi
	# install mcc front end if sources are present:
	@if [ -f mccparser/Makefile ] ; then cd mccparser && ${MAKE} ; fi
	# install local front end if sources are present:
	@if [ -d frontend ] ; then ${MAKE} installlocalfrontend ; fi

# install local front end:
.PHONY: installlocalfrontend
installlocalfrontend:
	cd frontend/curry-base && cabal install
	cd frontend/curry-frontend && cabal install
	# copy cabal installation of front end into local directory
	@if [ -f ${HOME}/.cabal/bin/cymake ] ; then cp -p ${HOME}/.cabal/bin/cymake ${LOCALBIN} ; fi

.PHONY: Compile
Compile: ${INSTALLCURRY}
	cd src ; ${MAKE} CompileBoot

.PHONY: REPL
REPL: ${INSTALLCURRY}
	cd src ; ${MAKE} REPLBoot

# generate module with basic installation information:
${INSTALLCURRY}: ${INSTALLHS}
	cp ${INSTALLHS} ${INSTALLCURRY}

${INSTALLHS}: Makefile
	echo "-- This file is automatically generated, do not change it!" > ${INSTALLHS}
	echo "module Installation where" >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerName :: String' >> ${INSTALLHS}
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDir :: String' >> ${INSTALLHS}
	echo 'installDir = "'`pwd`'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'majorVersion :: Int' >> ${INSTALLHS}
	echo 'majorVersion = ${MAJORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'minorVersion :: Int' >> ${INSTALLHS}
	echo 'minorVersion = ${MINORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerDate :: String' >> ${INSTALLHS}
	echo 'compilerDate = "'${COMPILERDATE}'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDate :: String' >> ${INSTALLHS}
	echo 'installDate = "'`date`'"' >> ${INSTALLHS}

# install required cabal packages

.PHONY: installhaskell
installhaskell:
	cabal update
	cabal install network
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search
	cabal install mtl

.PHONY: clean
clean:
	rm -f *.log
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	cd src   ; ${MAKE} clean
	@if [ -d lib/.curry/kics2 ] ; then cd lib/.curry/kics2 && rm -f *.hi *.o ; fi
	cd cpns  ; ${MAKE} clean
	cd tools ; ${MAKE} clean
	cd www   ; ${MAKE} clean

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	bin/cleancurry -r
	rm -rf bin/idc ${LOCALBIN}


###############################################################################
# Create distribution versions of the complete system as tar file kics2.tar.gz:

# temporary directory to create distribution version
KICS2DIST=/tmp/kics2
# repository with new front-end:
FRONTENDREPO=http://www-ps.informatik.uni-kiel.de/kics2/repos

# generate a source distribution of KICS2:
.PHONY: dist
dist:
	rm -rf kics2.tar.gz ${KICS2DIST}       # remove old distribution
	git clone . ${KICS2DIST}               # create copy of git version
	# install front-end sources
	mkdir ${KICS2DIST}/frontend
	cd ${KICS2DIST}/frontend && git clone ${FRONTENDREPO}/curry-base.git
	cd ${KICS2DIST}/frontend && git clone ${FRONTENDREPO}/curry-frontend.git
	cd ${KICS2DIST} && ${MAKE} cleandist   # delete unnessary files
	cd bin && cp idc ${KICS2DIST}/bin      # copy bootstrap compiler
	cd ${KICS2DIST} && ${MAKE} Compile     # translate compiler
	cd ${KICS2DIST} && ${MAKE} REPL        # translate REPL
	cd ${KICS2DIST} && ${MAKE} clean       # clean object files
	# copy documentation:
	@if [ -f docs/Manual.pdf ] ; then cp docs/Manual.pdf ${KICS2DIST}/docs ; fi
	cd ${KICS2DIST}/bin && rm -rf .local idc idc.bak # clean execs
	sed -e "/distribution/,\$$d" < Makefile > ${KICS2DIST}/Makefile
	cd /tmp && tar cf kics2.tar kics2 && gzip kics2.tar
	mv /tmp/kics2.tar.gz .
	chmod 644 kics2.tar.gz
	rm -rf ${KICS2DIST}
	@echo "----------------------------------------------------------------"
	@echo "Distribution kics2.tar.gz generated."

#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -rf .git .gitignore bin/.gitignore
	rm -rf frontend/curry-base/.git frontend/curry-base/.gitignore
	rm -rf frontend/curry-frontend/.git frontend/curry-frontend/.gitignore
	rm -rf docs/src
	rm -rf benchmarks papers talks tests examples experiments
	rm -f TODO compilerdoc.wiki testsuite/TODO

# publish the distribution files in the local web pages
HTMLDIR=${HOME}/public_html/kics2/download
.PHONY: publish
publish:
	cp kics2.tar.gz INSTALL.html ${HTMLDIR}
	chmod -R go+rX ${HTMLDIR}
	@echo "Don't forget to run 'update-kics2' to make the update visible!"
