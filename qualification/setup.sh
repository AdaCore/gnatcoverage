# Very rough script that collects the steps necessary to prepare an
# environment capable of producing the GNATcoverage qualification package
# (with genbundle.py).

# This script usually doesn't work straight out (for example the format in
# which some of the preriquisites are archived has changed, or today's build
# of one component has failed). It provides, nevertheless, a precise idea of
# the required components and a possible way to set them up for the task.

# The general idea is to setup a temporary work directory where we
# will have
#
# - a "PACKAGES" subdirectory, where the preriquisite components are fetched
#   (binary distributions pre-packaged by gnatmail somewhere)
#
# - a "local" subdirectory, where they are all installed and that should be
#   prepended to PATH to produce a qualification bundle.

TMP=$HOME/tmp-gnatcov
mkdir $TMP || exit 1

# ------------------------------
# -- fetch & install packages --
# ------------------------------

# fetch

GNATMAIL=tolbiac:/tolbiac.a/gnatmail
FTP=tolbiac:/ftpserver/art/file-view
DAILYBINDIR=$GNATMAIL/gcc-45/binaries/today/
OS=$(uname -o)

case $OS in
  *win*)
	OS=windows
	HOST_GNATEMU=rostov
	HOST_GNATCOV=rostov
	HOST_QMACHINE=tormes
	UNPACK="unzip -q"
	PACKEXT="zip"
	;;

  *Linux*)
	OS=linux
	HOST_GNATEMU=chinon
	HOST_GNATCOV=chinon
	HOST_QMACHINE=chinon
	UNPACK="tar xzf"
	PACKEXT="tar.gz"
	;;

esac

PACKDIR=$TMP/PACKAGES
mkdir $PACKDIR || exit 1

cd $PACKDIR

rsync $DAILYBINDIR/ppc-elf-$OS/${HOST_GNATEMU}/gnatemulator-\*.$PACKEXT .
rsync $DAILYBINDIR/x86-$OS/${HOST_GNATCOV}/gnatcoverage-\*.$PACKEXT .
rsync $DAILYBINDIR/x86-$OS/${HOST_QMACHINE}/qmachine-\*.$PACKEXT .

rsync $GNATMAIL/svn-binaries/gnatpython/1.2/distrib/x86-$OS/gnatpython-\*.tgz .

rsync $FTP/ada-6.4.2/ppc-elf-$OS/gnat/gnatpro-\* .

# install

LOCAL=$TMP/local
mkdir $LOCAL || exit 1

$(echo $UNPACK) gnatemulator-*.$PACKEXT
cp -rp gnatemulator-*-bin/* $LOCAL

$(echo $UNPACK) gnatcoverage-*.$PACKEXT
cp -rp gnatcoverage-*-bin/* $LOCAL

$(echo $UNPACK) qmachine-*.$PACKEXT
cp -rp qmachine-*-bin/* $LOCAL

tar xzf gnatpython-*.tgz
cp -rp gnatpython-*-bin $LOCAL/gnatpython

case $OS in
    windows)
	./gnatpro-*.exe /S "/D=`cygpath -w $LOCAL`";;

    *)
	tar xzf gnatpro-*
	cd gnat-*bin
	./doinstall
esac

# ------------------------------------------------
# -- retrieve and run the material build script --
# ------------------------------------------------

PATH=$LOCAL/bin:$LOCAL/gnatpython:$LOCAL/gnatpython/Scripts:$PATH

cd $TMP

svn export svn://scm.forge.open-do.org/scmrepos/svn/couverture/trunk/couverture/qualification/genbundle.py

TEMP=$TMP
DATE=`date +%Y-%m-%d`
python ./genbundle.py --root-dir=$TMP/QMAT

