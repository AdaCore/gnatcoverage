#! /bin/sh

# This is a developer convenience script to build and install gnatcov_rts_full
# in the toolchain's default prefix.

if [ $# -gt 0 ]; then
    PREFIX_ARG="--prefix=$1"
fi

# If gnatcov_rts_full was already installed, just uninstall it (one single
# gprbuild --uninstall is enough for all variants).
gprinstall --uninstall -Pgnatcov_rts_full

# Build and install it for all supported library types
for i in static static-pic relocatable
do
    gprbuild   -P gnatcov_rts_full.gpr -XLIBRARY_TYPE=$i -p
    gprinstall -P gnatcov_rts_full.gpr -XLIBRARY_TYPE=$i -f -p \
        --sources-subdir=include/gnatcov_rts_full \
        --build-var=LIBRARY_TYPE --build-name=$i \
        $PREFIX_ARG
done
