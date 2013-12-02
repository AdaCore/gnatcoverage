###########################################################
# Kind of HOST OS we're on and common dependent variables #
###########################################################

HOST_UNAME=$(shell uname -s)

ifneq (,$(filter MINGW% CYGW%, $(HOST_UNAME)))
HOST_OS=windows
LN_S=$(CP)
exeext=.exe
endif
ifneq (,$(filter Linux, $(HOST_UNAME)))
HOST_OS=linux
LN_S=ln -sf
exeext=
endif

RM=rm -f
CP=cp -pf
MKDIR=mkdir -p
