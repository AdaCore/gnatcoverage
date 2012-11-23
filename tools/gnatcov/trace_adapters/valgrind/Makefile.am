include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = docs/cov-manual.xml

#----------------------------------------------------------------------------
# coverage-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = coverage-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += coverage-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

NONE_SOURCES_COMMON = cov_main.c cov_traces.c

coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(NONE_SOURCES_COMMON)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(coverage_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(NONE_SOURCES_COMMON)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(coverage_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif


