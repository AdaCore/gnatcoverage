# Template Makefile for integrated instrumentation tests using Makefiles.

# Main build target
{build_target}: {build_target_deps}
	{linker} -o {build_target} {build_target_deps} {linker_switches}

%.o: %.c
	{cc} {compiler_switches} -c $< -o $@

%.o: %.cpp
	{cxx} {compiler_switches} -c $< -o $@

clean:
	rm -rf *.o {build_target}

