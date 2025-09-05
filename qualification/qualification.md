# Producing a qualification kit for GNATcoverage


This documents describes how to produce the various documents that
comprise the DO-178C qualification kit for GNATcoverage, and provide
some details as to how each document is generated and about the scripts
used to generate them.

## Prerequisites

All documents require some common configuration that we pull from the
<adacore-gitlab-remote>/eng/libadalang/gnatcheck-qualkit repository.
To set things up correctly clone the repository somewhere (later referenced
as <gnatcheck-qualkit-clone>) and copy / make a symbolic link as follows

```console
(gnatcov/qualification)$ ln -s <gnatcheck-qualkit-clone>/doc/_common gnatcheck_qualkit_common
```

or

```console
(gnatcov/qualification)$ cp -r <gnatcheck-qualkit-clone>/doc/_common gnatcheck_qualkit_common
```

## General components and structure of a qualkit

### Qualification kit contents

A qualification kit (qualkit) is comprised of three main documents:
- The TORs, which stands for tool operational requirements, which describes
  the coverage results gnatcov is expected to generate for the relevant Ada
  language constructs, and how it is supposed to behave depending on the
  various command line arguments that can be used in certification projects;

- The STR, which stands for Software Test Results, which mostly contains the
  results from a qualification testsuite run (qualrun), along with statistics
  on the kind and numbers of verified coverage obligations in the tests;

- The PLANs document, which describe all things procedure related. This
  includes the tools developments practices, the tool testing procedure, the
  qualkit verification procedure, and the tool interface users must adhere to
  when using it in a certification project.

### Source material for qualkit production

To produce a qualkit we thus need:
- The sources for the various documents which are
  * In the `qualification/qualkit/plans/` directory for the PLANS;
  * In the testsuite all the `testsuite/Qualif/**/content.rst` for the TORs;
  * Mostly generated for the STR, but some static bits are placed in 
    `testsuite/STR/`
- The testsuite tree after execution of the testsuite in qualification mode.
  It will contain, in addition to the usual artifacts, some `.dump` files
  containing some information about the test context and the amount of tested
  obligations.

The TOR production is a bit delicate as the source `content.rst` are not valid
ReST documents, but instead contain some custom directives that need to be
expanded by the `qualkit/tor/genrest.py` script. This script has a two pass
approach to compute the full view of the testcases and requirements:
- A top-down traversal of the testsuite to determine the hierarchy between
  documents;
- A bottom-up approach to compute for a given test-case, which are the sources
  that are actually part of the testcase.

Once we have an in-memory representation of the various test-cases &
requirements, along with the sources that go with them, the contents of each
file are processed to replace the "importers" (custom directives) to generate
tables of links to the children and artifacts.

The importers would gain to be inlined, we can't completely get rid of them yet
as we still need to compute the actual set of sources that belong to a testcase.


## Producing a qualification kit with anod

The simplest way to generate a qualkit is to use the anod specs that will run
the qualification testsuite and produce the documents.
To do so, from a **linux** wavefront sandbox run

```console
(wave)$ anod build gnatcov-qualkit
```

There are multiple qualifiers available to customize the qualification
parameters, a description of which is available through `anod help
gnatcov-qualkit`.

One important caveat is that with this mechanism it is not possible to create
a qualkit for a Windows hosted testsuite run, unless it is scheduled in a
nightly run through the plans. This is because the production of the pdf
documents depend on texlive, which is not available on Windows hosts, the
`gnatcov-qualkit` spec thus expects the artifacts from the testsuite run to
be available through a cathod component.

## Producing a kit from source

To produce a qualkit from source, the first step is to obtain the artifacts
form a qualrun, either through anod, or by running the testsuite manually.

### Producing a qualrun

#### Through anod:

From a wavefront sandbox simply run:

```
(wave)$ anod test gnatcov-qualrun
```

The qualrun artifacts will be available under
`<wave>/<target>/gnatcov-qualrun-<qualifier-repr>/artifacts/`.

#### Running the testsuite manually:

The requirements to run a qualification testsuite are the same as those for a
"regular" testsuite run, so the environment produced by `gnatcov-dev.anod` is suitable.

```console
(gnatcov/testsuite)$ ./testsuite.py --trace-mode=src --qualif-level=doA --cargs:ada="-gnatec=native-doA.adc -gnat12"
```

The artifacts are in the `testsuite` directory.

### The genbundle script

The main entrypoint to produce a qualkit is the `qualification/genbundle.py`
script. It will set the required environment to produce a qualification kit,
generate the sources for the TOR and STR documents, invoke the sphinx
invocations to produce the pdf or html documents, and zip everything together.

A typical invocation for this script will look like:

```
(gnatcov/qualification)$ python genbundle.py --docformat=html,pdf --work-dir=$HOME/qualkit-dev/ \
--dolevel=doA --languages=Ada2012 --testsuite-dir=$HOME/repos/gnatcoverage/testsuite \
--rsync-from $HOME/repos/gnatcoverage --trace-mode=src
```

This invocation will produce kits in both HTML and pdf formats, in the
`$HOME/qualkit-dev/` directory, for a `doA` qualification level, targeting
`Ada2012`, assuming source traces using the testsuite artifacts from
`$HOME/repos/gnatcoverage/testsuite` and the qualification sources & scripts
from `$HOME/repos/gnatcoverage`.

Parameters may of course be tweaked, see the help of `genbundle.py` for more
information.

It is also possible not to produce a whole kit but only a single document using
the `--parts` option.

### Producing the documents individually

When working on the qualification source directly it may be faster to build the
documents directly. The only requirement is to install `doctools-e3` from a
wave sandbox and set its environment. Beware that the setenv primitive for this
spec will modify the locale, which will most certainly break your shell. You
can instead print the environment variables that would be set, and only export
`PATH` and `LD_LIBRARY_PATH`.

For the TOR document it is necessary to add `<gnatcov_repo>/qualification/qualkit`
to the `PYTHONPATH`. This is done by `genbundle.py` so it not necessary when
using this wrapper.

#### Producing the PLANS

The plans are a "simple" ReST document, in order to build either the html or
pdf document go to `qualification/plans` and run:

```console
(qualification/qualkit/plans)$ make [html|pdf]
```

### Producing the TORs

To generate the TORs, the first step is to gather the required sources, using
the `qualification/tor/genrest.py` script:

```console
(qualification/qualkit/tor)$ python genrest.py --dolevel=doA
```

You can then build the ReST document via make (this takes a while and generates
lots of warnings, it is expected):

```console
(qualification/qualkit/tor)$ make [html|pdf]
```

### Producing the STR

Similarly, the STR first needs to go through a source generations gathering
phase:

```console
(testsuite/STR)$ python genrest.py --testsuite-dir=../ --dolevel=doA
```

The generated sources need to be copied into the `qualification/qualkit/str`
directory:

```console
(qualification/qualkit/str)$ cp -r ../../../testsuite/STR/source .
```

then followed by

```console
(qualification/qualkit/str)$ make [html|pdf]
```
