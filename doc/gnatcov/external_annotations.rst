.. _ext_annot:

###########################
External source annotations
###########################

In case modifying the sources is not possible or desirable, it is possible to
generate annotations in separate files, which can be passed to |gcvins| or
|gcvcov| to modify the behavior of the instrumenter or of the coverage
analysis.

Annotations loaded from an external file can be used along in-source
annotations, however in case of conflicts, the annotations defined in the
sources will always be prioritized.

External annotations are stored in TOML files, which can be manipulated through
three |gcv| commands, |gcvaddan|, |gcvdelan| and |gcvshoan|, to respectively
add a new annotations, delete an existing annotation from the files, or show
the annotations that are stored in the files.

Once generated, annotation files should be passed to the |gcvins| or |gcvcov|
commands with the :cmd-option:`--external-annotations` switch for them to be
taken into account by |gcv|.

.. _gen_ext:

Generating external annotations
###############################

The |gcvaddan| command can be used to create an annotation, tied to a specific
source location.

The help section for the |gcvaddan| command can be displayed by running
``gnatcov add-annotation --help``. Its synopsis is::

    gnatcov add-annotation --kind=KIND [--external-annotations=FILENAME] --output=OUTPUT_FILENAME [OPTIONS] FILENAME

Some notable command line options are:

:cmd-option:`--output`
    Name of the file to the newly created annotation will be written. If there
    already is a file, it will be overwritten.

:cmd-option:`--external-annotations`, |rarg|
    Loads pre-existing annotations from `FILENAME`. They are used to check that
    the new annotation does not conflict with any pre-existing one. The loaded
    annotations are all written to the output file specified through
    :cmd-option:`--output`.

:cmd-option:`FILENAME`, positional
    Filename to which the new annotation should apply. There are special
    considerations to keep in mind when specifying the name of the file to be
    annotated, see :ref:`ext_annot_relocs`

:cmd-option:`--annotation-id=IDENTIFIER`, optional
    Unique identifier for the new annotation. If not specified, |gcv| will
    generate one based on the kind of annotation and the designated location.

    This identifier must be unique across all external annotation files passed to
    any |gcv| invocation, and is used in diagnostics, or in the other annotation
    manipulation commands, |gcvdelan| and |gcvshoan| to uniquely designate an
    annotation.

:cmd-option:`--force`, optional
    Force overwriting of a pre-existing annotation for the same location, or
    with the same identifier. If not specified, gnatcov will emit an error and
    abort the annotation generation. The output file will not be modified.

The required command line switches depend on the value of the
:cmd-option:`--kind`, conveying the kind annotation to be generated, which
correspond to the annotations kinds supported in
``pragma Annotate (Xcov, KIND, ..)``. The required switches are detailed in the
help text for the |gcvaddan| command, and are detailed bellow. A switch in
brackets signifies that the switch is optional, otherwise the switch is required
and |gcvaddan| will emit an error if not found on the command line.

* :cmd-option:`--kind=Exempt_On`
    Generate an annotation symbolizing the beginning of an
    :ref:`exempted region <exemptions>`.

    :cmd-option:`--location=LINE:COL`
        Source location for the beginning of the exempted region.

    :cmd-option:`--justification=MESSAGE`
        Justification message to be displayed in the coverage reports for the
        exempted region.

*  :cmd-option:`--kind=Exempt_Off`
    Generate an annotation symbolizing the end of an exempted region.

    :cmd-option:`--location=LINE:COL`
        Source location for the end of the exempted region.

* :cmd-option:`--kind=Exempt_Region`
    Generate an annotation symbolizing an entire exempted region.

    :cmd-option:`--start-location=LINE:COL`
        Source location for the beginning of the exempted region.

    :cmd-option:`--end-location=LINE:COL`
        Source location for the end of the exempted region.

    :cmd-option:`--justification=MESSAGE`
        Justification message to be displayed in the coverage reports for the
        exempted region.

* :cmd-option:`--kind=Cov_Off`
    Generate an annotation symbolizing the beginning of a :ref:`disabled
    coverage region <disable_cov>`.

    :cmd-option:`--location=LINE:COL`
        Source location for the beginning of the disabled coverage region.

    :cmd-option:`--justification=MESSAGE`
        Justification message for the disabled coverage region, to be displayed
        in the coverage reports.

* :cmd-option:`--kind=Cov_On`
    Generate an annotation symbolizing the end of a disabled coverage region.

    :cmd-option:`--location=LINE:COL`
        Location for the end of the disabled coverage region.

* :cmd-option:`--kind=Dump_Buffers`
    Generate an annotation instructing |gcv| to insert a
    :ref:`buffer dump procedure call <manual_dump>` at the specified location.
    This is only taken into account when the selected dump trigger is
    ``manual``, see :ref:`Dump_Triggers` for more information concerning the
    dump triggers.

    :cmd-option:`--location=LINE:COL`
        Source location at which the buffer dump procedure call should be
        inserted.

    :cmd-option:`[--dump-filename-prefix=TEXT]`
        Optional trace filename prefix to be passed to the buffer dump procedure
        call. This will be textually passed as argument to the buffer dump, and
        must be an expression evaluating to a null-terminated ``char *``. As
        such, if the prefix to be used is a literal string, the argument passed
        to ``--dump-filename-prefix`` must contain quotes
        (e.g. ``--dump-filename-prefix='"my_trace"'``).

    :cmd-option:`[--annotate-after]`
        If specified, instruct |gcv| to insert the buffer dump procedure
        **after** the statement designated by the annotation. See
        :ref:`buf_semantics` for more details on the meaning of this option.

* :cmd-option:`--kind=Reset_Buffers`
    Generate an annotation instructing gnatcov to insert a :ref:`coverage buffer
    reset procedure call <buff_reset>` at the specified location. This is only
    taken into account when the selected dump trigger is ``manual``, see
    :ref:`Dump_Triggers` for more information concerning the dump triggers.

    :cmd-option:`--location=LINE:COL`
        Location at which the buffer reset procedure call should be inserted.

    :cmd-option:`[--annotate-after]`
        If specified, instruct |gcv| to insert the buffer reset procedure
        **after** the statement designated by the annotation. See
        :ref:`buf_semantics` for more details on the meaning of this option.

.. _buf_semantics:

Semantics of buffer manipulation annotations
--------------------------------------------

Due to the differences in instrumentation technology used by |gcv| for C/C++ and
Ada, the external annotations concerning buffer dump/reset have different
semantics that need to be taken into account when first annotation sources.

For C and C++ sources, |gcv| will insert the buffer dump/reset call at the exact
location designated by the annotation, without validating if the resulting code
is legal. It is thus recommended to choose a location corresponding to a
whitespace character, immediately before or after a statement.

For instance, starting from the following source file:

.. code-block:: C
    :linenos:

    int main(){
      // Execute the core program
      do_stuff();

      // Cleanup temp files
      cleanup();
    }

Creating an annotation as follows::

    gnatcov add-annotation --kind=Dump_Buffers -o annotations.toml --location=6:3 main.c

would result in the following invalid code to be generated:

.. code-block:: C
    :linenos:
    :emphasize-lines: 6

    int main(){
      //Execute the core program
      do_stuff();

      // Cleanup temp files
      cgnatcov_dump_buffers();leanup();
    }

Instead, it is better to target any whitespace character before the statement,
as in ``--location=6:2``.

For Ada sources, |gcv| will locate the inner-most statement list that encloses
the designated location, and insert the procedure call immediately **before**
this statement by default. The ``--annotate-after`` switch can be used to
instruct gnatcov to instead insert the procedure call **after** the designated
statement. This in particular is necessary to add a buffer dump annotation after
the last statement of a list.

If gnatcov cannot locate a statement list enclosing the designated location, a
warning will be emitted and the annotations will be ignored.

For instance, starting from the following source file:

.. code-block:: Ada
    :linenos:

    procedure Main is
    begin
       --  Run the actual program

       Do_Processing;

       --  Cleanup temp files

       Do_Cleanup;
    end Main;

Generating an annotation with::

    gnatcov add-annotation --kind=Dump_Buffers -o annotations.toml --location=9:15 main.adb

results in the following source, despite the source location pointing at the end
of the Do_Cleanup procedure call:

.. code-block:: Ada
    :linenos:
    :emphasize-lines: 9

    procedure Main is
    begin
       --  Run the actual program

       Do_Processing;

       --  Cleanup temp files

       GNATCov_RTS_Dump_Buffers; Do_Cleanup;
    end Main;

To ensure the buffer dump procedure is inserted after the Do_Cleanup call, it is
necessary to pass the ``--annotate-after`` command line switch.


.. _ext_annot_relocs:

File relocation considerations
------------------------------

The external file annotation mechanism stores the filename passed to the
|gcvaddan| command in the generated annotation file. When the annotations are
loaded by a |gcvins| or |gcvcov| command invocation, to determine if an
annotation is relevant for any of the processed files, |gcv| checks whether the
full filename of the file being processed ends with the annotation target
filename. It is thus important to only store in the annotation the part of the
filename that will not change between the different |gcv| command invocations.

This means that relative paths components (e.g. ``./`` or ``../``), and absolute
paths are likely to not be properly recognized.

The |gcvaddan| command accepts a ``--source-root=PREFIX`` option that will strip
``PREFIX`` from the target filename when generating the annotations. As such, it
is possible to generate an annotation for a file located in a parent directory,
while ensuring the generated annotation will correctly be taken into account in
subsequent |gcv| invocations with the following command line::

    gnatcov add-annotation [OPTIONS] --source-root="../" ../src/file.adb

|gcv| can also automatically deduce the appropriate prefix to be stripped from
the filename if a project file is passed to |gcvaddan| with the ``-P`` option.
Note that this only works if the file is unique in the project tree, or if the
file is located in a sub-directory of its project root directory.


Deleting a pre-existing annotation
##################################

The |gcvdelan| command can be used to remove a pre-existing annotation from an
external annotation file.

The help section for the |gcvaddan| command can be displayed by running
``gnatcov delete-annotation --help``. Its synopsis is::

    gnatcov delete-annotation --external-annotations=FILENAME --output=OUTPUT_FILENAME --annotation-id=IDENTIFIER

The semantics of each command line switch is:

:cmd-option:`--annotation-id=IDENTIFIER`:
    Unique IDENTIFIER of the annotation to be deleted.``

:cmd-option:`--external-annotations=FILENAME`, |rarg|:
    External annotation files from which the annotation will be loaded.
    If multiple files are passed to |gcv|, the annotations will be consolidated
    together and all written to the output file.

:cmd-option:`--output=OUTPUT_FILENAME`:
    Name of the file where the annotations will be written back after deletion
    of the designated annotation. This will overwrite any pre-existing file with
    the same OUTPUT_FILENAME.

Displaying the annotations contained in annotation files
########################################################

The command |gcvshoan| can be used to display the annotations contained in
annotation files in a more user-friendly manner.

The help section for the |gcvaddan| command can be displayed by running
``gnatcov show-annotations --help``. Its synopsis is::

    gnatcov show-annotations --external-annotations=FILENAME [--kind=KIND] [-P PROJECT] [FILENAMES]

The semantics of the command line switches are as follow:

:cmd-option:`--external-annotations=FILENAME`, |rarg|:
    External annotation files from which annotations will be loaded

:cmd-option:`--kind=KIND`, optional:
    Only display the annotations of kind KIND.

:cmd-option:`-P PROJECT`, optional:
    Show all annotations applicable to all source files of the project tree
    rooted at PROJECT.

:cmd-option:`FILENAMES`, positional:
    Only show the annotations applicable to the listed files.

Either the ``-P`` command line option or positional filenames must be specified.

The output format is as follows:

.. code-block::

    BASENAME_1:
    - START_LOCATION - END_LOCATION; id: IDENTIFIER; kind: KIND; [EXTRA_FIELDS]
    - ...

    BASENAME_2:
    - ...

``BASENAME_i`` corresponds to the basename of each file for which there is an
annotation. The each annotation is displayed on each line, starting by the
location range for the annotation. If the annotation only concerns a single
location, the ``END_LOCATION`` field will be identical to the
``START_LOCATION``. The unique identifier of the annotation is then displayed in
place of ``IDENTIFIER``, and the annotation kind is displayed in place of
``KIND``. The ``EXTRA_FIELDS`` concerns options specific to each annotation
kind, and are displayed as a semi-column separated list. See :ref:`gen_ext` for
more details on the extra fields that each annotation kind supports.

Annotation stability through file modifications
###############################################

The external annotations generated by the |gcvaddan| command embed varying
levels of information so that the source location designated on the command
line option can be remapped when possible, or invalidated otherwise.

This depends mainly on the language of the file to be annotated:

- For Ada sources, the annotation is tied to the inner-most enclosing named
  construct, such as a subprogram or a package. If the file is modified outside
  of that construct the annotation will be remapped properly. If the enclosing
  construct is modified, the annotation will be invalidated.

- For Cor C++ sources, the annotations are tied to the inner-most enclosing
  named declaration, such as a function declaration for C, or any of a
  namespace declaration, a class declaration or function/method declaration for
  C++.

Note that in both cases, if no enclosing named construct can be found, the
|gcvaddan| command will emit a warning and fall back to an absolute annotation,
which is invalidated as soon as the file is modified.

If an annotation is invalidated gnatcov will emit a warning stating that the
annotation was ignored, along with its unique identifier.

The output of the |gcvshoan| command will also display stale annotations, the
format for those annotations will be:

.. code-block::

    - STALE ANNOTATION; id: IDENTIFIER; kind: KIND; [EXTRA_FIELDS]; diagnostic: DIAGNOSTIC

where ``DIAGNOSTIC`` will contain a short explanation of why the entry is stale.

To fix this, simply replace the entry with an invocation of |gcvaddan|,
specifying the annotation identifier to be replaced, and forcing the
replacement::

    gnatcov add-annotation --annotation-id=IDENTIFIER --force [OPTIONS]
