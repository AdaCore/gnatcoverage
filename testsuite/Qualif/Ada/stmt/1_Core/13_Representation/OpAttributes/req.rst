Attribute Definition Clauses for Operational Attributes
=======================================================

%REQ_ID%

Subprogram calls issued through operational attributes shall be
treated as regular subprogram calls, regarding both the call statement
itself and the called subprogram code.

.. rubric:: Testing Strategy

Check correctness of the tool behavior for a panel of calls issued through
operational attributes, with variations of the achieved coverage at both the
call sites and within the called suprograms. Note that the operational Stream
attributes are only available in environments where the runtime profile
features Ada.Streams.

.. qmlink:: TCIndexImporter

   *

