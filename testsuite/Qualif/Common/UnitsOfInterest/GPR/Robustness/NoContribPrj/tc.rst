**Check warnings on projects not contributing to units of interest**

Exercise a variety of situations involving a mini project hierarchy.

Verify that the tool warns when a project is explicitly designated
as "of interest" with :option:`--projects`, yet eventually carries no
unit of interest to the analysis after an override with :option:`--units`.

Verify that the tool does not warn when a project in the scope carries
no unit of interest in other situations, such as when the project was
only brought in the scope implicitly by :option:`--recursive` or when
the project itself has explicit unit selection attributes.

Verify that the tool support globing patterns for unit specification,
in :option:`--units` or in project attributes.
