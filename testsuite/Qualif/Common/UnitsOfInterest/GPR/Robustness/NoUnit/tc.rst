**Check warnings on empty set of Units Of Interest**

Verify that the tool emits a warning when the set of Units Of Interest
conveyed by the user is empty. Check a variety of cases leading to such
a situation with a single project file:

- When the Coverage.Included_Units attribute explicitly defines
  an empty set;

- When the Coverage.Included_Units attribute lists a single unit
  which is not part of the project;

- When the Coverage.Excluded_Units attribute takes out everything
  that would have been of interest otherwise (without the exclusion).
