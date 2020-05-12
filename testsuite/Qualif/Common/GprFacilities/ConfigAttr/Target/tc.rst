**Check gnatcov run -P on a project file with a Target attribute**

Verify that when provided a root project file featuring a Target
attribute, "gnatcov run" does not need a :option:`--target` argument.

Also verify that a :option:`--target` argument takes precedence over
the Target attribute.
