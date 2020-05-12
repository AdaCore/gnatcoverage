**Check the ability to provide switches for different commands through project file attributes**

Verify that we can pass at least a :option:`--tag` option to gnatcov
run and a :option:`--annotate` option to gnatcov coverage through
:option:`Switches(<command-name>)` attributes, producing the intended
effect.

Verify that we get the intended effects of a :option:`--level` switch
through either a :option:`Switches('coverage')` attribute or a
:option:`Switches('*')` one.

Verify that a Switches attribute for a specific command always
overrides one specified with a '*' index, whether the command specific
one is stated before or after the '*' occurrence.

