**Check the command line precedence over project file attributes**

Using a single root project file where we specify a :option:`--level`
switch for all commands and a :option:`--annotate` switch for the
coverage command, with a test program resulting in stmt and decision
coverage violations on purpose, verify that we obtain the intended set
of violations in the intended output format when passing on the
command line

- Neither :option:`--level` nor :option:`--annotate`

- :option:`--level` and not :option:`--annotate`

- :option:`--annotate` and not :option:`--level`

- Both :option:`--annotate` and :option:`--level`
