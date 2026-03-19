Tests in this dir test the instrumentation of various kinds of expression functions
depending on if:
1- they have a previous spec or not
2- they have aspects or not
3- they are self-referencing (or recursive) or not.

This lead to 8 tests. For each dir, if the expression function under test
is recursive the folder name contains "rec", if it does not it contains "no-rec";
if it has aspects, it contain "asp", if it does not it contains "no-asp";
if it has a previous spec it contains "prespec", if it does not, it contains "no-prespec".
