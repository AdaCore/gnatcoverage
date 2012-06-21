# run this to remove most temp artifacts created
# by the testsuite execution or ongoing development.

LIST=""
LIST="$LIST $(find -type d -name 'tmp_*')"
LIST="$LIST $(find -type d -name '[0-9]')"
rm -rf $LIST

LIST=""
LIST="$LIST $(find -type d -name 'obj')"
rm -rf $LIST

LIST=""
LIST="$LIST $(find -type f -name 'test.py.???')"
LIST="$LIST $(find -type f -name '*~')"
LIST="$LIST $(find -type f -name '*.adb.*')"
rm -rf $(echo $LIST | sed -e 's/[^ ]*svn[^ ]*//g')



