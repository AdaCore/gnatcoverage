# run this to remove most temp artifacts created
# by the testsuite execution or ongoing development.

rm -rf $(find -type d -name 'isyswspace')
rm -rf $(find -type d -name 'tmp_*')
rm -rf $(find -type d -name 'st_*')
rm -rf $(find -type d -name 'dc_*')
rm -rf $(find -type d -name 'mc_*')
rm -rf $(find -type d -name 'uc_*')
rm -rf $(find -type d -name '[0-9]')

rm -rf $(find -type d -name 'obj')
rm -rf $(find -type d -name 'obj_*')

rm -rf $(find -type f -name '*~')
rm -rf $(find -type f -name 'test.py.???' | grep -v svn)
rm -rf $(find -type f -name '*.adb.*' | grep -v svn)
rm -rf $(find -type f -name '*.dump' | grep -v svn)
