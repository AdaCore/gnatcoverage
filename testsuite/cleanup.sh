# run this to remove most temp artifacts created
# by the testsuite execution or ongoing development.

rm -rf $(find -type d -name 'isyswspace')
rm -rf $(find -type d -name 'tmp_*')
rm -rf $(find -type d -name 's_*')
rm -rf $(find -type d -name 'd_*')
rm -rf $(find -type d -name 'm_*')
rm -rf $(find -type d -name 'u_*')
rm -rf $(find -type d -name '[0-9]')
rm -rf $(find -type d -name 'wd_*')

rm -rf $(find -type d -name 'obj')
rm -rf $(find -type d -name 'obj_*')

rm -rf $(find -type f -name 'tmp*.list')

rm -rf $(find -type f -name '*~')
rm -rf $(find -type f -name 'test.py.???')
rm -rf $(find -type f -name '*.adb.*')
rm -rf $(find -type f -name '*.dump')

rm -rf $(find -type d -name 'memcheck.log')
rm -rf $(find -type d -name 'callgrind-*.log')
