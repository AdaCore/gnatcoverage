# run this to remove most temp artifacts created
# by the testsuite execution or ongoing development.

rm -rf $(find -type d -name 'tmp_*')
rm -rf $(find -type d -name '[0-9]')
rm -rf $(find -type f -name 'test.py.???')
rm -rf $(find -type f -name '*~')


