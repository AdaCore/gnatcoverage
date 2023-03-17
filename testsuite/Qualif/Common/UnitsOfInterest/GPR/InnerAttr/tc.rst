**Check unit selection attributes in a non-root project**

From a test dir where we have

   src/ops/ops.ad?
           ops-andthen.ad?
           ops-orelse.ad?
      /test_*.adb

Verify that the proper set of units is examined for
a variety of subtests for which we

- Create an ops.gpr project with attributes to include
  or exclude some of the units in src/ops,

- Launch a testcase which will create it's own working dir and
  it's own root project there, for which we setup an empty set of
  units of interest.

Check the selection of units in ops.gpr with a lone Units attribute, a
lone Excluded_Units attribute or a mix of the two.
