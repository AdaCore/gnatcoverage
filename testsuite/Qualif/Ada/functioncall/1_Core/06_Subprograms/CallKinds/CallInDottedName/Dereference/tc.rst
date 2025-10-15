**Check FCC for a call in the context of dereference**

Make a call to a subprogram returning an access type and dereference the
return value directly.

gnatcov limitation: gnatcov is unable to instrument calls made within dotted
names, including explicit dereferences. A source coverage obligation is
emitted and the call's coverage is undertermined.
