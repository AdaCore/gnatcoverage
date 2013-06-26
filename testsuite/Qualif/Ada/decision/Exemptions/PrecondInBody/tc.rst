Exercise the use of an exemption region to exempt a functional precondition
===========================================================================

Exercise the use of an exemption region to exempt a functional precondition
expressed manually (explicit check) at the beginning of a subprogram body.

Check:

* Situations where the precondition is always evaluated True
  (nominal case, exempted DC violation)

* Situations where the precondition is evaluated only False
  (force error case, exempted DC violation)

* Consolidation of the previous cases: precondition exercized both
  ways (no exempted violation)
 
