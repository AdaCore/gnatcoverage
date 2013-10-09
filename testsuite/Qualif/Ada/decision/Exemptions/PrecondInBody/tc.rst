Exercise the use of an exemption region to exempt a functional precondition at beginning of subprogram body
===========================================================================================================

Exercise the use of an exemption region to exempt a functional precondition
expressed manually (explicit check) at the beginning of a subprogram body.

Check:

* Situations where the precondition always evaluates to True
  (nominal case, exempted DC violation)

* Situations where the precondition only evaluates to False
  (force error case, exempted DC violation)

* Consolidation of the previous cases: precondition exercised both
  ways (no exempted violation)
 
