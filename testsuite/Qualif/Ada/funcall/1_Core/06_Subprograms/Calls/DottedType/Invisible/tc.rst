**Check FCC for calls to a function returning an invisible type**

Make a call to a function returning a type which is not visible at the call
site. Such calls cannot be instrumented and should their coverage status
should be repoted as undeterminded.
