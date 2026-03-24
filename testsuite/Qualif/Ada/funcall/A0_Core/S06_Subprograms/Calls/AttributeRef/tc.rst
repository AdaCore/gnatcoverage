**Check the correct instrumentation of calls in an attribute reference**

Make a reference to an attribute of a call's return value. This should be
correctly instrumented and its coverage reported normally. Attribute
references should not be instrumented as calls so no coverage violation is
reported when they are not executed.
