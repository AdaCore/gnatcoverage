Null statements (ARM 5.2)
=========================

Check SC of Null statements (ARM 5.2)

Check various code fragments that contain null statements such as:

* case statement alternative;

* null procedure;

* last statement in a sequence, with a label attached to it (used for
  a control transfer to skip earlier statements in the sequence).

Check that only those null statements that are not executed are reported as
uncovered.

