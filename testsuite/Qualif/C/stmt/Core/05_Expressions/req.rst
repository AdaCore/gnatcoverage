SC expectations for chapter 6, section 5: Expressions
======================================================

SC expectations regarding chapter 6, section 5: Expressions


.. rubric:: Requirement(s)



All C expressions can be used as a statement.


.. rubric:: Testing Strategy



We exercice every possible kind of expression, as defined by the C99 ISO
standard, in the following situations:

-   no code is run, so all statement-expression must be uncovered
-   code is run partially, so only one out of two statement-expressions must be
    covered
-   code is run fully, so the two statement-expressions must be covered


.. qmlink:: TCIndexImporter

   *


