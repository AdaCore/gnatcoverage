--  Test to check whether we can analyze correctly parts of a task type
--  covered by different instances of the type. There are three if
--  statements, each of them checking the discriminant used in the
--  task instantiation. There are two tasks instances, intended to
--  cover the first two blocks, and a third block is not covered
--  because no task instance is declared to cover that block.

with System;

package Blocks_By_Tasks is
   type Task_Kind is (Block1, Block2, Block3);
   
   task type Processing (Kind : Task_Kind) is
      pragma Priority (System.Priority'Last);
   end Processing;
   
   T1 : Processing (Block1);
   T2 : Processing (Block2);
end Blocks_By_Tasks;
