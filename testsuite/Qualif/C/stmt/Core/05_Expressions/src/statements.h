#ifndef STATEMENTS_H
# define STATEMENTS_H

/* Execute "statemant-all" and "statement-cond" tagged statements if FULL, or just
   "statement-all" ones otherwise.
   Auxillary "statement-aux-all" tagged statements are executed if and only if
   "statement-all" are executed, and "statement-aux-cond" ones are too iff.
   "statement-cond" ones are executed.*/
void run_statements (int full);

#endif
