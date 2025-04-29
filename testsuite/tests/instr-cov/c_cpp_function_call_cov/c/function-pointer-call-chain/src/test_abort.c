#include <stdlib.h>

struct list;

typedef struct list *(*fct_type) ();

struct list
{
  struct list *next;
  int data;
  fct_type get_head;
};

static struct list *HEAD = (struct list *) 0;

struct list *
head_abort ()
{
  /* GNATCOV_DUMP_BUFFERS */
  abort ();    // # not-call
  return HEAD; // # not-reached
}

int
main (void)
{
  struct list end = { .next = (struct list *) 0, .data = 1, .get_head = NULL };

  struct list start = { .next = &end, .data = 1, .get_head = head_abort };

  HEAD = &start;

  int x = (*HEAD)         // # statement
            .get_head ()  // # first_call
            ->get_head () // # second_call
            ->data;
}

//# test_abort.c
// /statement/   l+ ## 0
// /first_call/  l+ ## 0
// /second_call/ l! ## c-
// /not-reached/ l- ## s-
// /not-call/ 	 l- ## s-,c-
