#include "simple_pools.h"

#define POOL_SIZE 20

static struct sensor_node store[POOL_SIZE];
static unsigned next_free = 0;

struct sensor_node *
pool_allocate (void)
{
  return &store[next_free++];
}
