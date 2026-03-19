#ifndef RANGE_ITERABLE_HH
#define RANGE_ITERABLE_HH

class RangeCursor
{
  int max;
  int current;

public:
  RangeCursor (int max, int current) : max (max), current (current) {}

  int
  operator* () const
  {
    return current;
  }

  RangeCursor &
  operator++ ()
  {
    current += 1;
    return *this;
  }

  static inline bool
  equals (const RangeCursor &lhs, const RangeCursor &rhs)
  {
    return lhs.current == rhs.current;
  }
};

inline bool
operator!= (const RangeCursor &lhs, const RangeCursor &rhs)
{
  return !RangeCursor::equals (lhs, rhs);
}

class RangeIterable
{
  int max;

public:
  RangeIterable (int max) : max (max) {}

  RangeIterable (const RangeIterable &&r) : max (r.max) {}

  RangeCursor
  begin () const
  {
    return RangeCursor (max, 0);
  }

  RangeCursor
  end () const
  {
    return RangeCursor (max, max + 1);
  }
};

#endif
