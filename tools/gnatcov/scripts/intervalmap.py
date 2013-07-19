# -*- coding: utf-8 -*-

import bisect


class IntervalMap(object):
    """Map integer intervals to anything else."""

    def __init__(self):
        # Sorted list of inserted bounds.
        self.bounds = []
        # When an interval [x; y[ is associated with a `value`,
        # `self.items[x]` is set to `value`.
        self.values = {}

    def __setitem__(self, interval, value):
        """Associate a `value` with the `interval`.

        `interval` must be a slice object with integer bounds, and it must not
        overlap with previously added intervals. Nothing is done when the
        interval is empty ([x:y] when x >= y).
        """

        if not isinstance(interval, slice):
            raise TypeError('Interval required')

        elif interval.start is None or interval.stop is None:
            raise ValueError('Interval must have integer bounds')

        elif interval.start >= interval.stop:
            # Do nothing for the empty interval.
            return

        # The first interval to add is a special case.
        if not self.bounds:
            self.bounds = [interval.start, interval.stop]
            self.values[interval.start] = value
            return

        # Compute where to insert bounds.
        start_insert_index = bisect.bisect_left(self.bounds, interval.start)
        stop_insert_index = bisect.bisect_left(self.bounds, interval.stop)

        # Insert bounds when needed and check `interval` do not overlap with
        # previously inserted intervals.
        if start_insert_index == stop_insert_index:
            if start_insert_index == 0:
                if self.bounds[0] != interval.stop:
                    # Example: [1;2[ is added to bounds [2;4;5]
                    self.bounds.insert(0, interval.stop)
                else:
                    # Example: [1;2[ is added to bounds [3;4;5]
                    pass
                self.bounds.insert(0, interval.start)

            elif start_insert_index >= len(self.bounds):
                # Example: [5;6[ is added to bounds [1;2;3]
                self.bounds.append(interval.start)
                self.bounds.append(interval.stop)

            else:
                # This interval should not be inside another previously added
                # one, i.e.: the bound right before the one we are going to
                # insert must not start an added interval.
                if self.bounds[start_insert_index - 1] in self.values:
                    # Example: [3;4[ is added to bounds [1;6]
                    raise ValueError(
                        '[{};{}[ is inside previously added'
                        ' interval [{};{}['.format(
                            interval.start, interval.stop,
                            self.bounds[start_insert_index - 1],
                            self.bounds[start_insert_index]
                        )
                    )

                if self.bounds[stop_insert_index] != interval.stop:
                    # Insert the end bound only when not already present:
                    # Example: [3;4[ is added to bounds [1;2;4;5]
                    self.bounds.insert(stop_insert_index, interval.stop)
                else:
                    # Example: [3;4[ is added to bounds [1;2;5;6]
                    pass
                self.bounds.insert(start_insert_index, interval.start)

        elif stop_insert_index > start_insert_index + 1:
            # Example: [2;4[ is added to bounds [1;3;5]
            raise ValueError('[{};{}[ contains previous bound {}'.format(
                interval.start, interval.stop,
                self.bounds[start_insert_index + 1]
            ))

        else:
            # Here, start_insert_index + 1 == stop_insert_index.
            if self.bounds[start_insert_index] != interval.start:
                # Example: [1;3[ is added to bounds [2;4]
                raise ValueError('[{};{}[ contains previous bound {}'.format(
                    interval.start, interval.stop,
                    self.bounds[start_insert_index]
                ))

            # The starting bound is already inserted: just check it doesn't
            # start any previously added interval.
            elif interval.start in self.values:
                # Example: [3; 4] is added to bounds [1;2;3;5]
                raise ValueError(
                    '[{};{}[ overlaps with previously added'
                    ' interval [{};{}['.format(
                        interval.start, interval.stop,
                        interval.start, self.bounds[start_insert_index + 1]
                    )
                )

            # The starting bound is already inserted: check for the ending one.
            elif (
                stop_insert_index >= len(self.bounds) or
                self.bounds[stop_insert_index] != interval.stop
            ):
                # Example: [2; 4] is added to bounds [1;2;4;5]
                self.bounds.insert(stop_insert_index, interval.stop)

            else:
                # Example: [2; 4] is added to bounds [1;2;5;6]
                pass

        # And finally, insert the value itself!
        self.values[interval.start] = value


    def __getitem__(self, key):
        """Return the value associated to the interval that contains `key`.

        Raise a KeyError if there is no such interval.
        """

        start_bound_index = bisect.bisect_right(self.bounds, key)
        if start_bound_index < 1:
            raise KeyError('No interval contains {}'.format(key))
        else:
            start_bound = self.bounds[start_bound_index - 1]

        return self.values[start_bound]

    def __contains__(self, key):
        """Return if `key` belongs to some covered interval.
        """
        try:
            self[key]
        except KeyError:
            return False
        else:
            return True

    def get(self, key, default=None):
        """Return the value associated to the interval that contains `key`, or
        `default` if there is no such interval.
        """
        try:
            return self[key]
        except KeyError:
            return default

    def items(self):
        """Return an iterator over added intervals and associated values.

        Yielded items are like: `((low_bound, high_bound), value)`
        """

        for i, bound in enumerate(self.bounds):
            try:
                value = self.values[bound]
            except KeyError:
                # If this happens, `bound` doesn't start an interval.
                continue
            interval = (bound, self.bounds[i + 1])
            item = (interval, value)
            yield item

    def __repr__(self):
        return '{{{}}}'.format(', '.join(
            '[{}; {}[: {}'.format(low, high, repr(value))
            for (low, high), value in self.items()
        ))


if __name__ == '__main__':
    # Run tests...

    m = IntervalMap()

    def sanity_check():
        """Check that the interval map is healthy (no buggy internal data)."""

        # Bounds must be strictly increasing.
        last_bound = None

        # First bound must start an interval, and so does every bound that
        # follow another bound that does not.
        last_bound_starts_interval = False

        for bound in m.bounds:
            if last_bound is not None:
                assert last_bound < bound

            starts_interval = bound in m.values
            assert last_bound_starts_interval or starts_interval

            last_bound = bound
            last_bound_starts_interval = starts_interval

    def add(low, high, value, exception_expected=None):
        print('Adding [{}; {}[: {}'.format(low, high, repr(value)))
        try:
            m[low:high] = value
        except Exception as e:
            if exception_expected and isinstance(e, exception_expected):
                print('  {}: {}'.format(type(e).__name__, e))
            else:
                raise
        else:
            if exception_expected:
                raise RuntimeError(
                    'I was expecting an error, but nothing happened.'
                )
            else:
                print('  ', m)
                print('  >>', m.bounds)

        sanity_check()

    print('Empty interval map')
    print('  ', m)

    add(1, 4, 'B')

    add(0, 2, 'Z', ValueError)
    add(0, 4, 'Z', ValueError)
    add(0, 5, 'Z', ValueError)

    add(1, 2, 'Z', ValueError)
    add(1, 4, 'Z', ValueError)
    add(1, 5, 'Z', ValueError)

    add(2, 3, 'Z', ValueError)
    add(2, 4, 'Z', ValueError)
    add(2, 5, 'Z', ValueError)

    add(3, 0, 'Z')

    add(0, 1, 'A')
    add(4, 5, 'B')

    add(10, 15, 'D')
    add(20, 25, 'F')
    add(30, 35, 'H')

    add(5, 8, 'C')
    add(18, 20, 'E')
    add(25, 30, 'G')
