"""Experiment around program instrumentation and MC/DC computation.

This script is a proof of concept to instrument code for decisions in order to
compute MC/DC efficiently.

The starting point is to acknowledge that each decision has a limited set of
paths to represent its various evaluations. For instance, the following
decision:

    (A and B) or C

Has the following set of possible evaluations:

    A evaluates to False,                       C evaluates to False
    A evaluates to False,                       C evaluates to True
    A evaluates to True,  B evaluates to False, C evaluates to False
    A evaluates to True,  B evaluates to False, C evaluates to True
    A evaluates to True,  B evaluates to True

The idea here is then to associate to each of these evaluations one index.
The next step is during evaluation to remember in an array of booleans (one
boolean per possible evaluation, above) which evaluations happen. Finally,
using this set of evaluations, it is possible to assess MC/DC the usual way,
making sure that each condition has an independent influence on the result of
the decision.

The only problem left to solve is: how to associate these indexes to
evaluations?

A naive way is to consider all possible combinations for conditions. This does
not scale, however, as this creates a lot of indexes (2**N, for the number of
conditions N). This makes the buffer array described above unrealistically
large for pathological decisions such as:

    A or B or C or D or ...

In practice, the number of possible evaluations is on the same order of
magnitude as the number of conditions.

This POC suggests to rather enumerate all evaluations in a deterministic way:
traverse the BDD (binary decision diagram) in a depth-first recursive fashion
and remember all paths than can reach outcomes: see the IndexedBDD class below.
This creates a contiguous set of index (one per possible evaluation).

It is then possible to generate efficient code whose size is linear compared to
the number of conditions to evaluate this index at the same time as the
conditions are evaluated. See the ada_eval_code function below, which generates
an example code.

Finally, the "test" function below generates BDD and IndexedBDD as graphical
diagrams (using dot(1)), generates an Ada test program to check that the
proposed index computation works as expected. This function is then used on
several example decisions.
"""

from __future__ import print_function

import subprocess


class Expression(object):
    """Base class for all nodes that constitute a decision expression."""

    def __or__(self, other):
        return OrElse(self, other)

    def __and__(self, other):
        return AndThen(self, other)

    def __not__(self):
        return Not(self)

    @property
    def operands(self):
        raise NotImplementedError()


class Condition(Expression):
    """Condition in a decision expression.

    Conditions are identified by a single letter (its "symbol") and each letter
    must appear only one in a valid decision expression tree.
    """

    def __init__(self, symbol):
        assert isinstance(symbol, str) and len(symbol) == 1
        self.symbol = symbol

    def __repr__(self):
        return self.symbol

    def __eq__(self, other):
        return isinstance(other, Condition) and self.symbol == other.symbol

    def __hash__(self):
        return hash(self.symbol)


class AndThen(Expression):
    """And-then construct in a decision expression."""

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return '({} and {})'.format(self.lhs, self.rhs)

    @property
    def operands(self):
        return [self.lhs, self.rhs]


class OrElse(Expression):
    """Or-else construct in a decision expression."""

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return '({} or {})'.format(self.lhs, self.rhs)

    @property
    def operands(self):
        return [self.lhs, self.rhs]


class Not(Expression):
    """Not construct in a decision expression."""

    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return 'not {}'.format(self.expr)

    @property
    def operands(self):
        return [self.expr]


class BDD(object):
    """Binary decision diagram for a decision.

    The BDD is represented as a graph. Nodes are either conditions to evaluate
    or decision outcomes (true/false). Conditions have two outgoing edges: one
    to follow if the condition evaluates to false and the other to follow if
    the condition evaluates to true.
    """

    class Placeholder(object):
        pass

    class EdgePair(object):
        """Pair of outgoing edges for a condition."""

        def __init__(self, condition, for_false, for_true):
            assert isinstance(condition, Condition)
            self.condition = condition
            """Condition whose evaluation leads to the two outgoing edges."""

            assert isinstance(for_false, (BDD.Placeholder, Condition, bool))
            self.for_false = for_false
            """Destination of the edge in case of evaluation to "false"."""

            assert isinstance(for_true, (BDD.Placeholder, Condition, bool))
            self.for_true = for_true
            """Destination of the edge in case of evaluation to "true"."""

    def __init__(self, decision):
        self.decision = decision
        """Decision expression to analyze."""

        self.conditions = []
        """Deterministically-ordered list of conditions for this decision."""

        self.edges = {}
        """Mapping: Condition -> BDD.EdgePair for all conds. in the BDD."""

        # Check that the given expression is valid (all conditions present at
        # most once).
        assert isinstance(decision, Expression)
        conditions = set()

        def check_expr(expr):
            assert isinstance(expr, Expression)
            if isinstance(expr, Condition):
                assert expr.symbol not in conditions
                conditions.add(expr.symbol)
            else:
                for o in expr.operands:
                    check_expr(o)

        check_expr(decision)

        # Compute all edges in this BDD. This is done in two passes: first do a
        # recursive traversal of the expression tree, creating the edges.
        #
        # During this traversal, not all edge destinations are known: in this
        # case, we create Placeholder instances instead. Returning from
        # recursion gives the information about destinations, so we remember in
        # the "substitutions" dict how placeholders should be replaced.
        #
        # The second pass just iterates over edges and does the substitution.
        substitutions = {}

        def compute_edges(expr, for_false, for_true):
            """Compute outgoing edges for conditions in the given expression.

            When "expr" evaluates to false, we create an edge to "for_false",
            and to "for_true" when it evaluates to true. Return the first
            condition to evaluate when evaluating "expr".

            As we visit each condition exactly once, this deterministic
            traversal is also used as an opportunity to construct the
            "self.conditions" list.
            """
            if isinstance(expr, Condition):
                assert expr not in self.edges
                self.conditions.append(expr)
                self.edges[expr] = BDD.EdgePair(expr, for_false, for_true)
                root = expr

            elif isinstance(expr, AndThen):
                right_root_ph = BDD.Placeholder()
                left_root = compute_edges(expr.lhs, for_false, right_root_ph)
                right_root = compute_edges(expr.rhs, for_false, for_true)

                root = left_root
                substitutions[right_root_ph] = right_root

            elif isinstance(expr, OrElse):
                right_root_ph = BDD.Placeholder()
                left_root = compute_edges(expr.lhs, right_root_ph, for_true)
                right_root = compute_edges(expr.rhs, for_false, for_true)

                root = left_root
                substitutions[right_root_ph] = right_root

            elif isinstance(expr, Not):
                root = compute_edges(expr.expr, for_true, for_false)

            else:
                assert False

            return root

        self.entry_condition = compute_edges(self.decision, False, True)
        """First condition to evaluate when evaluating the whole decision."""

        # Second pass: apply the placeholder substitutions
        for e in self.edges.values():
            if isinstance(e.for_false, BDD.Placeholder):
                e.for_false = substitutions[e.for_false]
            if isinstance(e.for_true, BDD.Placeholder):
                e.for_true = substitutions[e.for_true]

    def write_dot(self, f):
        """Emit this BDD as a dot diagram to the "f" file."""
        print('False [shape=box];', file=f)
        print('True [shape=box];', file=f)
        for c in self.edges:
            print('{} [shape=circle];'.format(c), file=f)
        for e in self.edges.values():
            print('{e.condition} -> {e.for_false} [label=false];\n'
                  '{e.condition} -> {e.for_true} [label=true];'
                  .format(e=e), file=f)


class IndexedBDD(object):
    """Binary decision diagram for a decision with index information.

    This diagram contains the same information as in BDD, plus information to
    compute the evaluation index.

    The general idea is that when starting to evaluate the decision, we aim at
    the first index (offset = 0):

      * If the first condition evaluates to false, we leave the offset as-is
        and move on to the next condition to evaluate.

      * If this first condition evaluates to true, however, we increment the
        offset by the number of evaluations that can happen when following the
        "false" outgoing edge, and move on to the next condition to evaluate.

      * When reaching an outcome, the current offset becomes the index.

    This method guarantees that each evaluation path gives a different index
    and that all possible indexes are contiguous.
    """

    class EdgePair(object):
        """Pair of outgoing edges for a condition."""

        def __init__(self, condition, for_false, for_true):
            assert isinstance(condition, Condition)
            self.condition = condition
            """Condition whose evaluation leads to the two outgoing edges."""

            assert isinstance(for_false, (Condition, bool))
            self.for_false = for_false
            """Destination of the edge in case of evaluation to "false"."""

            assert isinstance(for_true, (Condition, bool))
            self.for_true = for_true
            """Destination of the edge in case of evaluation to "true"."""

            self.offset_for_true = None
            """Offset increment when "condition" evaluates to true."""

            self.path_count = None
            """
            Number of possible evaluation paths from the evaluation of
            "condition".
            """

    def __init__(self, bdd):
        assert isinstance(bdd, BDD)
        self.bdd = bdd
        """Raw BDD for which we want to compute indexes."""

        self.edges = {}
        """
        Mapping: Condition -> IndexedBDD.EdgePair for all conds. in the BDD.
        """

        def path_count(cond_or_bool):
            """Return the number of possible evaluations from given node.

            This assumes that outgoing edges for the node have already been
            computed.
            """
            if isinstance(cond_or_bool, bool):
                # If we already reached a decision outcome, there is only one
                # possible evaluation: the outcome itself.
                return 1

            else:
                assert isinstance(cond_or_bool, Condition)
                edge_pair = self.edges[cond_or_bool]
                assert edge_pair.path_count is not None
                return edge_pair.path_count

        def compute_edges(node):
            """Compute outgoing edges for the given BDD node."""
            if isinstance(node, bool) or node in self.edges:
                return

            # Create an IndexedBDD edge for the BDD edge corresponding to
            # "node".
            simple_edge = self.bdd.edges[node]
            edge = IndexedBDD.EdgePair(simple_edge.condition,
                                       simple_edge.for_false,
                                       simple_edge.for_true)

            # Compute outgoing edges for both destination nodes
            compute_edges(simple_edge.for_false)
            compute_edges(simple_edge.for_true)

            # Now compute offsets increments for both edges
            path_count_for_false = path_count(edge.for_false)
            path_count_for_true = path_count(edge.for_true)
            edge.offset_for_true = path_count_for_false
            edge.path_count = path_count_for_false + path_count_for_true

            self.edges[node] = edge

        compute_edges(self.entry_condition)

    @property
    def entry_condition(self):
        """See BDD.entry_contition."""
        return self.bdd.entry_condition

    @property
    def conditions(self):
        """See BDD.conditions."""
        return self.bdd.conditions

    def write_dot(self, f):
        """Emit this indexed BDD as a dot diagram to the "f" file."""
        def node_id(offset, node):
            return '{}_{}'.format(node, offset)

        def output_node(offset, node):
            print('{} [shape={}];'.format(
                node_id(offset, node),
                'circle' if isinstance(node, Condition) else 'box'
            ), file=f)

        def process_edges(offset, node):
            self_id = node_id(offset, node)
            output_node(offset, node)
            if isinstance(node, bool):
                return

            edge_pair = self.edges[node]

            left_offset = offset
            left_id = node_id(offset, edge_pair.for_false)
            output_node(left_offset, edge_pair.for_false)

            right_offset = left_offset + edge_pair.offset_for_true
            right_id = node_id(right_offset, edge_pair.for_true)
            output_node(right_offset, edge_pair.for_true)

            print('{} -> {} [label="false+{}"];\n'
                  '{} -> {} [label="true+{}"];'
                  .format(self_id, left_id, 0,
                          self_id, right_id, edge_pair.offset_for_true),
                  file=f)

            process_edges(left_offset, edge_pair.for_false)
            process_edges(right_offset, edge_pair.for_true)

        process_edges(1, self.entry_condition)


def ada_eval_code(ibdd):
    """
    Return example Ada code to evaluate decision and the corresponding index.

    This returns the body of a Decision function, which takes the value of each
    condition (booleans) in parameter and returns the boolean evaluation. It
    assigns the computed index (1-based) to the Witnessed_Index non-local
    variable.
    """
    result = [
        'function Decision',
    ]
    for i, cond in enumerate(ibdd.conditions):
        param = '{}{} : Boolean := False'.format(
            ('  (' if i == 0 else '   '),
            cond
        )
        param += ';' if i < len(ibdd.conditions) - 1 else ') return Boolean'
        result.append(param)
    result.extend([
        'is',
        '   Index  : Positive := 1;',
        '   Result : Boolean;',
        'begin',
    ])

    result_label = 'Return_Result'

    def cond_label(cond):
        return 'Cond_{}'.format(cond)

    visited = {False, True}

    def process_single_edge(node, offset):
        if offset:
            result.append('      Index := Index + {};'.format(offset))

        if isinstance(node, bool):
            result.append('      Result := {};'.format(node))
            result.append('      goto {};'.format(result_label))
        else:
            result.append('      goto {};'.format(cond_label(node)))

    def process_edges(node):
        if node in visited:
            return
        visited.add(node)
        edge_pair = ibdd.edges[node]

        result.append('   <<{}>> if {} then'.format(cond_label(node), node))
        process_single_edge(edge_pair.for_true, edge_pair.offset_for_true)
        result.append('   else')
        process_single_edge(edge_pair.for_false, 0)
        result.append('   end if;')

        process_edges(edge_pair.for_false)
        process_edges(edge_pair.for_true)

    process_edges(ibdd.entry_condition)

    result.extend([
        '   <<{}>> Witnessed_Index := Index;'.format(result_label),
        '   return Result;',
        'end Decision;'
    ])
    return '\n'.join(result)


def indent(text, prefix='   '):
    return '\n'.join(prefix + line for line in text.splitlines())


def test(name, expr):
    bdd = BDD(expr)
    ibdd = IndexedBDD(bdd)

    # Graphical diagram to show the various BDDs forms
    dot_file = '{}.dot'.format(name)
    png_file = '{}.png'.format(name)

    with open(dot_file, 'w') as f:
        print('digraph InstrumentMCDC {', file=f)

        print('decision [shape=box,label="{}"];'.format(expr), file=f)

        print('subgraph cluster_BDD {', file=f)
        print('label="BDD";', file=f)
        print('color=black;', file=f)
        bdd.write_dot(f)
        print('}', file=f)

        print('subgraph cluster_IndexedBDD {', file=f)
        print('label="Indexed BDD";', file=f)
        print('color=black;', file=f)
        ibdd.write_dot(f)
        print('}', file=f)

        print('}', file=f)
    subprocess.check_call(['dot', '-Tpng', '-o', png_file, dot_file])

    # Small program to test the code that the ada_eval_code function above
    # emits.
    ada_file = '{}.adb'.format(name)

    with open(ada_file, 'w') as f:
        print('procedure {} is'.format(name), file=f)
        print('   Witnessed_Index : Positive;', file=f)
        print(indent(ada_eval_code(ibdd)), file=f)
        print('begin', file=f)

        # Enumerate all possible decision evaluations and emit assertions to
        # check that 1) the decision evaluates the expected boolean result and
        # 2) that the computed index is the one expected.
        #
        # "tests" is a list of decision evaluations to test. Each list item is
        # a couple: first element is a list of (condition, value) and second
        # element is the expected boolean result for the evaluation.
        #
        # Possible decision evaluations are enumerated so that the expected
        # indexes go from 1 to N.
        tests = []

        def enumerate_tests(node, path):
            if isinstance(node, bool):
                tests.append((path, node))
                return

            edge = ibdd.edges[node]
            enumerate_tests(edge.for_false, path + [(node, False)])
            enumerate_tests(edge.for_true, path + [(node, True)])

        enumerate_tests(ibdd.entry_condition, [])

        for index, (path, result) in enumerate(tests, 1):
            print('  pragma Assert(Decision ({}) = {});'.format(
                ', '.join('{} => {}'.format(cond, value)
                          for cond, value in path),
                result
            ), file=f)
            print('  pragma Assert (Witnessed_Index = {});'.format(index),
                  file=f)

        print('end {};'.format(name), file=f)

    subprocess.check_call(['gnatmake', '-q', '-g', '-gnata', ada_file])
    subprocess.check_call(['./{}'.format(name)])


if __name__ == '__main__':
    A = Condition('A')
    B = Condition('B')
    C = Condition('C')
    D = Condition('D')
    E = Condition('E')
    F = Condition('F')
    G = Condition('G')

    test('test_1', Not(A & B) | C)
    test('test_2', (A & Not(B)) | (C & (D | E)))
    test('test_3', (A & B & C & D) | (E & (F | G)))
    test('test_4', (A & (B | C)))
