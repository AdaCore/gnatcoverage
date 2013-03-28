# -*- coding: utf-8 -*-

'''
Generation engine, that makes choice on what to generate.
'''

import os
import os.path
import textwrap


import SCOV.expgen.language             as language
import SCOV.expgen.language.ada
import SCOV.expgen.generator            as generator
import SCOV.expgen.generator.composition
import SCOV.expgen.generator.parsing
import SCOV.expgen.generator.utils
import SCOV.expgen.topology             as topology
import SCOV.expgen.utils                as utils

from SCOV.expgen.generator.errors import GenerationError, DriverError


# Used for bindings between test drivers and generated code.
ada = language.ada.Language()


# TODO: it would be nice to test all operands on one single line sometimes...
# but when?
one_operand_per_line = True


def run(group_py):
    '''
    Run generation in the directory containing "group_py", catch any exception
    if any, display them and exit accordingly.
    '''
    import sys

    env = generator.utils.Environment()
    group_py_dir = os.path.dirname(os.path.abspath(group_py))
    try:
        with env.get_dir(group_py_dir):
            generate_all(env)
    except GenerationError as e:
        print >> sys.stderr, '{}: error: {}'.format(e.context, e.message)
        sys.exit(1)


def generate_all(env):
    '''
    Generate code for all topology directories in the current directory.
    '''
    # Each directory contains drivers for a given topology.
    for topo_dir in os.listdir('.'):
        if not os.path.isdir(topo_dir) or topo_dir == 'src':
            continue
        with env.get_dir(topo_dir):
            generate_topology(topo_dir, env)

def generate_topology(topo_dir, env):
    '''
    Generate code inside the `topo_dir` directory.
    '''
    topo = None
    truth_vectors = set()

    # Look at all drivers to collect the truth vectors involved. Extract the
    # topology used and check that each driver use the same one.
    with env.get_dir('src'):
        for driver in os.listdir('.'):
            if not driver.startswith('test_') or not driver.endswith('.adb'):
                continue

            drv_topo = generator.parsing.parse_driver(
                topo_dir, driver, truth_vectors
            )

            # Check that this driver has a topology and that its topology is
            # not different from others.
            if drv_topo is None:
                raise DriverError(topo_dir, driver, None,
                    'This driver has no topology'
                )
            elif (
                topo is not None
                and not utils.is_topology_equal(topo.expression, drv_topo)
            ):
                raise DriverError(topo_dir, driver, None,
                    'This driver has a different topology from other one'
                )
            topo = topology.Topology(drv_topo)

    # Check that there is at least one test driver.
    if topo is None:
        raise GenerationError(topo_dir, 'There is no driver here')

    # Now we have the topology and the truth vectors, we can generate
    # everything else.
    for lang in generator.composition.languages:
        with env.get_dir(lang.NAME):
            generate_language(env, topo, truth_vectors, lang)

def generate_language(env, topo, truth_vectors, lang):
    '''
    Generate code for a given topology inside for the given language.
    '''
    # First get a list of operand kinds and contexts that can be used with
    # `lang`.
    reachable_operand_kinds = lang.filter_operand_kinds(
        generator.composition.operand_kinds
    )
    reachable_contexts = lang.filter_contexts(generator.composition.contexts)

    # Keep track of operand kinds that are not used.
    first_unused_op_kind = 0
    for i, ctx in enumerate(reachable_contexts):
        # Generate formal types/names for the "compute" function that will be
        # used by the "run" procedure.
        op_kinds = [
            reachable_operand_kinds[(i + j) % len(reachable_operand_kinds)]
            for j in range(topo.arity)
        ]
        first_unused_op_kind = i + topo.arity

        with env.get_dir('Op{}'.format(i)):
            generate_ctx_op(env, topo, truth_vectors, lang, ctx, op_kinds)

    # If some operand kinds were not used in the previous combination, use them
    # with the first context. This way, each operand kind and each context will
    # have been tested.
    if first_unused_op_kind < len(reachable_operand_kinds):
        # Isolate remaining operand kinds.
        operand_kinds_to_test = reachable_operand_kinds[first_unused_op_kind:]

        # The more operands there is in an expression, the more operand kinds
        # we can use in one combination, and thus the less combinations we have
        # to do.
        combination_count = max(1, len(operand_kinds_to_test) - topo.arity + 1)

        ctx = reachable_contexts[0]

        for k in range(combination_count):
            i += 1

            op_kinds = [
                operand_kinds_to_test[(k + j) % len(operand_kinds_to_test)]
                for j in range(topo.arity)
            ]
            with env.get_dir('Op{}'.format(i)):
                generate_ctx_op(env, topo, truth_vectors, lang, ctx, op_kinds)

def generate_ctx_op(env, topo, truth_vectors, lang, ctx, op_kinds):
    formal_names = [
        'X{}'.format(i + 1)
        for i in range(topo.arity)
    ]
    operands = [
        op_kind.get_operand(name)
        for name, op_kind in zip(formal_names, op_kinds)
    ]
    formal_types = [op_kind.param_type for op_kind in op_kinds]

    decision = topo.instanciate(operands, formal_names, ctx)
    program = ctx.get_program(decision)

    # Generate the needed type declarations in the TYPES module.
    used_types = utils.make_type_set(sum(
        (tuple(op_kind.used_types) for op_kind in op_kinds),
        ()
    ))

    with env.get_dir('src'):
        # First generate types needed by operands.
        with open(
            lang.get_specification_filename(lang.TYPES_MODULE), 'w'
        ) as types_fp:
            lang.serialize_specification_types(types_fp, used_types)

        # Then generate the ADA binding for the run module...
        with open(
            ada.get_specification_filename(ada.RUN_MODULE), 'w'
        ) as run_fp:
            ada.serialize_run_module_interface(run_fp, lang, truth_vectors)
        # ... and the run module itself, in the decided language.
        with open(
            lang.get_implementation_filename(lang.RUN_MODULE), 'w'
        ) as run_fp:
            lang.serialize_run_module_implementation(
                run_fp, op_kinds, truth_vectors
            )

        # And finally generate the specification and the implementation for the
        # computing module.
        with open(
            lang.get_specification_filename(lang.COMPUTING_MODULE), 'w'
        ) as comp_fp:
            lang.serialize_specification_program(
                comp_fp, program, formal_names, formal_types
            )

        with open(
            lang.get_implementation_filename(lang.COMPUTING_MODULE), 'w'
        ) as comp_fp:

            # Prepend the "computing" module implementation with some comments
            # documenting the operands usage and the target of the context.

            def get_doc(instance):
                text = type(instance).__doc__ or '<no documentation>'
                text = ' '.join(text.split())
                return textwrap.wrap(text, 72)

            lines = []
            lines.append('Operands:')
            for name, op_kind in zip(formal_names, op_kinds):
                lines.append('  - {}: {}'.format(name, type(op_kind).__name__))
                lines.extend(
                    '    {}'.format(line)
                    for line in get_doc(op_kind)
                )
            lines.append('Context: {}'.format(type(ctx).__name__))
            lines.extend(
                '    {}'.format(line)
                for line in get_doc(ctx)
            )

            # Serialize the implementation code itself.
            for line in lines:
                comp_fp.write(lang.format_comment(line))
                comp_fp.write('\n')
            comp_fp.write('\n')
            lang.serialize_implementation(
                comp_fp, program, formal_names, formal_types,
                one_operand_per_line
            )

    # The "test.py" testcase file is hardcoded...
    with open('test.py', 'w') as test_fp:
        test_fp.write('''\
from SCOV.tc import *
from SCOV.tctl import CAT

[TestCase(category=cat).run() for cat in CAT.critcats]
thistest.result()
''')
