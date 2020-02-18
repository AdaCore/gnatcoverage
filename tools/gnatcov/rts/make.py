#! /usr/bin/env python3

import argparse
import subprocess
import sys


parser = argparse.ArgumentParser(
    description='Development convenience script to build and install'
                ' GNATcov_RTS')
parser.add_argument('--prefix', help='Installation prefix for GNATcov_RTS')
parser.add_argument('--target', help='Build target')
parser.add_argument('--RTS', help='Name of the Ada runtime to use')


def spawn(*argv):
    subprocess.check_call(argv)


def build_and_install(project, common_argv, install_argv):
    spawn('gprbuild', '-P', project, '-p', *common_argv)
    spawn('gprinstall', '-P', project, '-f', '-p',
          *(common_argv + install_argv))


def main(args):
    is_cross = args.target
    project = 'gnatcov_rts' if is_cross else 'gnatcov_rts_full'

    # If gnatcov_rts_full was already installed, just uninstall it (one
    # single gprbuild --uninstall is enough for all variants). Don't abort if
    # this fails, as the project may not be installed yet.
    try:
        spawn('gprinstall', '--uninstall', '-P', project)
    except subprocess.CalledProcessError:
        pass

    try:

        common_argv = []
        install_argv = []

        if args.target:
            common_argv.append('--target={}'.format(args.target))
        if args.RTS:
            common_argv.append('--RTS={}'.format(args.RTS))
        if args.prefix:
            install_argv.append('--prefix={}'.format(args.prefix))

        if is_cross:
            build_and_install(project, common_argv, install_argv)
        else:
            install_argv.extend(['--sources-subdir=include/gnatcov_rts_full',
                                 '--build-var=LIBRARY_TYPE'])
            for lib_type in ('static', 'static-pic', 'relocatable'):
                build_and_install(
                    project,
                    common_argv + ['-XLIBRARY_TYPES={}'.format(lib_type)],
                    install_argv + ['--build-name={}'.format(lib_type)])

    except subprocess.CalledProcessError:
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main(parser.parse_args()))
