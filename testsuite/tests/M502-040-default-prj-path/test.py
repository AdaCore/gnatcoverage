import os
import os.path

from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import contents_of, text_to_file
from SUITE.tutils import exepath_to, gpr_emulator_package, gprbuild, xrun


env.add_search_path(env_var='PATH',
                    path=os.path.join(os.getcwd(), 'bin'),
                    append=True)
env.add_search_path(env_var='GPR_PROJECT_PATH',
                    path=os.path.join(os.getcwd(), 'proj'))

os.chdir('proj/ut')
text_to_file(
    contents_of('ut.gpr.template').format(
        pkg_emulator=gpr_emulator_package()
    ),
    'ut.gpr'
)

gprbuild('ut')
xrun(['-P', 'ut', exepath_to('obj/main')])

thistest.result()
