import platform
import ctypes
import os
import time
import sys
from distutils.spawn import find_executable

T32_OK = 0
EXIT_FAILURE = 1

PATH_TO_T32 = find_executable('t32marm-qt')

# We can't continue without a t32 executable
assert PATH_TO_T32, "no Trace32 executable on path"

PATH_TO_T32_HOME = os.path.join(os.path.dirname(PATH_TO_T32), '../../')
PATH_TO_T32_PY_API = os.path.join(PATH_TO_T32_HOME, 'demo/api/python')

# auto-detect the correct library
if (platform.system() == 'Windows') or (platform.system()[0:6] == 'CYGWIN'):
    if ctypes.sizeof(ctypes.c_voidp) == 4:
        # WINDOWS 32bit
        t32 = ctypes.cdll.t32api
    else:
        # WINDOWS 64bit
        t32 = ctypes.cdll.t32api64
elif platform.system() == 'Darwin':
    # Mac OS X
    t32 = ctypes.CDLL(os.path.join(PATH_TO_T32_PY_API,
                                   "./t32api.dylib"))
else:
    if ctypes.sizeof(ctypes.c_voidp) == 4:
        # Linux 32bit
        t32 = ctypes.CDLL(os.path.join(PATH_TO_T32_PY_API,
                                       "./t32api.so"))
    else:
        # Linux 64bit
        t32 = ctypes.CDLL(os.path.join(PATH_TO_T32_PY_API,
                                       "./t32api64.so"))


# -----------------
# -- cmd_wrapper --
# -----------------

def cmd_wrapper(cmd, wait_for_completion=False):

    t32.T32_Cmd(cmd)

    if wait_for_completion:
        state = ctypes.c_int()
        # Waiting for command to finish
        while True:
            t32.T32_GetPracticeState(ctypes.byref(state))

            if state.value == 0:
                break
            time.sleep(0.5)


# -------------
# -- connect --
# -------------

def connect(node="localhost", port=20000, packlen=1024):
    t32.T32_Config(b"NODE=", node)
    t32.T32_Config(b"PORT=", str(port))
    t32.T32_Config(b"PACKLEN=", str(packlen))

    print(' Connecting...')
    for i in range(1, 3):
        if t32.T32_Init() == T32_OK:
            if t32.T32_Attach(1) == T32_OK:
                print('Successfully established a remote connection with' +
                      ' TRACE32 PowerView.')
                break
            else:
                if i == 1:
                    print('Failed once to established a remote connection' +
                          ' with TRACE32 PowerView.')
                    t32.T32_Exit()
                elif i == 2:
                    print('Failed twice to established a remote connection' +
                          ' with TRACE32 PowerView.')
                    print('Terminating ...')
                    sys.exit(EXIT_FAILURE)
        else:
            if i == 1:
                print(' Failed once to initialize a remote connection with' +
                      ' TRACE32 PowerView.')
                t32.T32_Exit()
            elif i == 2:
                print(' Failed twice to initialize a remote connection with' +
                      ' TRACE32 PowerView.')
                print(' Terminating ...')
                sys.exit(EXIT_FAILURE)


# -----------------
# -- basic_setup --
# -----------------

def basic_setup():
    cmd_wrapper(b"RESet")
    cmd_wrapper(b"SYStem.RESet")
    cmd_wrapper(b"SYStem.CPU STM32F756ZG")
    cmd_wrapper(b"SYStem.CONFIG.DEBUGPORTTYPE SWD")
    cmd_wrapper(b"SYStem.CONFIG.CONNECTOR MIPI20T")
    cmd_wrapper(b"SYStem.MemAccess DAP")
    cmd_wrapper(b"SYStem.Option DUALPORT ON")
    cmd_wrapper(b"SYStem.Up")


# ------------------------
# -- init_trace_stm32f7 --
# ------------------------

def init_trace_stm32f7():
    # initialize Offchip-Trace
    # DBGMCU_CR
    cmd_wrapper(b"Data.Set E:0xe0042004 %Long Data.Long(E:0xe0042004)|0xe0")

    # set PinMux for PE2-6 -AF0",

    # RCC_AHB1RST
    cmd_wrapper(b"Data.Set E:0x40023810 %Long Data.Long(E:0x40023810)&~0x10")

    # RCC_AHB1ENR
    cmd_wrapper(b"Data.Set E:0x40023830 %Long Data.Long(E:0x40023830)|0x10")

    # GPIOE_MODER
    cmd_wrapper(b"Data.Set E:0x40021000 %Long" +
                " Data.Long(E:0x40021000)|0x00002aa0")

    # GPIOE_OSPEEDR
    cmd_wrapper(b"Data.Set E:0x40021008 %Long" +
                " Data.Long(E:0x40021008)|0x00003ff0")

    # GPIOE_AFRL
    cmd_wrapper(b"Data.Set E:0x40021020 %Long" +
                " Data.Long(E:0x40021020)&0xf00000ff")

    cmd_wrapper(b"TPIU.PortSize 4", wait_for_completion=True)
    cmd_wrapper(b"ETM.Trace ON", wait_for_completion=True)
    cmd_wrapper(b"ETM.ON", wait_for_completion=True)


# ---------------------
# -- load_executable --
# ---------------------

def load_executable(path):

    # The old breakpoints don't make sense with a new executable so we remove
    # them all.
    clear_breakpoints()

    cmd_wrapper(b"Data.Load.auto \"%s\"" % path, wait_for_completion=True)


# -------------------
# -- get_cpu_state --
# -------------------

def get_cpu_state():
    systemstate = ctypes.c_uint(0)
    retval = t32.T32_GetState(ctypes.byref(systemstate))
    if (retval == T32_OK):
        states = ["down", "halted", "stopped", "running"]

        # Safeguard the little trick
        systemstate.value = (systemstate.value) & 0x3

        print "CPU current state:" + states[systemstate.value]
        return states[systemstate.value]
    else:
        return "error"


# ---------------
# -- run_until --
# ---------------

def run_until(symbol, timeout_sec):

    # FIXME: It seems like we have to first do at least one step before using
    # go.direct. I don't know why at this point.
    cmd_wrapper("Step.Single 1", wait_for_completion=True)

    cmd_wrapper(b"go.direct %s" % symbol)

    # Wait for the CPU to stop or timeout
    cmd_wrapper(b"WAIT !STATE.RUN() %s.s" % str(timeout_sec),
                wait_for_completion=True)

    if get_cpu_state() == "running":
        # Stop the CPU
        cmd_wrapper(b"Break.direct")
        print "!!! TIMEOUT !!!"


# --------------------
# -- set_breakpoint --
# --------------------

def set_breakpoint(symbol):
    T32_MEMORY_ACCESS_PROGRAM = 0x1
    T32_BPCONFIG_PROGRAM = 0x001
    addr = ctypes.c_uint32(get_symbol_address(symbol))
    return t32.T32_WriteBreakpoint(addr, T32_MEMORY_ACCESS_PROGRAM,
                                   T32_BPCONFIG_PROGRAM, 1)


# ------------------------
# -- get_symbol_address --
# ------------------------

def get_symbol_address(symbol_name):

    addr = ctypes.c_uint32(0)
    size = ctypes.c_uint32(0)
    access = ctypes.c_uint32(0)

    ret = t32.T32_GetSymbol(symbol_name,
                            ctypes.byref(addr),
                            ctypes.byref(size),
                            ctypes.byref(access))

    if ret == T32_OK:
        return addr.value
    else:
        return -1


# ---------------------------
# -- read_register_by_name --
# ---------------------------

def read_register_by_name(name):
    upper = ctypes.c_uint32(0)
    lower = ctypes.c_uint32(0)
    ret = t32.T32_ReadRegisterByName(name,
                                     ctypes.byref(lower),
                                     ctypes.byref(upper))
    if ret == T32_OK:
        return upper.value * 2**32 + lower.value
    else:
        return -1


# ------------------
# -- export_trace --
# ------------------

def export_trace(path):
    cmd_wrapper(b"Trace.FLOWPROCESS", wait_for_completion=True)
    cmd_wrapper(b"Trace.export.BranchFlow %s /NOSYMBOL /CALLER" % path,
                wait_for_completion=True)


# ---------------------------
# -- CPU_stopped_at_symbol --
# ---------------------------

def CPU_stopped_at_symbol(symbol):
    sym_addr = get_symbol_address(symbol)
    pc_addr = read_register_by_name("PC")

    return pc_addr == sym_addr


# -----------------------
# -- clear_breakpoints --
# -----------------------

def clear_breakpoints():
    cmd_wrapper(b"Break.Reset")


# ------------------
# -- kill_trace32 --
# ------------------

def kill_trace32():
    connect()
    t32.T32_Terminate(0)
