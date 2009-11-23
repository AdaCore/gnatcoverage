/*
 * QEMU System Emulator
 *
 * Copyright (C) 2009, AdaCore
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "cpu.h"
#include "exec-all.h"

/* Set TRACE_OP_HIST_CACHE and maybe TRACE_OP_HIST_SET.  */
void tracefile_history_for_tb_search (TranslationBlock *);

/* True when full history is desired, either for all instructions or for
   the conditional jump instruction at the end of the tb.  */
static inline int tracefile_history_for_tb (TranslationBlock *tb) {
    if (!tb->tflags & TRACE_OP_HIST_CACHE)
        tracefile_history_for_tb_search (tb);
    return tb->tflags & TRACE_OP_HIST_CACHE;
}
