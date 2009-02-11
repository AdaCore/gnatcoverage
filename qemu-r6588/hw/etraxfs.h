/*
 * QEMU ETRAX System Emulator
 *
 * Copyright (c) 2008 Edgar E. Iglesias, Axis Communications AB.
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

#include "etraxfs_dma.h"

struct etraxfs_pic
{
    qemu_irq *irq;
    qemu_irq *nmi;
    qemu_irq *guru;

    void *internal;
};

struct etraxfs_pic *etraxfs_pic_init(CPUState *env, target_phys_addr_t base);
void etraxfs_timer_init(CPUState *env, qemu_irq *irqs, qemu_irq *nmi,
                        target_phys_addr_t base);
void *etraxfs_eth_init(NICInfo *nd, CPUState *env,
                       qemu_irq *irq, target_phys_addr_t base, int phyaddr);
void etraxfs_ser_init(CPUState *env, qemu_irq *irq, CharDriverState *chr,
                      target_phys_addr_t base);
