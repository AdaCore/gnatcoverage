/****************************************************************************
 *                                                                          *
 *                              Couverture                                  *
 *                                                                          *
 *                     Copyright (C) 2008-2010, AdaCore                     *
 *                                                                          *
 * Couverture is free software; you can redistribute it  and/or modify it   *
 * under terms of the GNU General Public License as published by the Free   *
 * Software Foundation; either version 2, or (at your option) any later     *
 * version.  Couverture is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License  for more details. You  should  have  received a copy of the GNU *
 * General Public License  distributed with GNAT; see file COPYING. If not, *
 * write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, *
 * Boston, MA 02111-1307, USA.                                              *
 *                                                                          *
 ****************************************************************************/

#define UART_DATA_ADDR(ADDR) ((volatile UART_Data_Register *)(ADDR))
#define UART_STATUS_ADDR(ADDR) ((volatile UART_Status_Register *)(ADDR))
#define UART_CONTROL_ADDR(ADDR) ((volatile UART_Control_Register *)(ADDR))
#define UART_SCALER_ADDR(ADDR) ((volatile UART_Scaler_Register *)(ADDR))

typedef struct _UART_Data_Register {
  unsigned int Reserved : 24;
  unsigned char RTD;
} UART_Data_Register;

typedef struct _UART_Status_Register {
  unsigned int Reserved: 25;
  unsigned int FE : 1;
  unsigned int PE : 1;
  unsigned int OV : 1;
  unsigned int BR : 1;
  unsigned int TH : 1;
  unsigned int TS : 1;
  unsigned int DR : 1;
} UART_Status_Register;

typedef struct _UART_Control_Register {
  unsigned int Reserved: 23;
  unsigned int EC : 1;
  unsigned int LB : 1;
  unsigned int FL : 1;
  unsigned int PE : 1;
  unsigned int PS : 1;
  unsigned int TI : 1;
  unsigned int RI : 1;
  unsigned int TE : 1;
  unsigned int RE : 1;
} UART_Control_Register;

typedef struct _UART_Scaler_Register {
  unsigned int Reserved : 20;
  unsigned int UART_Scaler : 12;
} UART_Scaler_Register;

static volatile UART_Data_Register * const UART1_Data_ptr = UART_DATA_ADDR (0x80000070);

static volatile UART_Status_Register * const UART1_Status_ptr = UART_STATUS_ADDR (0x80000074);

static volatile UART_Control_Register * const UART1_Control_ptr = UART_CONTROL_ADDR (0x80000078);

static volatile UART_Scaler_Register * const UART1_Scaler_ptr = UART_SCALER_ADDR (0x8000007c);

#define UART_1_DATA_REG (*UART1_Data_ptr)
#define UART_1_STATUS_REG (*UART1_Status_ptr)
#define UART_1_CONTROL_REG (*UART1_Control_ptr)
#define UART_1_SCALER_REG (*UART1_Scaler_ptr)

int putchar(int c)
{
  UART_Data_Register UART_Tx;
  UART_Status_Register UART_Status_Aux;

  UART_Tx.RTD = c;

  do
    UART_Status_Aux = UART_1_STATUS_REG;
  while (!UART_Status_Aux.TH);
  UART_1_DATA_REG = UART_Tx;
  return c;
}

int getchar (void)
{
  UART_Data_Register UART_Rx;
  UART_Status_Register UART_Status_Aux;

  do
    UART_Status_Aux = UART_1_STATUS_REG;
  while(!UART_Status_Aux.DR);

  UART_Rx = UART_1_DATA_REG;

  return UART_Rx.RTD;
}

void abort (void)
{
  asm ("mov 0, %g1; ta 0");
}
