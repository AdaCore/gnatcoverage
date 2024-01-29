package System is
   pragma Pure;
   pragma No_Elaboration_Code_All;

   subtype Name is String;
   System_Name : constant Name := "foo";

   --  System-Dependent Named Numbers

   Min_Int             : constant := -2 ** (Standard'Max_Integer_Size - 1);
   Max_Int             : constant :=  2 ** (Standard'Max_Integer_Size - 1) - 1;

   Max_Binary_Modulus    : constant := 2 ** Standard'Max_Integer_Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := Standard'Max_Integer_Size - 1;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 0.000_001;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := Standard'Word_Size;
   Memory_Size  : constant := 2 ** Word_Size;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order :=
                         Bit_Order'Val (Standard'Default_Bit_Order);
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   --  If the scheduling policy is SCHED_FIFO or SCHED_RR the runtime makes use
   --  of the entire range provided by the system.
   --
   --  If the scheduling policy is SCHED_OTHER the only valid system priority
   --  is 1 and other values are simply ignored.

   Max_Priority           : constant Positive := 62;
   Max_Interrupt_Priority : constant Positive := 63;

   subtype Any_Priority       is Integer      range  1 .. 63;
   subtype Priority           is Any_Priority range  1 .. 62;
   subtype Interrupt_Priority is Any_Priority range 63 .. 63;

   Default_Priority : constant Priority := 10;

private

   type Address is mod Memory_Size;
   for Address'Size use Standard'Address_Size;

   Null_Address : constant Address := 0;

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.

   Backend_Divide_Checks     : constant Boolean := False;
   Backend_Overflow_Checks   : constant Boolean := True;
   Command_Line_Args         : constant Boolean := True;
   Configurable_Run_Time     : constant Boolean := True;
   Denorm                    : constant Boolean := True;
   Duration_32_Bits          : constant Boolean := False;
   Exit_Status_Supported     : constant Boolean := True;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := False;
   Stack_Check_Probes        : constant Boolean := True;
   Stack_Check_Limits        : constant Boolean := False;
   Support_Aggregates        : constant Boolean := True;
   Support_Atomic_Primitives : constant Boolean := True;
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := True;
   Suppress_Standard_Library : constant Boolean := True;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   Frontend_Exceptions       : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;

end System;
