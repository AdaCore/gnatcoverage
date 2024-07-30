# Design documentation of block instrumentation

Block instrumentation (when using the `--instrument-block` instrument switch)
consists in dividing the code in *statement blocks* that execute altogether.
Instead of having a bit and a call setting this bit to monitor the execution of
a statement SCO, gnatcov instruments a block of statements as a whole, resulting
in having one bit and one call for the N statements constitutive of the block.
This makes the instrumented code more efficient by lessening the memory
footprint and the executable size. We deliberately choose to always instrument
the last statement of the block for soundness purposes. Indeed, if the execution
aborts before the end of the block, the tool will report violations over all of
the statements of the block even though some of them may have been covered.

Uninstrumented code:
```Ada
procedure P is
begin
   Put_Line ("Hello");
   Put_Line ("world");
end P;
```

Without --instrument-block:

```Ada
procedure P is
begin
   Witness (Xcov_Stmt_Buffers, 1);
   Put_Line ("Hello");
   Witness (Xcov_Stmt_Buffers, 2);
   Put_Line ("world");
end P;
```

With --instrument-block:

```Ada
procedure P is
begin
   Put_Line ("Hello");
   Witness (Xcov_Stmt_Buffers, 1);
   Put_Line ("world");
end P;
```

The implementation is done at multiple levels:

## Implementation at the instrumentation step

First, let's focus on the instrumentation side. The implementation is similar
for Ada, and C/C++. Knowing what is a block requires semantic knowledge of the
code, and specifically knowing what are the control flow entry points (e.g.
labels) and splitting points (e.g. conditional branches). Any code construct
checking one of the two criteria ends the current statement block. Refer to the
End_Statement_Block (ESB) subprogram in Instrument.Ada_Unit and Instrument.C.

We keep track of the blocks by using a block stack (refer to
Ada\_Unit\_Inst\_Context.Block\_Stack). Block cannot nest in the control flow,
but they can in the source. Let's take a look at the following:

```Ada
procedure P is            --  Start_Statement_Block (SSB),
                          --     Block_Stack: [B0]
   I : Integer := 0;      --  SCO#0, {B0: [SCO#0]}
  procedure P_Nested is  --  SSB, Block_Stack: [B0, B1]
   begin
      null;               --  SCO#1, {B0: [SCO#0], B1: [SCO#1]}
   end P_Nested;          --  End_Statement_Block (ESB),
                          --     Block_Stack: [B0]
begin
   null;                  --  SCO#2, {B0: [SCO#0, SCO#2], B1: [SCO#1]}
end P;                    --  ESB, Block_Stack: []
```

We can see that the declaration and the body of P belong to the same block,
which nests Block 1 (that consists of P\_Nested body). Thus, in addition to the
End\_Statement\_Block subprogram, we introduce a Start\_Statement\_Block (SSB)
to add a new block entry into the stack. We could technically put the
declaration of I and the body of P in different blocks to avoid this kind of
complexity, but as this is a common code occurrence in Ada, it will effectively
hinder the performance impact.

Then, any control flow statement can end the current statement block, e.g.:

```Ada
procedure P (B : Boolean) --  SSB, Block_Stack: [B0]
is
   I : Integer := 0;      --  SCO#0, {B0: [SCO#0]}
begin
   if B                   --  SCO#1, {B0: [SCO#0, SCO#1]},
                          --     ESB, Block_Stack: []
   then                   --  SSB, Block_Stack: [B1]
      Put_Line (I'Image); --  SCO#2, {B0: [SCO#0, SCO#1], B1: [SCO#2]}
   end if;                --  ESB, Block_Stack: []
                          --  SSB, Block_Stack: [B2]
   I := 2;                --  SCO#3, {..., B2: [SCO#3]}
end P;                    --  ESB, Block_Stack: []
```

As we usually do not know when instrumenting the statement that this is the last
of the block, we defer the statement instrumentation to the next call to the
End\_Statement\_Block subprogram. In the block stack, we thus need to preserve
the information required to properly instrument the last encountered statement
of the block.

We then save this block information into checkpoints; in the CU_Info.Blocks
record member.

## Implementation at the coverage step

On the coverage side, gnatcov loads statement blocks from checkpoints. Refer to
SC_Obligations.

All the statement SCOs that are in a statement block are considered as coverable
(meaning they will yield coverage results): this is done in
Coverage.Source.Initialize\_SCI\_For\_Instrumented_CU. Every bit in statement
coverage buffers (in the CU\_Info.Bit\_Maps) discharge all the block statement
SCOs (rather than a single statement obligation). Refer to
Coverage.Source.Compute\_Source\_Coverage.
