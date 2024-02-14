package body Blocks_By_Tasks is
   First_Block, Second_Block, Third_Block : Boolean := False;

   task body Processing is
   begin
      --  Block to be covered by instances of type Block1
      if Kind = Block1 then    -- # blk1_tst
	 First_Block := True;  -- # blk1_stmt
      end if;

      --  Block to be covered by instances of type Block2
      if Kind = Block2 then    -- # blk2_tst
	 Second_Block := True; -- # blk2_stmt
      end if;

      --  Block to be covered by instances of type Block3, but none
      --  created in this test case.
      if Kind = Block3 then    -- # blk3_tst
	 Third_Block := True;  -- # blk3_stmt
      end if;
   end Processing;
end Blocks_By_Tasks;
