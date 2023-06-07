with Buffer;

procedure Test_Pop_Push_Pop is
   Buf : Buffer.Buffer;
   V : Integer;
begin
   select
      Buf.Pop (V, False);
   else
      null;
   end select;
   V := 1;
   Buf.Push (V, True);
   V := 2;
   Buf.Pop (V, False);
   if V /= 1 then
      raise Program_Error;
   end if;
end Test_Pop_Push_Pop;

--# buffer.adb

--%cov: --level=stmt
-- =/push_guard/     l. ## 0
-- =/push_do/        l+ ## 0
-- =/push_test_tell/ l+ ## 0
-- =/push_tell/      l+ ## 0
-- =/pop_guard/      l. ## 0
-- =/pop_do/         l+ ## 0
-- =/pop_tell/       l- ## s-
-- =/pop_test_tell/  l+ ## 0

--%cov: --level=stmt\+decision
-- =/push_guard/     l. ## 0
-- =/push_do/        l+ ## 0
-- =/push_test_tell/ l! ## dF-
-- =/push_tell/      l+ ## 0
-- =/pop_guard/      l. ## 0
-- =/pop_do/         l+ ## 0
-- =/pop_tell/       l- ## s-
-- =/pop_test_tell/  l! ## dT-

--# buffer.ads

-- =/component_decl/ l+ ## 0
