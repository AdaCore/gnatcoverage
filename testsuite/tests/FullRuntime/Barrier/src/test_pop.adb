with Buffer;

procedure Test_Pop is
   Buf : Buffer.Buffer;
   V : Integer;
begin
   select
      Buf.Pop (V, False);
   else
      null;
   end select;
end Test_Pop;

--# buffer.adb

--%cov: --level=stmt
-- =/push_guard/     l. ## 0
-- =/push_do/        l- ## s-
-- =/push_test_tell/ l- ## s-
-- =/push_tell/      l- ## s-
-- =/pop_guard/      l. ## 0
-- =/pop_do/         l- ## s-
-- =/pop_tell/       l- ## s-
-- =/pop_test_tell/  l- ## s-

--%cov: --level=stmt\+decision
-- =/push_guard/     l- ## d-
-- =/push_do/        l- ## s-
-- =/push_test_tell/ l- ## s-
-- =/push_tell/      l- ## s-
-- =/pop_guard/      l! ## dT-
-- =/pop_do/         l- ## s-
-- =/pop_tell/       l- ## s-
-- =/pop_test_tell/  l- ## s-

--# buffer.ads

-- =/component_decl/ l+ ## 0
