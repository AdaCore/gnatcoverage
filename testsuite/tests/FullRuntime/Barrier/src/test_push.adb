with Buffer;

procedure Test_Push is
   Buf : Buffer.Buffer;
begin
   Buf.Push (1, False);
end Test_Push;

--# buffer.adb

--%cov: --level=stmt
-- =/push_guard/     l. ## 0
-- =/push_do/        l+ ## 0
-- =/push_test_tell/ l+ ## 0
-- =/push_tell/      l- ## s-
-- =/pop_guard/      l. ## 0
-- =/pop_do/         l- ## s-
-- =/pop_tell/       l- ## s-
-- =/pop_test_tell/  l- ## s-

--%cov: --level=stmt\+decision
-- =/push_guard/     l. ## 0
-- =/push_do/        l+ ## 0
-- =/push_test_tell/ l! ## dT-
-- =/push_tell/      l- ## s-
-- =/pop_guard/      l. ## 0
-- =/pop_do/         l- ## s-
-- =/pop_test_tell/  l- ## s-

--# buffer.ads

-- =/component_decl/ l+ ## 0