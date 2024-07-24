extern void stub_puts (const char *);

/* clang-format off */
int
main (void)
{
stub_puts ("Hello world");
/* GNATCOV_DUMP_BUFFERS */
return 0;
}
/* clang-format on */
