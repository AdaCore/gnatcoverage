#define PACKAGE "gnatcoverage"
#define PACKAGE_VERSION 1.0

#include <bfd.h>
#include <stdio.h>

#define CHECK(expr, call) do { if (!(expr)) { (*warn_cb)(call " failed", bfd_errmsg(bfd_get_error())); return; } } while (0)

void
_gnatcov_for_each_synthetic_symbol(char *fn, void (*cb)(const char *, long unsigned int), void (*warn_cb)(const char *, const char *))
{
  bfd *abfd;
  asymbol **dyn_syms = NULL, *synthsyms = NULL;
  long dyn_count = 0;
  long synth_count;
  int storage;
  int j;
  symbol_info si;

  abfd = bfd_openr(fn, "default");
  CHECK(abfd != NULL, "bfd_openr");

  bfd_check_format(abfd, bfd_object);

  storage = bfd_get_dynamic_symtab_upper_bound(abfd);
  CHECK(storage >= 0, "bfd_get_dynamic_symtab_upper_bound");

  if (storage == 0 ||
      bfd_get_dynamic_reloc_upper_bound (abfd) == 0)
    /* No dynamic symbols/relocations, nothing to do.  */
    return;

  dyn_syms = (asymbol **) bfd_alloc(abfd, storage);
  CHECK(dyn_syms != NULL, "bfd_alloc");

  dyn_count = bfd_canonicalize_dynamic_symtab(abfd, dyn_syms);
  CHECK(dyn_count > 0, "bfd_canonicalize_dynamic_symtab");

  synth_count = bfd_get_synthetic_symtab
                  (abfd, /*static_count*/ 0, /*static_syms*/ NULL,
                   dyn_count, dyn_syms, &synthsyms);

  /* Note: synth_count is either -1 or >0, never 0.  There is no way
     distinguish an error condition from the mere absence of synthetic
     symbols.  */

  for (j = 0; j < synth_count; j++) {
    bfd_symbol_info(&synthsyms[j], &si);
    (*cb) (si.name, si.value);
  }

  bfd_close(abfd);
}

#ifdef BFD_HELPERS_TEST
void print_cb(const char *name, unsigned long int val) {
  printf("%016lx %s\n", val, name);
}

int main(int argc, char **argv) {
  bfd_init();
  _gnatcov_for_each_synthetic_symbol(argv[1], print_cb);
}
#endif
