#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>

#include "fasl.h"
#include "obj.h"
#include "debug.h"

extern unsigned int scheme_entry(context_t *, obj_t, char *, char *) ;

int main(int ac, char *av[])
{
  heap_init() ;

  context_t ctx ;
  ctx.entry_arity_error = 0;
  ctx.entry_value_error = 0;
  ctx.entry_unbound_error = 0;
  ctx.entry_print = print_scm_obj;

  int fd = open(av[1], O_RDONLY) ;
  obj_t closure = read_fasl(fd) ;

  obj_t result = scheme_entry(&ctx, (obj_t) untag(closure, CLOSURE_TAG),
			      stack_base-WORDSIZE, heap_free) ;
  print_scm_obj(result) ;

  return 0 ;
}
