#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>

#define FX_MASK 0x03
#define FX_TAG  0x00
#define FX_SHIFT 2

#define PAIR_MASK      0x07
#define PAIR_TAG       0x01
#define PAIR_SHIFT     3

#define CLOSURE_MASK   0x07
#define CLOSURE_TAG    0x02
#define CLOSURE_SHIFT  3

#define SYMBOL_MASK    0x07
#define SYMBOL_TAG     0x03
#define SYMBOL_SHIFT   3

#define VECTOR_MASK    0x07
#define VECTOR_TAG     0x05
#define VECTOR_SHIFT   3

#define STRING_MASK    0x07
#define STRING_TAG     0x06
#define STRING_SHIFT   3

#define IMM_MASK       0x07
#define IMM_TAG        0x07
#define IMM_SHIFT      3

#define SCM_TRUE       0x6f
#define SCM_FALSE      0x2f
#define SCM_NIL        0b01001111
#define SCM_EOF        0b01011111
#define SCM_UNSPECIFIC 0b01101111
#define SCM_UNBOUND    0b01111111

#define WORDSIZE       4

typedef struct {
  void* eax ; /* scratch */
  void* ebx ; /* preserve */
  void* ecx ; /* scratch */
  void* edx ; /* scratch */
  void* esi ; /* preserve */
  void* edi ; /* preserve */
  void* ebp ; /* preserve */
  void* esp ; /* preserve */
} context_t ;

static char *heap_new ;
static char *heap_old ;

static char* alloc_protected_space(int size)
{
  int page = getpagesize() ;
  int status ;
  int aligned_size = ((size + page - 1) / page) * page ;
  char *p = mmap(0, aligned_size+2*page, PROT_READ|PROT_WRITE,
		 MAP_ANONYMOUS | MAP_PRIVATE,
		 0,0) ;
  if (p == MAP_FAILED) { printf("could not allocate memory") ; _exit(1) ; }
  status = mprotect(p, page, PROT_NONE) ;
  if (status != 0) { printf("could not protect begin of allocated memory") ; _exit(1) ; }
  status = mprotect(p + page + aligned_size, page, PROT_NONE) ;
  if (status != 0) { printf("could not protect begin of allocated memory") ; _exit(1) ; }
  return p+page ;
}

static void print_scm_obj(unsigned int x) ;

static void print_scm_vector(unsigned int x)
{
  printf("#(") ;
  int size = *((unsigned int *) (x-VECTOR_TAG)) ;
  int i ;

  for (i = 1 ; i <= size ; i++)
    print_scm_obj(((unsigned int *) (x-VECTOR_TAG))[i]) ;

  printf(")") ;
}

static void print_scm_pair(unsigned int x)
{
  printf("(") ;
  unsigned int *pair = (unsigned int *) (x) ;
  while (pair != (unsigned int *) SCM_NIL)
    {
      if ((((unsigned int) pair) & PAIR_MASK) == PAIR_TAG)
	{
	  unsigned int *p = (unsigned int *) (((unsigned int) pair) - PAIR_TAG) ;
	  print_scm_obj(*p) ;
	  printf(" ") ;
	  pair = (unsigned int *) p[1] ;
	}
      else
	{
	  print_scm_obj((unsigned int) pair) ;
	  break ;
	}
    }
  
  printf(")") ;
}

static void print_scm_obj(unsigned int x)
{
  if ((x & FX_MASK) == FX_TAG) { printf("%d", ((int) x) >> FX_SHIFT) ; }
  else if ((x & PAIR_MASK) == PAIR_TAG) { print_scm_pair(x) ; }
  else if ((x & CLOSURE_MASK) == CLOSURE_TAG) { printf("#{closure 0x%08x", x) ; }
  else if ((x & SYMBOL_MASK) == SYMBOL_TAG) { printf("%s", (char *) (x-SYMBOL_TAG+WORDSIZE)) ; }
  else if ((x & VECTOR_MASK) == VECTOR_TAG) { print_scm_vector(x) ; }
  else if ((x & STRING_MASK) == STRING_TAG) { printf("\"%s\"", (char *) (x-STRING_TAG+WORDSIZE)) ; }
  else if (x == SCM_TRUE) { printf("#t") ; }
  else if (x == SCM_FALSE) { printf("#f") ; }
  else if (x == SCM_EOF) { printf("#!eof") ; }
  else if (x == SCM_UNSPECIFIC) { printf("#!unspecific") ; }
  else if (x == SCM_UNBOUND) { printf("#!unbound") ; }
  else printf("#{unknown 0x%08x}", x) ;
}

extern unsigned int scheme_entry(context_t *, char *, char *) ;

int main(int ac, char *av[])
{
  /* Allocation of Scheme stack space */
  int stack_size = 16 * 4096 ;
  char *stack_top = alloc_protected_space(stack_size) ;
  char *stack_base = stack_top + stack_size ;

  heap_new = alloc_protected_space(16*4096) ;
  heap_old = alloc_protected_space(16*4096) ;

  context_t ctx ;

  print_scm_obj(scheme_entry(&ctx, stack_base, heap_new)) ;

  return 0 ;
}
