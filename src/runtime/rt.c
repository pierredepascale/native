#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

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

#define WORDSIZE       4

typedef intptr_t obj_t ;
typedef intptr_t header_t ;

/* Object layout
 * 00000000000000 000000 00 -> integers
 * 00000000000000 000000 01 -> pointers
 * 00000000000000 000000 10 -> immediates
 */
#define MAKE_IMMEDIATE(code) ((obj_t) (((code) << 2) + 2))
#define OBJ_TRUE       MAKE_IMMEDIATE(0)
#define OBJ_FALSE      MAKE_IMMEDIATE(1)
#define OBJ_UNSPECIFIC MAKE_IMMEDIATE(2)
#define OBJ_UNBOUND    MAKE_IMMEDIATE(3)
#define OBJ_NIL        MAKE_IMMEDIATE(4)
#define OBJ_EOF        MAKE_IMMEDIATE(5)

#define MAKE_NUMBER(n) ((obj_t) (n<<2))
#define NUMBER(n) (n>>2)

static obj_t *alloc(size_t) ;

typedef enum { code_template, code_closure, code_symbol, code_ref } header_code_t ;

header_t make_header(uint32_t len, header_code_t code) {
  return (len << 8) | code ;
}

header_code_t header_code(header_t h) {
  return h & 0xff ;
}

typedef struct {
  header_t header ;
  obj_t closed[] ;
} closure_t ;

uintptr_t closurep(obj_t o) {
  return (o & 0x3 == 1) && (header_code(o) == code_closure) ;
}

closure_t *make_closure(size_t len) {
  closure_t *closure = (closure_t *) alloc(sizeof(closure_t)+len*sizeof(obj_t)) ;
  closure->header = make_header(len, code_closure) ;
  return closure ;
}

typedef struct {
  header_t header ;
  obj_t debug ;
  char code[] ;
} template_t ;

uintptr_t templatep(obj_t o) {
  return (o & 0x3 == 1) && (header_code(o) == code_template) ;
}

template_t *make_template(size_t len) {
  template_t *template = (template_t *) alloc(sizeof(template_t)+len) ;
  template->header = make_header(len, code_template) ;
  return template ;
}
  
typedef struct {
  header_t header ;
  uint32_t length ;
  char ch[] ;
} symbol_t;

uintptr_t symbolp(obj_t o) {
  return (o & 0x3 == 1) && (header_code(o) == code_symbol) ;
}

symbol_t *make_symbol(uint32_t len) {
  symbol_t *symbol = (symbol_t *) alloc(sizeof(template_t)+len) ;
  symbol->header = make_header(len, code_symbol) ;
  return symbol ;
}

typedef struct {
  header_t header ;
  obj_t value ;
} ref_t ;

uintptr_t refp(obj_t o) {
  return (o & 0x3 == 1) && (header_code(o) == code_ref) ;
}

ref_t *make_ref(obj_t val) {
  ref_t * ref = (ref_t *) alloc(sizeof(ref_t)) ;

  ref->header = make_header(sizeof(ref_t), code_ref) ;
  ref->value = val ;

  return ref ;
}

obj_t tag(void *ptr) {
  return (((obj_t) ptr) << 2)+1 ;
}

obj_t *untag(obj_t obj) {
  return (obj_t *) ((obj - 1) >> 2) ;
}

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
static char *heap_free ;
static char *heap_limit ;

static char *heap_old ;

static char* alloc_protected_space(int size)
{
  int page = getpagesize() ;
  int status ;
  int aligned_size = ((size + page - 1) / page) * page ;
  char *p = mmap(0, aligned_size+2*page, PROT_READ|PROT_WRITE|PROT_EXEC,
		 MAP_ANONYMOUS | MAP_PRIVATE,
		 0,0) ;
  if (p == MAP_FAILED) { printf("could not allocate memory") ; _exit(1) ; }
  status = mprotect(p, page, PROT_NONE) ;
  if (status != 0) { printf("could not protect begin of allocated memory") ; _exit(1) ; }
  status = mprotect(p + page + aligned_size, page, PROT_NONE) ;
  if (status != 0) { printf("could not protect begin of allocated memory") ; _exit(1) ; }
  return p+page ;
}

static obj_t *alloc(size_t size) {
  if (heap_free+size < heap_limit) {
    obj_t *obj = (obj_t *) heap_free ;
    heap_free += size ;
    return (obj_t *) obj ;
  } else {
    printf("HEAP ALLOCATION ERROR FOR %d", size) ; _exit(1) ;
  }
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
  while (pair != (unsigned int *) OBJ_NIL)
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
  else if (x == OBJ_TRUE) { printf("#t") ; }
  else if (x == OBJ_FALSE) { printf("#f") ; }
  else if (x == OBJ_EOF) { printf("#!eof") ; }
  else if (x == OBJ_UNSPECIFIC) { printf("#!unspecific") ; }
  else if (x == OBJ_UNBOUND) { printf("#!unbound") ; }
  else if (x == OBJ_NIL) { printf("()") ; }
  else if (x == OBJ_EOF) { printf("#!eof") ; }
  else printf("#{??? 0x%08x}", x) ;
}

extern unsigned int scheme_entry(context_t *, obj_t, char *, char *) ;

obj_t read_fasl(int);

int main(int ac, char *av[])
{
  /* Allocation of Scheme stack space */
  int stack_size = 16 * 4096 ;
  char *stack_top = alloc_protected_space(stack_size) ;
  char *stack_base = stack_top + stack_size ;

  heap_new = heap_free = alloc_protected_space(16*4096) ;
  heap_limit = heap_free + 16*4096 ;
  heap_old = alloc_protected_space(16*4096) ;
 
  context_t ctx ;
  
  int fd = open(av[1], O_RDONLY) ;
  obj_t closure = read_fasl(fd) ;

  obj_t result = scheme_entry(&ctx, (obj_t) untag(closure), stack_top, heap_free) ;
  print_scm_obj(result) ;

  return 0 ;
}

/**
 * FASL reading
 */
typedef enum { fop_template, fop_ref, fop_number, fop_closure, fop_symbol,
               fop_true, fop_false, fop_unbound, fop_unspecific } file_operation_t ;

obj_t read_template(int);
obj_t read_ref(int);
obj_t read_number(int);
obj_t read_closure(int);
obj_t read_symbol(int);

uint8_t read_u8(int fd)
{
  uint8_t v ;
  if (read(fd, &v, 1) == 1) {
    return v ;
  } else {
    _exit(1) ;
  }
}

uint32_t read_u32(int fd)
{
  uint32_t v ;
  uint8_t b1 = read_u8(fd) ;
  uint8_t b2 = read_u8(fd) ;
  uint8_t b3 = read_u8(fd) ;
  uint8_t b4 = read_u8(fd) ;

  return b4 ;
}

obj_t read_fasl(int fd)
{
  uint8_t op = read_u8(fd) ;
  switch(op) {
  case fop_template: return read_template(fd) ;
  case fop_ref: return read_ref(fd) ;
  case fop_number: return read_number(fd) ;
  case fop_closure: return read_closure(fd) ;
  case fop_symbol: return read_symbol(fd) ;
  case fop_true: return OBJ_TRUE ;
  case fop_false: return OBJ_FALSE ;
  case fop_unbound: return OBJ_UNBOUND ;
  case fop_unspecific: return OBJ_UNSPECIFIC ;
  default: return 0 ;
  }
}

obj_t read_template(int fd)
{
  uint32_t len = read_u32(fd) ;
  template_t *template = make_template(len) ;

  template->debug = OBJ_FALSE ;

  printf("code @ %08x\n", &template->code) ;
  for (int i = 0 ; i < len ; i++) {
    uint8_t byte = read_u8(fd) ;
    printf("%02x ", byte) ;
    template->code[i] = byte ;
  }

  return tag(template) ;
}

obj_t read_ref(int fd)
{
  return tag(make_ref(OBJ_UNSPECIFIC)) ;
}

obj_t read_number(int fd)
{
  int8_t num = read_u8(fd)-128 ;

  return MAKE_NUMBER(num) ;
}

obj_t read_closure(int fd)
{
  uint32_t n = read_u32(fd) ;

  closure_t *closure = make_closure(n) ;

  template_t *template = (template_t *) untag(read_fasl(fd)) ;
  closure->closed[0] = (obj_t) &template->code ;

  for (int i = 1 ; i < n ; i++) {
    closure->closed[i] = read_fasl(fd) ;
  }
  
  return tag(closure) ;
}

obj_t read_symbol(int fd)
{
  uint8_t len = read_u32(fd) ;

  symbol_t *symbol = make_symbol(len) ;

  for (int i = 0 ; i < len ; i++)
    symbol->ch[i] = read_u8(fd) ;

  return tag(symbol) ;
}

