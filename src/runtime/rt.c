#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>

#define PRIMARY_SHIFT   0x02
#define PRIMARY_MASK    0x03
#define FX_TAG          0x00
#define PTR_TAG         0x01
#define CLOSURE_TAG     0x02
#define IMM_TAG         0x03

#define CHAR_TAG        0x0f

#define SECONDARY_SHIFT 0x03
#define SECONDARY_MASK  0x07
#define VECTOR_TAG      0x00
#define STRING_TAG      0x01
#define SYMBOL_TAG      0x02
#define BYTEVECTOR_TAG  0x03
#define TEMPLATE_TAG    0x04
#define PAIR_TAG        0x05
#define INSTANCE_TAG    0x06
#define FORWARD_TAG     0x07

#define WORDSIZE       4

typedef uintptr_t obj_t ;
typedef uintptr_t header_t ;

/* Object layout
 * 
 * Primary tag
 *
 * 00000000000000 000000 00 -> integers
 * 00000000000000 000000 01 -> pointers
 * 00000000000000 000000 10 -> closures
 * 00000000000000 000000 11 -> immediate
 *
 * Secondary tag
 *
 * 00000000000000 00000 000 -> vector
 * 00000000000000 00000 001 -> string
 * 00000000000000 00000 010 -> symbol
 * 00000000000000 00000 011 -> byte-vector
 * 00000000000000 00000 100 -> template
 * 00000000000000 00000 101 -> pair
 * 00000000000000 00000 110 -> instance
 * 00000000000000 00000 111 -> forward
 */

#define MAKE_IMMEDIATE(code) ((obj_t) (((code) << 4) + 15))
#define OBJ_TRUE       MAKE_IMMEDIATE(3)
#define OBJ_FALSE      MAKE_IMMEDIATE(2)
#define OBJ_UNSPECIFIC MAKE_IMMEDIATE(6)
#define OBJ_UNBOUND    MAKE_IMMEDIATE(7)
#define OBJ_NIL        MAKE_IMMEDIATE(4)
#define OBJ_EOF        MAKE_IMMEDIATE(5)

#define MAKE_NUMBER(n) ((obj_t) (n<<2))
#define NUMBER(n) (((intptr_t) n) / 4)
#define NUMBERP(n) (((n) & PRIMARY_MASK) == FX_TAG)

#define MAKE_CHAR(ch) ((obj_t) (((ch) << 8) + CHAR_TAG))
#define CHAR(ch) ((ch) >> 8)
#define CHARP(x) (((x) & 0xff) == CHAR_TAG)

#define CLOSUREP(x) (((x) & PRIMARY_MASK) == CLOSURE_TAG)
#define PTRP(x) (((x) & PRIMARY_MASK) == PTR_TAG)

static obj_t *alloc(size_t) ;

typedef enum { code_vector, code_string, code_symbol, code_byte_vector,
	       code_template, code_pair, code_instance, code_forward }
  header_code_t ;

header_t make_header(uint32_t len, uint8_t code) {
  return (len << SECONDARY_SHIFT) | code ;
}

header_code_t header_code(header_t h) {
  return h & SECONDARY_MASK ;
}

size_t header_size(header_t h) {
  return h >> SECONDARY_SHIFT ;
}

obj_t deref(obj_t ptr, uintptr_t offset) { 
  return ((obj_t *)(ptr & ~PRIMARY_MASK))[offset] ;
}

typedef struct {
  header_t header ;
  obj_t closed[] ;
} closure_t ;

uintptr_t closurep(obj_t o) {
  return (o & PRIMARY_MASK == CLOSURE_TAG) ;
}

closure_t *make_closure(size_t len) {
  closure_t *closure = (closure_t *) alloc(sizeof(closure_t)+len*sizeof(obj_t)) ;
  closure->header = make_header(len, 0) ;
  return closure ;
}

typedef struct {
  header_t header ;
  obj_t debug ;
  char code[] ;
} template_t ;

uintptr_t templatep(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o,0)) == code_template) ;
}

template_t *make_template(size_t len) {
  template_t *template = (template_t *) alloc(sizeof(template_t)+len) ;
  template->header = make_header(len, code_template) ;
  return template ;
}
  
typedef struct {
  header_t header ;
  size_t length ;
  char ch[] ;
} symbol_t;

uintptr_t symbolp(obj_t o) {
  return (o & 0x3 == PTR_TAG) && (header_code(deref(o, 0)) == code_symbol) ;
}

symbol_t *make_symbol(uint32_t len) {
  symbol_t *symbol = (symbol_t *) alloc(sizeof(symbol_t)+len) ;
  symbol->header = make_header(len, code_symbol) ;
  return symbol ;
}

typedef struct {
  header_t header ;
  size_t   length ;
  char ch[] ;
} string_t ;

uintptr_t stringp(obj_t o) {
  return (o & 0x3 == PTR_TAG) && (header_code(deref(o, 0)) == code_string) ;
}

string_t *make_string(size_t len) {
  string_t *str = (string_t *) alloc(sizeof(string_t)+len) ;
  str->header = make_header(len, code_string) ;
  return str ;
}

typedef struct {
  header_t header ;
  obj_t head ;
  obj_t tail ;
} pair_t ;

uintptr_t pairp(obj_t o) {
  return (o & 0x3 == PTR_TAG) && (header_code(deref(o, 0)) == code_pair) ;
}

pair_t *make_pair() {
  pair_t *pair = (pair_t *) alloc(sizeof(pair_t)) ;
  pair->header = make_header(0, code_pair) ;
  return pair ;
}

typedef struct {
  header_t header ;
  obj_t value[] ;
} vector_t ;

uintptr_t vectorp(obj_t o) {
  return (o & 0x3 == PTR_TAG) && (header_code(deref(o, 0)) == code_vector) ;
}

vector_t *make_vector(size_t len) {
  vector_t *v = (vector_t *) alloc(sizeof(vector_t)+sizeof(obj_t)*len) ;
  v->header = make_header(len, code_vector) ;
  return v ;
}

obj_t tag(void *ptr, uint8_t tag) {return (((obj_t) ptr))+tag ; }
obj_t *untag(obj_t obj, uint8_t tag) { return (obj_t *) ((obj - tag)) ; }

typedef struct {
  void* eax ; /* scratch */
  void* ebx ; /* preserve */
  void* ecx ; /* scratch */
  void* edx ; /* scratch */
  void* esi ; /* preserve */
  void* edi ; /* preserve */
  void* ebp ; /* preserve */
  void* esp ; /* preserve */

  void* entry_arity_error ;
  void* entry_value_error ;
  void* entry_unbound_error ;
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
  /* char *p ; */
  /* int rc = posix_memalign(&p, 8, aligned_size) ; */
  if (p == MAP_FAILED) {
    perror("could not allocate memory") ; 
    _exit(1) ;
  }

  if (((obj_t) (p)) & 0x07 != 0) {
    printf("Allocation not aligned\n") ; _exit(1) ;
  }
    
  status = mprotect(p, page, PROT_NONE) ;
  if (status != 0) {
    perror("could not protect begin of allocated memory") ;
    
    _exit(1) ;
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE) ;
  if (status != 0) {
    printf("could not protect begin of allocated memory") ; _exit(1) ;
  }
  return p+page;
}

static obj_t *alloc(size_t size) {
  if (heap_free+size < heap_limit) {
    obj_t *obj = (obj_t *) heap_free ;
    heap_free += ((size+7) / 8) * 8 ;
    return (obj_t *) obj ;
  } else {
    printf("HEAP ALLOCATION ERROR FOR %d", size) ; _exit(1) ;
  }
}

static void print_scm_obj(obj_t x) ;

static void print_scm_vector(obj_t x)
{
  printf("#(") ;
  size_t size = header_size(deref(x, 0)) ;
  uintptr_t i ;

  for (i = 1 ; i <= size ; i++) {
    print_scm_obj(deref(x, i)) ;
    if (i < size)
      printf(" ") ;
  }

  printf(")") ;
}

static void print_scm_pair(obj_t x)
{
  printf("(") ;
  obj_t pair = x;
  while (pair != OBJ_NIL)
    {
      if (PTRP(pair) && header_code(deref(pair, 0)) == code_pair)
	{
	  obj_t p = deref(pair, 1) ;
	  print_scm_obj(p) ;
	  pair = deref(pair,2) ;
	  if (pair != OBJ_NIL) {
	    printf(" ") ;
	  }
	}
      else
	{
	  print_scm_obj(pair) ;
	  break ;
	}
    }
  
  printf(")") ;
}

static void print_secondary(obj_t x) {
  uint8_t code = header_code(deref(x,0)) ;

  switch (code) {
  case code_vector:
    print_scm_vector(x) ;
    break ;
  case code_string:
    printf("\"%s\"", (char *) ((obj_t *)(x & ~PRIMARY_MASK)+2)) ;
    break ;
  case code_symbol:
    printf("'%s", (char *) ((obj_t *)(x & ~PRIMARY_MASK)+2)) ;
    break ;
  case code_byte_vector:
    printf("#[...]") ;
    break;
  case code_template:
    printf("#{template}") ;
    break;
  case code_pair:
    print_scm_pair(x) ;
    break;
  case code_instance:
    printf("#{instance}") ;
    break;
  case code_forward:
    printf("#{forward}") ;
    break;
  }
}

static void print_scm_obj(obj_t x)
{
  if (NUMBERP(x)) { printf("%d", (NUMBER(x))) ; }
  else if (CHARP(x)) { printf("#\\%c", CHAR(x)) ; }
  else if (CLOSUREP(x)) { printf("#{closure}", x) ; }
  else if (PTRP(x)) { print_secondary(x) ; }
  else if (x == OBJ_TRUE) { printf("#t") ; }
  else if (x == OBJ_FALSE) { printf("#f") ; }
  else if (x == OBJ_EOF) { printf("#!eof") ; }
  else if (x == OBJ_UNSPECIFIC) { printf("#!unspecific") ; }
  else if (x == OBJ_UNBOUND) { printf("#!unbound") ; }
  else if (x == OBJ_NIL) { printf("()") ; }
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

  obj_t result = scheme_entry(&ctx, (obj_t) untag(closure, CLOSURE_TAG),
			      stack_base-WORDSIZE, heap_free) ;
  print_scm_obj(result) ;

  return 0 ;
}

/**
 * FASL reading
 */
typedef enum { fop_template, fop_ref, fop_number, fop_closure, fop_symbol,
	       fop_string, fop_null, fop_unbound, fop_unspecific, fop_vector,
               fop_true, fop_false, fop_pair, fop_char}
 file_operation_t ;

obj_t read_template(int);
obj_t read_ref(int);
obj_t read_number(int);
obj_t read_closure(int);
obj_t read_symbol(int);
obj_t read_string(int);
obj_t read_vector(int);
obj_t read_pair(int);
obj_t read_char(int);

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
  case fop_string: return read_string(fd) ;
  case fop_null: return OBJ_NIL ;
  case fop_unbound: return OBJ_UNBOUND ;
  case fop_unspecific: return OBJ_UNSPECIFIC ;
  case fop_vector: return read_vector(fd) ;
  case fop_true: return OBJ_TRUE ;
  case fop_false: return OBJ_FALSE ;
  case fop_pair: return read_pair(fd) ;
  case fop_char: return read_char(fd) ;
  default: return 0 ;
  }
}

obj_t read_vector(int fd) {
  uint32_t len = read_u32(fd) ;
  vector_t *vec = make_vector(len) ;

  intptr_t i ;
  for (i = 0 ; i < len ; i++) {
    obj_t o = read_fasl(fd) ;
    vec->value[i] = o ;
  }
  return tag(vec, PTR_TAG) ;
}

obj_t read_pair(int fd) {
  obj_t head = read_fasl(fd) ;
  obj_t tail = read_fasl(fd) ;

  pair_t *pair = make_pair() ;
  pair->head = head ;
  pair->tail = tail ;
  return tag(pair, PTR_TAG) ;
}

obj_t read_char(int fd) {
  uint8_t ch = read_u8(fd) ;
  return MAKE_CHAR(ch) ;
}

obj_t read_template(int fd)
{
  uint32_t len = read_u32(fd) ;
  template_t *template = make_template(len) ;

  template->debug = OBJ_FALSE ;

  int i ;
  for (i = 0 ; i < len ; i++) {
    uint8_t byte = read_u8(fd) ;
    template->code[i] = byte ;
  }

  /* printf("template @ %08x:", template) ; */
  /* for (i = 0 ; i < len ; i++) { */
  /*   printf("%02x ", template->code[i]) ; */
  /* } */
  /* printf("\n") ; */

  return tag(template, PTR_TAG) ;
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

  template_t *template = (template_t *) untag(read_fasl(fd), PTR_TAG) ;
  closure->closed[0] = (obj_t) &template->code ;
  
  int i ;
  for (i = 1 ; i < n ; i++) {
    closure->closed[i] = read_fasl(fd) ;
  }
  
  return tag(closure, CLOSURE_TAG) ;
}

static obj_t symbols = OBJ_NIL ;

obj_t intern(obj_t symbol) {
  symbol_t *sym = (symbol_t *) (symbol & ~PRIMARY_MASK) ;
  obj_t s ;

  for (s=symbols; s != OBJ_NIL ; s = deref(s, 2))
    {
      
      symbol_t *ss = (symbol_t *)((pair_t *) (s & ~PRIMARY_MASK))->head ;
      if (ss->length == sym->length &&
	  strncmp((const char *) &ss->ch,
		  (const char *) &sym->ch, ss->length) == 0)
	return ((pair_t *) (s & PRIMARY_MASK))->head ;
    }
  
  pair_t *p = make_pair() ;
  p->head = symbol ;
  p->tail = symbols ;

  return symbol ;
}

obj_t read_symbol(int fd)
{
  size_t len = read_u32(fd) ;
  int i ;

  symbol_t *symbol = make_symbol(len) ;
  
  for (i = 0 ; i < len ; i++)
    symbol->ch[i] = read_u8(fd) ;
  
  return tag(symbol, PTR_TAG) ;
}

static obj_t refs = OBJ_NIL ;

obj_t read_ref(int fd) {
  obj_t sym = read_symbol(fd) ;
  /* print_scm_obj(sym) ; */

  sym = intern(sym) ;
  /* print_scm_obj(sym) ; */
  obj_t ref ;
  for (ref = refs ; ref != OBJ_NIL && deref(deref(ref, 1),1) != sym ;
       ref = deref(ref, 2)) ;
  if (ref == OBJ_NIL)
    {
      pair_t *entry = make_pair() ;
      pair_t *link = make_pair() ;
      entry->head = sym ;
      entry->tail = OBJ_UNBOUND ;

      link->head = tag(entry, PTR_TAG) ;
      link->tail = refs ;
      refs = tag(link, PTR_TAG) ;

      return tag(entry, PTR_TAG) ;
    }

  return deref(ref, 1) ;
}

obj_t read_string(int fd)
{
  size_t len = read_u32(fd) ;
  int i ;

  string_t *str = make_string(len) ;

  for (i = 0 ; i < len ; i++)
    str->ch[i] = read_u8(fd) ;

  return tag(str, PTR_TAG) ;
}

