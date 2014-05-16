#include <sys/mman.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#include "obj.h"

int stack_size ;
char *stack_top ;
char *stack_base ;

char *heap_new ;
char *heap_free ;
char *heap_limit ;

char *heap_old ;

static obj_t *alloc(size_t size) {
  if (heap_free+size < heap_limit) {
    obj_t *obj = (obj_t *) heap_free ;
    heap_free += ((size+7) / 8) * 8 ;
    return (obj_t *) obj ;
  } else {
    printf("HEAP ALLOCATION ERROR FOR %d", size) ; _exit(1) ;
  }
}

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

uintptr_t closurep(obj_t o) {
  return (o & PRIMARY_MASK == CLOSURE_TAG) ;
}

closure_t *make_closure(size_t len) {
  closure_t *closure = (closure_t *) alloc(sizeof(closure_t)+len*sizeof(obj_t)) ;
  closure->header = make_header(len, 0) ;
  return closure ;
}

uintptr_t templatep(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o,0)) == code_template) ;
}

template_t *make_template(size_t len) {
  template_t *template = (template_t *) alloc(sizeof(template_t)+len) ;
  template->header = make_header(len, code_template) ;
  return template ;
}
  
uintptr_t symbolp(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o, 0)) == code_symbol) ;
}

symbol_t *make_symbol(size_t len) {
  symbol_t *symbol = (symbol_t *) alloc(sizeof(symbol_t)+len) ;
  symbol->header = make_header(len, code_symbol) ;
  return symbol ;
}

uintptr_t stringp(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o, 0)) == code_string) ;
}

string_t *make_string(size_t len) {
  string_t *str = (string_t *) alloc(sizeof(string_t)+len) ;
  str->header = make_header(len, code_string) ;
  return str ;
}

uintptr_t pairp(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o, 0)) == code_pair) ;
}

pair_t *make_pair() {
  pair_t *pair = (pair_t *) alloc(sizeof(pair_t)) ;
  pair->header = make_header(0, code_pair) ;
  return pair ;
}

uintptr_t vectorp(obj_t o) {
  return (o & PRIMARY_MASK == PTR_TAG) && (header_code(deref(o, 0)) == code_vector) ;
}

vector_t *make_vector(size_t len) {
  vector_t *v = (vector_t *) alloc(sizeof(vector_t)+sizeof(obj_t)*len) ;
  v->header = make_header(len, code_vector) ;
  return v ;
}

obj_t tag(void *ptr, uint8_t tag) {return (((obj_t) ptr))+tag ; }
obj_t *untag(obj_t obj, uint8_t tag) { return (obj_t *) ((obj - tag)) ; }

static obj_t *alloc(size_t) ;

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

void heap_init() {
  /* Allocation of Scheme stack space */
  stack_size = 16 * 4096 ;
  stack_top = alloc_protected_space(stack_size) ;
  stack_base = stack_top + stack_size ;

  heap_new = heap_free = alloc_protected_space(16*4096) ;
  heap_limit = heap_free + 16*4096 ;
  heap_old = alloc_protected_space(16*4096) ;
 
  
}
