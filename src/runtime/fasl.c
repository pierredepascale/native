#include <unistd.h>

#include "fasl.h"

typedef enum { fop_template, fop_ref, fop_number, fop_closure, fop_symbol,
	       fop_string, fop_null, fop_unbound, fop_unspecific, fop_vector,
               fop_true, fop_false, fop_pair, fop_char}
 file_operation_t ;

static obj_t read_template(int);
static obj_t read_ref(int);
static obj_t read_number(int);
static obj_t read_closure(int);
static obj_t read_symbol(int);
static obj_t read_string(int);
static obj_t read_vector(int);
static obj_t read_pair(int);
static obj_t read_char(int);

static 
uint8_t read_u8(int fd)
{
  uint8_t v ;
  if (read(fd, &v, 1) == 1) {
    return v ;
  } else {
    _exit(1) ;
  }
}

static
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

