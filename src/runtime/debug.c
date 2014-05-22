#include <stdio.h>
#include "debug.h"

void print_scm_obj(obj_t x) ;

static
void print_scm_vector(obj_t x)
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

static
void print_scm_pair(obj_t x)
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

static
void print_secondary(obj_t x)
{
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

void print_scm_obj(obj_t x)
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
