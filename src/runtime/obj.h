#ifndef SCHEME_OBJ_H
#define SCHEME_OBJ_H

#include <stdint.h>

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

#define CLOSURE(c) ((closure_t *) ((c) - PTR_TAG))
#define TEMPLATE(t) ((template_t *) ((t) - PTR_TAG))
#define SYMBOL(s) ((symbol_t *) ((s) - PTR_TAG))
#define STRING(s) ((symbol_t *) ((s) - PTR_TAG))
#define PAIR(p) ((pair_t *) ((p) - PTR_TAG))
#define VECTOR(v) ((vector_t *) ((v) - PTR_TAG))
#define BYTEVECTOR(v) ((byte_vector_t *) ((v) - PTR_TAG))

typedef struct {
  header_t header ;
  obj_t closed[] ;
} closure_t ;

typedef struct {
  header_t header ;
  obj_t debug ;
  char code[] ;
} template_t ;

typedef struct {
  header_t header ;
  uintptr_t length ;
  char ch[] ;
} symbol_t;

typedef struct {
  header_t header ;
  uintptr_t   length ;
  char ch[] ;
} string_t ;

typedef struct {
  header_t header ;
  obj_t head ;
  obj_t tail ;
} pair_t ;

typedef struct {
  header_t header ;
  obj_t value[] ;
} vector_t ;

typedef struct {
  header_t header ;
  uint8_t value[] ;
} byte_vector_t ;

typedef enum { code_vector, code_string, code_symbol, code_byte_vector,
	       code_template, code_pair, code_instance,
	       code_forward }
  header_code_t ;

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
  
  void* entry_print ;
} context_t ;

extern int stack_size ;
extern char *stack_top ;
extern char *stack_base ;

extern char *heap_new ;
extern char *heap_free ;
extern char *heap_limit ;

extern char *heap_old ;

extern vector_t *make_vector(size_t) ;
extern pair_t *make_pair() ;
extern closure_t *make_closure(size_t) ;
extern template_t *make_template(size_t) ;
extern symbol_t *make_symbol(size_t) ;
extern string_t *make_string(size_t) ;

#endif
