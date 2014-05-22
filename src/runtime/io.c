#include <unistd.h>
#include "obj.h"

obj_t scmrt_open_input(obj_t filename)
{
  return OBJ_FALSE ;
}

obj_t scmrt_open_output(obj_t filename)
{
  return OBJ_FALSE ;
}

obj_t scmrt_close(obj_t fd)
{
  int rc = close(NUMBER(fd)) ;
  if (rc == -1)
    return MAKE_NUMBER(rc) ;
  else
    return MAKE_NUMBER(rc) ;
}

obj_t scmrt_write(obj_t fd, obj_t buffer, obj_t start, obj_t end)
{
  ssize_t rc = write(NUMBER(fd), &STRING(buffer)->ch[NUMBER(start)],
		     NUMBER(end)-NUMBER(start)) ;
  if (rc == -1) {
    return MAKE_NUMBER(rc) ;
  } else {
    return MAKE_NUMBER(rc) ;
  }
}

obj_t scmrt_read(obj_t fd, obj_t buffer, obj_t len)
{
  ssize_t rc = read(NUMBER(fd), STRING(buffer)->ch, NUMBER(len)) ;

  if (rc == -1) {
    return MAKE_NUMBER(rc) ;
  } else {
    return MAKE_NUMBER(rc) ;
  }
}
