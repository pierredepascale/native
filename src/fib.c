#include <stdio.h>

int fib(int n) {
  if (n < 2) return n ; else return fib(n-1)+fib(n-2) ;
}

int main(int ac, char *av[])
{
  printf("fib38=%d", fib(38)) ;
}
