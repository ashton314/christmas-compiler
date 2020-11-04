#include <stdio.h>

void printi(int i) {
  printf("%d", i);
}

void newline() {
  printf("\n");
}

int main () {
  int x = 42;
  printi(x);
  newline();
  return x;
}
