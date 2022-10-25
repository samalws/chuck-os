#include <stdio.h>
#include <stdlib.h>
#include "kernel.h"

enum ProgramInp* runIO(struct IO* io) {
  enum ProgramInp* (*asFunction) (void*) = io->startPoint;
  return asFunction(io->input);
}

enum ProgramOtp* runProgram(struct Program* prog) {
  enum ProgramOtp* (*asFunction) (void*) = prog->startPoint;
  return asFunction(prog->inputs);
}

ID allocMID(int size) {
  return (ID) malloc(size);
}

void freeMID(ID mid) {
  free((void*) mid);
}

int main() {
  printf("Running testbench...\n");
  kernelMain();
  return 0;
}
