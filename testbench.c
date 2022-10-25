#include <stdio.h>
#include <stdlib.h>
#include "kernel.h"

void runIO(struct IO* io, enum ProgramInp* otpLoc) {
  void (*asFunction) (void*, enum ProgramInp*) = io->startPoint;
  asFunction(io->input, otpLoc);
}

void runProgram(struct Program* prog, enum ProgramOtp* otpLoc) {
  void (*asFunction) (void*, enum ProgramOtp*) = prog->startPoint;
  asFunction(prog->inputs, otpLoc);
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
