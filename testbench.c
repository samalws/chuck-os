#include <stdio.h>
#include <stdlib.h>
#include "kernel.h"

long oneHundred = 100;
void* exampleIOInput = &oneHundred;

void* exampleIOStartPoint(void* inp) {
  printf("We're in IO, inp is %ld\n", * (long*) inp);
}

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

int testSets() {
  defineSet(exampleSet, 100);
  insert(&exampleSet, 5, 0);
  insert(&exampleSet, 6, 0);
  insert(&exampleSet, 7, 0);
  delete(&exampleSet, 6);
  delete(&exampleSet, 7);
  insert(&exampleSet, 6, 0);
  insert(&exampleSet, 7, 0);
  delete(&exampleSet, 7);

  if (lookup(&exampleSet, 5) == 0) {
    printf("Set lookup failed\n");
    return 1;
  }
  if (lookup(&exampleSet, 6) == 0) {
    printf("Set lookup failed\n");
    return 1;
  }

  if (lookup(&exampleSet, 0) != 0) {
    printf("Set lookup shouldn't have succeeded\n");
    return 1;
  }
  if (lookup(&exampleSet, 7) != 0) {
    printf("Set lookup shouldn't have succeeded\n");
    return 1;
  }
  return 0;
}

int testMaps() {
  int a = 10;
  int b = 11;

  defineMap(exampleMap, 100, int);
  insert(&exampleMap, 5, (void*) &a);
  insert(&exampleMap, 6, (void*) &a);
  insert(&exampleMap, 7, (void*) &b);
  delete(&exampleMap, 6);
  delete(&exampleMap, 7);
  insert(&exampleMap, 6, (void*) &b);
  insert(&exampleMap, 7, (void*) &a);
  delete(&exampleMap, 7);

  if (* (int*) lookup(&exampleMap, 5) != a) {
    printf("Map lookup failed\n");
    return 1;
  }
  if (* (int*) lookup(&exampleMap, 6) != b) {
    printf("Map lookup failed\n");
    return 1;
  }

  if (lookup(&exampleMap, 0) != 0) {
    printf("Map lookup shouldn't have succeeded\n");
    return 1;
  }
  if (lookup(&exampleMap, 7) != 0) {
    printf("Map lookup shouldn't have succeeded\n");
    return 1;
  }
  return 0;
}

int main() {
  printf("Running testbench...\n");

  kernelMain();

  if (testSets() == 0)
    printf("Sets test passed\n");
  if (testMaps() == 0)
    printf("Maps test passed\n");

  return 0;
}
