#include <stdio.h>
#include <stdlib.h>
#include "kernel.h"
#include "util.h"

void print(char* str) {
  printf("%s",str);
}

void exampleIOStartPoint(void* inp, void* otp) {
  long longInp = * (long*) inp;
  printf("We're in IO, inp is %ld\n", longInp);
  * (long*) otp = longInp*3;
}

void exampleProgramStartPoint(void* inp, void* otp) {
  long longInp = * (long*) inp;
  printf("We're in a program, inp is %ld\n", longInp);
  * (long*) otp = longInp*4;
}

void runIO(void* startPoint, void* inpLoc, void* otpLoc) {
  void (*asFunction) (void*, void*) = startPoint;
  asFunction(inpLoc, otpLoc);
}

void runProgram(int nMids, void** mids, void* inpLoc, void* otpLoc) {
  void (*asFunction) (void*, void*) = mids[0];
  asFunction(inpLoc, otpLoc);
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

  kernelMain((void*) 1, (void*) 10000);

  if (testSets() == 0)
    printf("Sets test passed\n");
  if (testMaps() == 0)
    printf("Maps test passed\n");

  return 0;
}
