#include "shared.h"

void* exampleIOStartPoint(); // function so that it says in the text section
void* exampleProgramStartPoint();

void print(char* str);

void runIO(void* execAt, void* inpLoc, void* otpLoc);
void runProgram(int nMids, void** mids, void* inpLoc, void* otpLoc);
