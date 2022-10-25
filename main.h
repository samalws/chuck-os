#include "shared.h"

void* exampleIOInput;
void* exampleIOStartPoint(); // function so that it says in the text section

volatile void* runIO(struct IO* io, enum ProgramInp* otpLoc);
void runProgram(struct Program* prog, enum ProgramOtp* otpLoc);

ID allocMID(int size);
void freeMID(ID mid);
