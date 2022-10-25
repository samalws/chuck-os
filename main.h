#include "shared.h"

void runIO(struct IO* io, enum ProgramInp* otpLoc);
void runProgram(struct Program* prog, enum ProgramOtp* otpLoc);

ID allocMID(int size);
void freeMID(ID mid);
