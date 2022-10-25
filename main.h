#include "shared.h"

enum ProgramInp* runIO(struct IO* io);
enum ProgramOtp* runProgram(struct Program* prog);

ID allocMID(int size);
void freeMID(ID mid);
