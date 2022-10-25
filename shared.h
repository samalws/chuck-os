typedef enum { false, true } bool;

typedef unsigned long ID;

enum IDType {
  PID,
  MID,
  TID,
  IID
};

// TODO all map functions are really inefficient
struct IDMap {
  int capacity;
  int size;
  int valSize;
  ID* keys;
  void* vals;
};

struct Program {
  bool sudo;
  int arity;
  int argsGiven;
  void* inputs;
  void* startPoint;
};
// TODO MIDs that contain the program?

struct IO {
  void* input;
  void* startPoint;
};

enum ProgramInp {
  IUndef,
  ILiteral,
  ITup,
  IIDVal
};

enum ProgramOtp {
  OUndef,
  OLiteral,
  OTup,
  OIDVal,
  OToReadMem,
  OCall,
  OLitProgram,
  OMakeToken,
  OLitIO,
  OPointer
};
