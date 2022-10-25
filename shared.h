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

#define defineMap(name,capac,valType) \
ID name##Keys[capac]; \
valType name##Vals[capac]; \
struct IDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = sizeof(valType), \
  .keys = name##Keys, \
  .vals = name##Vals \
};

#define defineSet(name,capac) \
ID name##Keys[capac]; \
struct IDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = 0, \
  .keys = name##Keys, \
  .vals = (void*) 1 \
};
