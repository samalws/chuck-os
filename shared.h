// TODO add "pointer to ID"? that way we can check for equality

#ifndef shared_h
#define shared_h

typedef enum { false, true } bool;

typedef unsigned long ID;

// TODO will be an int but should be a char
enum IDType {
  MIDLinType,
  MIDNonlinType,
  TIDLinType,
  TIDNonlinType,
  PIDLinType,
  PIDNonlinType,
  IIDLinType,
  IIDNonlinType
};

// TODO all map functions are really inefficient
struct MIDMap {
  int capacity;
  int size;
  int valSize;
  ID* keys;
  void* vals;
};

// TODO this is gonna be an int but should be a char
// TODO volatile?
enum ValEnum {
  Undef,
  Literal,
  Tup,
  IDVal,
  ToReadMem,
  Call,
  EvalInExecEnv,
  LitProgram,
  MakeToken,
  LitIO,
  PointerOutsideExecEnv
};

struct Value {
  int rc;
  enum ValEnum valEnum;
  union {
    unsigned long literal;
    struct { ID id; enum IDType idType; } idAndType;
    struct { struct Value* va; struct Value* vb; bool bl; bool subsReady; } valuesAndBool;
    struct { struct Value* mids; int arity; bool sudo; bool subsReady; } litProgram;
    struct { void* execAt; struct Value* args; bool subsReady; } litIO;
    struct { void* execEnv; struct Value* v; } pointerOutsideExecEnv;
  } valUnion;
};

#define defineMap(name,capac,valType) \
ID name##Keys[capac]; \
valType name##Vals[capac]; \
struct MIDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = sizeof(valType), \
  .keys = name##Keys, \
  .vals = name##Vals \
};

#define defineSet(name,capac) \
ID name##Keys[capac]; \
struct MIDMap name = { \
  .capacity = capac, \
  .size = 0, \
  .valSize = 0, \
  .keys = name##Keys, \
  .vals = (void*) 1 \
};

#endif
