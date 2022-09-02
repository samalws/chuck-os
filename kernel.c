typedef enum { false, true } bool;

void memcpy(void* dest, const void* src, int n) {
  char* dest_ = dest;
  const char* src_ = src;
  while (n > 0) {
    *dest_ = *src_;
    dest_++;
    src_++;
    n--;
  }
}

typedef int ID;

enum IDType {
  PID,
  MID,
  TID,
  IID
};

enum IDType getIDType(ID id) {
  return id & 3;
}

// TODO all map functions are really inefficient
struct IDMap {
  int capacity;
  int size;
  int valSize;
  ID* keys;
  void* vals;
};

void* lookup(struct IDMap* map, ID key) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      return map->vals + i * map->valSize;
  return 0;
}

void insert(struct IDMap* map, ID key, const void* val);

void assign(struct IDMap* map, ID key, const void* val) {
  for (int i = 0; i < map->size; i++)
    if (map->keys[i] == key)
      memcpy(map->vals + i * map->valSize, val, map->valSize);
  insert(map, key, val);
}

void insert(struct IDMap* map, ID key, const void* val) {
  if (map->size + 1 == map->capacity) return;
  if (lookup(map, key) != 0) assign(map, key, val);
  map->keys[map->size] = key;
  memcpy(map->vals + map->size * map->valSize, val, map->valSize);
  map->size++;
}

void delete(struct IDMap* map, ID key) {
  bool found = false;
  for (int i = 0; i < map->size; i++)
    if (found || map->keys[i] == key) {
      found = true;
      map->keys[i] = map->keys[i+1];
      memcpy(map->vals + i * map->valSize, map->vals + (i+1) * map->valSize, map->valSize);
    }
  if (found)
    map->size--;
}

ID unusedID(struct IDMap* map, enum IDType type) {
  return (map->size >> 2) | type; // TODO
}

struct Program {
  bool sudo;
  int arity;
  void* inputs;
  void* startPoint;
};

struct IO {
  void* input;
  void* startPoint;
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

defineMap(programs, 100, struct Program);
defineMap(memBlks, 100, void*);
defineSet(tokens, 100);
defineMap(ios, 100, struct IO);
defineSet(linearIDs, 100);

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

#define advanceFwd(amt) \
  *((char**) inpLoc) += amt; \
  *((char**) otpLoc) += amt;

#define copyBits(amt) \
  memcpy((void*) *inpLoc, (void*) *otpLoc, amt); \
  advanceFwd(amt);

#define writeIVal(val) \
  **inpLoc = val; \
  (*inpLoc)++; \
  (*otpLoc)++;

#define writeID(id) \
  **((ID**) inpLoc) = id; \
  (*((ID**) inpLoc))++;

// TODO throw a const somewhere around otp
void evalProgramOtp(bool sudo, struct IDMap* allowed, enum ProgramOtp** otpLoc, enum ProgramInp** inpLoc) {
  enum ProgramOtp otpVal = **otpLoc;
  if (otpVal == OUndef) {
    writeIVal(IUndef)
  } else if (otpVal == OLiteral) {
    writeIVal(ILiteral);
    int amtToCopy = *((int*) *otpLoc) + sizeof(int);
    copyBits(amtToCopy);
  } else if (otpVal == OTup) {
    writeIVal(ITup);
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
  } else if (otpVal == OIDVal) {
    writeIVal(IIDVal);
    copyBits(sizeof(ID));
  } else if (otpVal == OToReadMem) {
    // TODO
  } else if (otpVal == OCall) {
    // TODO
  } else if (otpVal == OLitProgram) {
    bool* linear = (bool*) ((*otpLoc) + 1);
    struct Program* prog = (struct Program*) ((*linear) + 1);
    if (!sudo && prog->sudo) {
      writeIVal(IUndef);
    } else {
      ID pid = unusedID(&programs, PID);
      insert(&programs, pid, prog); // TODO program creation should be broken off into a separate method
      if (*linear)
        insert(&linearIDs, pid, 0);
      writeIVal(IIDVal);
      writeID(pid);
    }
    *((char**) otpLoc) += sizeof(bool) + sizeof(struct Program);
  } else if (otpVal == OMakeToken) {
    bool* linear = (bool*) ((*otpLoc) + 1);
    ID tid = unusedID(&tokens, TID);
    insert(&tokens, tid, 0);
    if (*linear)
      insert(&linearIDs, tid, 0);
    writeIVal(IIDVal);
    writeID(tid);
    *((char**) otpLoc) += sizeof(bool);
  } else if (otpVal == OLitIO) {
    if (!sudo) {
      writeIVal(IUndef);
    } else {
      bool* linear = (bool*) ((*otpLoc) + 1);
      struct IO* io = (struct IO*) ((*linear) + 1);
      ID iid = unusedID(&ios, IID);
      insert(&ios, iid, io);
      if (*linear)
        insert(&linearIDs, iid, 0);
      writeIVal(IIDVal);
      writeID(iid);
    }
    *((char**) otpLoc) += sizeof(bool);
  } else if (otpVal == OPointer) {
    (*otpLoc)++;
    *otpLoc = **((enum ProgramOtp***) otpLoc); // wtf?
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
  }
}

#undef advanceFwd
#undef copyBits
#undef writeIVal
#undef writeID

int init() {
}
