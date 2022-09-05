// TODO:

// "non-technical":
// eval IO stand-in function
// fill in some arguments for prog2 in evalProgram
// fix up sudo and allowed when calling fns
// master function corresponding to runProgram in haskell

// "technical":
// get assembly to call this C code; test stuff eg maps out (probably the easiest)
// real IO (probably the second easiest)
// figure out where to malloc stuff (prog arguments, ProgramOutput and ProgramInput)
// real memory allocation
// real process calls

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
  int argsGiven;
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

enum ProgramOtp* runProgram(struct Program* prog) {
  return 0; // TODO
}

enum ProgramOtp* evalProgram(bool forcePartialLinear, bool wasLinear, struct Program* prog, enum ProgramInp* inp) {
  struct Program prog2; // TODO
  if (prog2.argsGiven >= prog2.arity) {
    return runProgram(&prog2);
  } else {
    ID pid = unusedID(&programs, PID);
    insert(&programs, pid, &prog2);
    bool linear = forcePartialLinear || wasLinear; /* || inp linear TODO */
    if (linear)
      insert(&linearIDs, pid, 0);
  }
}

#define advanceInp(amt) *((char**) inpLoc) += amt;

#define viewInpAs(name,typ) \
  typ* name = (typ*) (*inpLoc); \
  advanceInp(sizeof(typ));

#define copyBits(loc, amt) \
  memcpy((void*) *inpLoc, (void*) loc, amt); \
  advanceInp(amt);

#define writeIVal(val) \
  **inpLoc = val; \
  advanceInp(sizeof(enum ProgramInp));

#define writeID(val) \
  writeIVal(IIDVal); \
  **inpLoc = val; \
  advanceInp(sizeof(ID));

#define advanceOtp(amt) *((char**) otpLoc) += amt;

#define viewOtpAs(name,typ) \
  typ* name = (typ*) (*otpLoc); \
  advanceOtp(sizeof(typ));

// TODO throw a const somewhere around otp
void evalProgramOtp(bool sudo, struct IDMap* allowed, enum ProgramOtp** otpLoc, enum ProgramInp** inpLoc) {
  enum ProgramOtp otpVal = **otpLoc;
  advanceOtp(sizeof(enum ProgramOtp));
  if (otpVal == OUndef) {
    writeIVal(IUndef);
  } else if (otpVal == OLiteral) {
    writeIVal(ILiteral);
    int amtToCopy = *((int*) *otpLoc) + sizeof(int);
    copyBits(*otpLoc, amtToCopy);
    advanceOtp(amtToCopy);
  } else if (otpVal == OTup) {
    writeIVal(ITup);
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
  } else if (otpVal == OIDVal) {
    viewOtpAs(id, ID);
    if (lookup(allowed, *id) == 0) {
      writeIVal(IUndef);
    } else {
      writeID(*id);
    }
  } else if (otpVal == OToReadMem) {
    viewOtpAs(id, ID);
    if (lookup(allowed, *id) == 0 || getIDType(*id) != MID) {
      writeIVal(IUndef);
    } else {
      delete(&linearIDs, *id);
      // TODO actually change the memory
      writeID(*id);
    }
  } else if (otpVal == OCall) {
    viewOtpAs(forcePartialLinear, bool);
    enum ProgramInp* oldInpLoc = *inpLoc; // TODO for now we just hijack inpLoc
    enum ProgramInp* fnLoc = *inpLoc;
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
    enum ProgramInp* fnInp = *inpLoc;
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
    *inpLoc = oldInpLoc;
    ID pid = *((ID*) (fnLoc+1));
    struct Program* prog = lookup(&programs, pid);
    if (*fnLoc != IIDVal || lookup(allowed, pid) == 0 || prog == 0) {
      writeIVal(IUndef);
    } else {
      bool wasLinear = lookup(&linearIDs, pid) != 0; // yes, I know I can leave out the "!= 0"
      evalProgram(*forcePartialLinear, wasLinear, prog, fnInp);
      *inpLoc = oldInpLoc;
      evalProgramOtp(sudo, allowed, otpLoc, inpLoc); // TODO sudo and allowed need to change
    }
  } else if (otpVal == OLitProgram) {
    viewOtpAs(linear, bool);
    viewOtpAs(prog, struct Program);
    if (!sudo && prog->sudo) {
      writeIVal(IUndef);
    } else if (prog->arity == 0) {
      *otpLoc = runProgram(prog);
      evalProgramOtp(sudo, allowed, otpLoc, inpLoc); // TODO sudo and allowed need to change
    } else {
      ID pid = unusedID(&programs, PID);
      insert(&programs, pid, prog); // TODO program creation should be broken off into a separate method(?)
      if (*linear)
        insert(&linearIDs, pid, 0);
      writeID(pid);
    }
  } else if (otpVal == OMakeToken) {
    viewOtpAs(linear, bool);
    ID tid = unusedID(&tokens, TID);
    insert(&tokens, tid, 0);
    if (*linear)
      insert(&linearIDs, tid, 0);
    writeID(tid);
  } else if (otpVal == OLitIO) {
    viewOtpAs(linear, bool);
    viewOtpAs(io, struct IO);
    if (!sudo) {
      writeIVal(IUndef);
    } else {
      ID iid = unusedID(&ios, IID);
      insert(&ios, iid, io);
      if (*linear)
        insert(&linearIDs, iid, 0);
      writeID(iid);
    }
  } else if (otpVal == OPointer) {
    // TODO typically we don't want to let the user access this
    *otpLoc = **((enum ProgramOtp***) otpLoc); // wtf?
    evalProgramOtp(sudo, allowed, otpLoc, inpLoc);
  }
}

// copypasted from evalProgramOtp and then modified
void genIDSetOtp(struct IDMap* set, enum ProgramOtp** otpLoc) {
  enum ProgramOtp otpVal = **otpLoc;
  advanceOtp(sizeof(enum ProgramOtp));
  if (otpVal == OUndef) {
  } else if (otpVal == OLiteral) {
    int amtToCopy = *((int*) *otpLoc) + sizeof(int);
    advanceOtp(amtToCopy);
  } else if (otpVal == OTup) {
    genIDSetOtp(set, otpLoc);
    genIDSetOtp(set, otpLoc);
  } else if (otpVal == OIDVal) {
    viewOtpAs(id, ID);
    insert(set, *id, 0);
  } else if (otpVal == OToReadMem) {
    viewOtpAs(id, ID);
    insert(set, *id, 0);
  } else if (otpVal == OCall) {
    viewOtpAs(forcePartialLinear, bool);
    genIDSetOtp(set, otpLoc);
    genIDSetOtp(set, otpLoc);
  } else if (otpVal == OLitProgram) {
    viewOtpAs(linear, bool);
    viewOtpAs(prog, struct Program);
  } else if (otpVal == OMakeToken) {
    viewOtpAs(linear, bool);
  } else if (otpVal == OLitIO) {
    viewOtpAs(linear, bool);
    viewOtpAs(io, struct IO);
  } else if (otpVal == OPointer) {
    // TODO typically we don't want to let the user access this
    *otpLoc = **((enum ProgramOtp***) otpLoc); // wtf?
    genIDSetOtp(set, otpLoc);
  }
}

void genIDSetInp(struct IDMap* set, enum ProgramInp** inpLoc) {
  enum ProgramInp inpVal = **inpLoc;
  advanceInp(sizeof(enum ProgramInp));
  if (inpVal == IUndef) {
  } else if (inpVal == ILiteral) {
    int amtToCopy = *((int*) *inpLoc) + sizeof(int);
    advanceInp(amtToCopy);
  } else if (inpVal == ITup) {
    genIDSetInp(set, inpLoc);
    genIDSetInp(set, inpLoc);
  } else if (inpVal == IIDVal) {
    viewInpAs(id, ID);
    insert(set, *id, 0);
  }
}

#undef advanceInp
#undef viewInpAs
#undef copyBits
#undef writeIVal
#undef writeID
#undef advanceOtp
#undef viewOtpAs

int init() {
}
