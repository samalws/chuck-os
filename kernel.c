// notes from future me:
// "memory environments"
//   program execution has a fixed amount of memory for managing stuff
//   should "raise an exception" if we're out of memory and "catch" at some point using IO for catch
//   this catch will allow for feeding more memory (MIDs) or killing the task
//   when I make the scheduler this will be part of it: not only what programs are running but also what memory environment they're in
// should do two passes for evalProgramOtp:
//   one to add stuff that needs to be run to the scheduler,
//   and another to fill in values from the scheduler
//   scheduler should record function outputs temporarily until they're needed
// I ought to do tail call elimination later on (how?)
//   really only applies when a function returns "call f x"
//   in general, how do we avoid call stacks getting really long?
// as long as program output memory chunk sizes are limited (they will be),
//   the recursion depth will be bounded, so we should never need to grab new memory except for making new MIDs
// we need two malloc functions:
//   one for mallocing a program IO chunk within a memory environment (easy because theyre all the same size),
//   and one for mallocing a new MID (also easy if we make them all the same size)
// on my computer, page size is 4096 bytes which is really small; MIDs should be bigger; maybe we should make them be non fixed size
//   also ideally program IO chunks should be non fixed size as well, so lightweight programs can return small stuff but big programs can return a lot
//   how to specify a program IO chunk size?
//   either specify at the function literal level (easier),
//   or when returning from the function (more versatile; really you dont even need to specify, it can be implicit; unused IO memory gets freed right away)
//     this is the better solution
//   ideally we could just memcpy the function return values to a safe place and reuse the return memory chunk
//     only good if the fn return value isnt too big (shouldnt be; should be a page at most "ie" 4096 bytes which is trivial-ish to copy)
// scheduler needs to make a dependency graph of processes and then run the ones at the "top"
//   this might just be a call stack lol
//   only on single core, that is
//   so good enough for now but later we should improve on it
// scheduler should also include a list of IO to run
//   doesnt matter that we keep it in "sequential order" since if two IOs are on top, then they must be forked, independent tasks,
//   rather than 1 depends on the other

// TODO:

// "non-technical":
// fill in some arguments for prog2 in evalProgram
// gc, linearity check

// "technical":
// get assembly to call this C code; test stuff eg maps out (probably the easiest)
// real IO (probably the second easiest)
// figure out where to malloc stuff (prog arguments, ProgramOutput and ProgramInput)
// real memory allocation
// real process calls

#include "main.h"

// TODO should be shared
enum IDType getIDType(ID id) {
  return id & 3;
}

void* memcpy(void* dest, const void* src, long unsigned n) {
  char* dest_ = dest;
  const char* src_ = src;
  while (n > 0) {
    *dest_ = *src_;
    dest_++;
    src_++;
    n--;
  }
  return dest;
}

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

enum ProgramOtp* evalProgram(bool forcePartialLinear, bool wasLinear, struct Program* prog, enum ProgramInp* inpLoc) {
  struct Program prog2; // TODO
  if (prog2.argsGiven >= prog2.arity) {
    return runProgram(&prog2);
  } else {
    ID pid = unusedID(&programs, PID);
    insert(&programs, pid, &prog2);
    bool linear = forcePartialLinear || wasLinear; /* || inpLoc linear TODO */
    if (linear)
      insert(&linearIDs, pid, 0);
  }
}

enum ProgramOtp* evalPID(bool forcePartialLinear, ID pid, enum ProgramInp* inpLoc) {
  bool wasLinear = lookup(&linearIDs, pid) != 0;
  struct Program* prog = lookup(&programs, pid);
  return evalProgram(forcePartialLinear, wasLinear, prog, inpLoc);
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

void evalPIDOtp(ID pid, enum ProgramOtp** otpLoc, enum ProgramInp** inpLoc);

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
      evalPID(*forcePartialLinear, pid, fnInp);
      *inpLoc = oldInpLoc;
      evalPIDOtp(pid, otpLoc, inpLoc);
    }
  } else if (otpVal == OLitProgram) {
    viewOtpAs(linear, bool);
    viewOtpAs(prog, struct Program);
    if (!sudo && prog->sudo) {
      writeIVal(IUndef);
    } else if (prog->arity == 0) {
      *otpLoc = runProgram(prog);
      defineSet(allowed, 100);
      enum ProgramOtp* otpLocBackup = *otpLoc;
      genIDSetOtp(&allowed, otpLoc);
      *otpLoc = otpLocBackup;
      evalProgramOtp(prog->sudo, &allowed, otpLoc, inpLoc);
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

void evalPIDOtp(ID pid, enum ProgramOtp** otpLoc, enum ProgramInp** inpLoc) {
  struct Program* prog = lookup(&programs, pid);
  bool sudo = false; // default 0 doesn't matter much; otp should be OUndef if lookup returns 0
  if (prog != 0)
    sudo = prog->sudo;
  defineSet(allowed, 100);
  enum ProgramInp* inpLocBackup = *inpLoc;
  genIDSetInp(&allowed, inpLoc);
  *inpLoc = inpLocBackup;
  evalProgramOtp(sudo, &allowed, otpLoc, inpLoc);
}

enum ProgramInp* runIID(ID iid) {
  if (getIDType(iid) != IID) return 0; // TODO IUndef

  struct IO* io = lookup(&ios, iid);
  if (io == 0) return 0; // TODO IUndef

  // TODO gc if linear

  return runIO(io);
}

enum ProgramInp* runInpIO(enum ProgramInp** inpLoc) {
  enum ProgramInp inpVal = **inpLoc;
  advanceInp(sizeof(enum ProgramInp));
  if (inpVal == IIDVal) {
    viewInpAs(id, ID);
    return runIID(*id);
  } else {
    // TODO IUndef
    return 0;
  }
}

enum ProgramInp* runOtpIO(bool sudo, enum ProgramOtp* otpLoc) {
  enum ProgramOtp* otpLocBackup = otpLoc;
  defineSet(allowed, 100);
  genIDSetOtp(&allowed, &otpLoc);
  otpLoc = otpLocBackup;
  evalProgramOtp(sudo, &allowed, &otpLoc, 0 /* TODO */);
  return runInpIO(0 /* TODO */);
}

enum ProgramInp* runPIDIO(ID pid, enum ProgramInp* inpLoc) {
  enum ProgramOtp* otpLoc = evalPID(false, pid, inpLoc);
  enum ProgramInp* inpLoc2 = inpLoc; // TODO can we cannibalize inpLoc?
  enum ProgramInp* inpLoc2Backup = inpLoc2;
  evalPIDOtp(pid, &otpLoc, &inpLoc2); // TODO could use better variable names :P
  inpLoc2 = inpLoc2Backup;
  return runInpIO(&inpLoc2);
}

#undef advanceInp
#undef viewInpAs
#undef copyBits
#undef writeIVal
#undef writeID
#undef advanceOtp
#undef viewOtpAs

int kernelMain() {
}
