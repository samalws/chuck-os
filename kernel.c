#include "main.h"
#include "util.h"

void* memAllowedStart;
void* memAllowedEnd;
ID maxTID;
void* execEnvStack[1000]; // TODO what size?
int execEnvStackPtr = 0;
char programInpSpace[4096]; // TODO what size?
char programOtpSpace[4096]; // TODO what size?

// TODO this is rlly bad
void* maxMIDAllocated = 0;
int lastAllocSize = 0;
void* allocMID(int size) {
  if (maxMIDAllocated == 0)
    maxMIDAllocated = memAllowedStart;
  void* retVal = maxMIDAllocated;
  maxMIDAllocated += size;
  lastAllocSize = size;
  return retVal;
}
void freeMID(void* mid) {}
int getMIDSize(void* mid) {
  return lastAllocSize;
}
void makeMIDReadOnly(void* mid) {}

void incRCMID(void* mid) {}
void decRCMID(void* mid) {}

void pushExecEnv(void* execEnv) {
  execEnvStack[execEnvStackPtr++] = execEnv;
}
void* peekExecEnv() {
  return execEnvStackPtr ? execEnvStack[execEnvStackPtr-1] : 0;
}
void popExecEnv() {
  execEnvStackPtr--;
}

struct Value* allocExecEnv(void* execEnv) {
  void* mid = execEnv; // TODO deal with multiple mids in execEnv
  char* asChars = (char*) mid;

  int numAllocBits = getMIDSize(mid) / sizeof(struct Value);
  int numAllocBytes = (numAllocBits >> 3) + 1; // TODO DRY

  for (int i = 0; i < numAllocBytes; i++)
    if (asChars[i])
      for (int j = 0 ;; j++)
        if (asChars[i] & (1 << j))
          return &((struct Value*) execEnv)[i<<3 + j];
  return 0;
}
void freeExecEnv(void* execEnv, struct Value* val) {
  void* mid = execEnv; // TODO deal with multiple mids in execEnv
  char* asChars = (char*) mid;

  int valOffset = ((void*) val - mid) / sizeof(struct Value);

  char* allocByte = &asChars[valOffset>>3];
  *allocByte &= ~ (1 << (valOffset & 7)); // wtf?
}

void incRCExecEnv(void* execEnv, struct Value* val) {}
void decRCExecEnv(void* execEnv, struct Value* val) {}

void setupExecEnvSingleMid(void* mid) {
  char* asChars = (char*) mid;

  int numAllocBits = getMIDSize(mid) / sizeof(struct Value);
  int numAllocBytes = (numAllocBits >> 3) + 1;
  int numAllocAllocBytes = ((numAllocBytes / sizeof(struct Value)) >> 3) + 1; // wtf?
  int numAllocStackBytes = (numAllocBytes >> 2) + 1;

  for (int i = numAllocAllocBytes; i < numAllocBytes - numAllocStackBytes; i++)
    asChars[i] = 0xFF;

  for (int i = 0; i < numAllocAllocBytes; i++)
    asChars[i] = 0;

  for (int i = numAllocBytes - numAllocStackBytes; i < numAllocBytes; i++)
    asChars[i] = 0;

  // stack pointer
  int* sp = mid + getMIDSize(mid) - sizeof(int);
  *sp = 0;
}
// TODO should make its own stack rather than being recursive:
struct Value* copyIntoExecEnv(void* execEnv, struct Value* toInitWith) {
  struct Value* retVal = allocExecEnv(execEnv);
  _memcpy(retVal, toInitWith, sizeof(struct Value));
  // TODO switch on type and call for subtrees
  return retVal;
}
void setupExecEnv(struct Value* mids, struct Value* toInitWith, struct Value** retHeadVal, void** retExecEnv) {
  void* mid = (void*) mids -> valUnion . idAndType . id; // TODO should traverse tree getting em all instead
  setupExecEnvSingleMid(mid);
  *retHeadVal = copyIntoExecEnv(mid, toInitWith);
  *retExecEnv = mid;
}

bool pushExecStack(void* execEnv, struct Value* val) {
  // TODO deal with multiple exec envs, and check for going out of range
  int* sp = execEnv + getMIDSize(execEnv) - sizeof(int);
  struct Value* memBlk = ((struct Value*) sp) - (sizeof(struct Value) * (*sp));
  _memcpy(memBlk, val, sizeof(struct Value));
  *sp ++;
}
struct Value* popExecStack(void* execEnv) {
  // TODO deal with multiple exec envs, and check for going out of range
  int* sp = execEnv + getMIDSize(execEnv) - sizeof(int);
  if (*sp == 0) return 0;
  *sp --;
  return ((struct Value*) sp) - (sizeof(struct Value) * (*sp));
}

void callVals(void* execEnv, struct Value* f, struct Value* x, struct Value* retLoc) {
  retLoc -> valEnum = Undef; // TODO
}

struct Value* callIO(void* execEnv, void* execAt, struct Value* args) {
  struct Value* retVal = allocExecEnv(execEnv);
  retVal -> valEnum = Undef; // TODO
  print("IO called\n");
  return retVal;
}

// DOESN'T SANITIZE
// TODO should return bool for success and check whether pushExecStack worked
void eval(void* execEnv, struct Value* val) {
  enum ValEnum valEnum = val->valEnum;

  if (valEnum == Tup) {
    pushExecStack(execEnv, val -> valUnion . valuesAndBool . va);
    pushExecStack(execEnv, val -> valUnion . valuesAndBool . vb);
  } else if (valEnum == ToReadMem) {
    makeMIDReadOnly((void*) val -> valUnion . idAndType . id);
    val -> valUnion . idAndType . idType = MIDNonlinType;
  } else if (valEnum == Call) {
    if (! val -> valUnion . valuesAndBool . subsReady) {
      val -> valUnion . valuesAndBool . subsReady = true;
      pushExecStack(execEnv, val);
      pushExecStack(execEnv, val -> valUnion . valuesAndBool . va);
      pushExecStack(execEnv, val -> valUnion . valuesAndBool . vb);
    } else {
      struct Value newVal;
      callVals(execEnv, val -> valUnion . valuesAndBool . va, val -> valUnion . valuesAndBool . vb, &newVal);
      decRCExecEnv(execEnv, val -> valUnion . valuesAndBool . va);
      decRCExecEnv(execEnv, val -> valUnion . valuesAndBool . vb);
      *val = newVal;
    }
  } else if (valEnum == EvalInExecEnv) {
    if (! val -> valUnion . valuesAndBool . subsReady) {
      val -> valUnion . valuesAndBool . subsReady = true;
      pushExecStack(execEnv, val);
      pushExecStack(execEnv, val -> valUnion . valuesAndBool . va);
      // DON'T push vb; that will be evaluated in a new env
    } else {
      struct Value* newLoc;
      void* firstMid;
      setupExecEnv(val -> valUnion . valuesAndBool . va, val -> valUnion . valuesAndBool . vb, &newLoc, &firstMid);
      decRCExecEnv(execEnv, val -> valUnion . valuesAndBool . va);
      decRCExecEnv(execEnv, val -> valUnion . valuesAndBool . vb);
      // TODO should we incRC newLoc or does setupExecEnv do that for us?
      if (newLoc == 0)
        val -> valEnum = Undef;
      else {
        val -> valEnum = PointerOutsideExecEnv;
        val -> valUnion . pointerOutsideExecEnv . execEnv = firstMid;
        val -> valUnion . pointerOutsideExecEnv . v = newLoc;
        pushExecEnv(firstMid);
      }
    }
  } else if (valEnum == LitProgram) {
    if (! val -> valUnion . litProgram . subsReady) {
      val -> valUnion . litProgram . subsReady = true;
      pushExecStack(execEnv, val -> valUnion . litProgram . mids);
    }
  } else if (valEnum == MakeToken) {
    bool linear = val -> valUnion . valuesAndBool . bl;
    val -> valUnion . idAndType . id = maxTID++;
    val -> valUnion . idAndType . idType = linear ? TIDLinType : TIDNonlinType;
  } else if (valEnum == LitIO) {
    if (! val -> valUnion . litIO . subsReady) {
      val -> valUnion . litIO . subsReady = true;
      pushExecStack(execEnv, val -> valUnion . litIO . args);
    }
  } else if (valEnum == PointerOutsideExecEnv) {
    if (! val -> valUnion . valuesAndBool . subsReady) {
      val -> valUnion . valuesAndBool . subsReady = true;
      pushExecStack(execEnv, val -> valUnion . valuesAndBool . va);
    }
  }
}

void evalExecStack() {
  for (;;) {
    void* execEnv = peekExecEnv();
    if (execEnv == 0) return;
    struct Value* toEval = popExecStack(execEnv);
    if (toEval == 0) {
      popExecEnv();
      continue;
    }
    eval(execEnv, toEval);
  }
}

void ioMain(void* execEnv, struct Value* ioVal) {
  for (;;) {
    pushExecEnv(execEnv);
    pushExecStack(execEnv, ioVal);
    evalExecStack();
    struct Value* newIOVal = callIO(execEnv, ioVal -> valUnion . litIO . execAt, ioVal -> valUnion . litIO . args);
    decRCExecEnv(execEnv, ioVal);
    // TODO do we need to inc the new one?
    ioVal = newIOVal;
  }
}

void kernelMain(void* _memAllowedStart, void* _memAllowedEnd) {
  memAllowedStart = _memAllowedStart;
  memAllowedEnd = _memAllowedEnd;

  long inpToIO = 100;
  long otpFromIO = 0;
  runIO((void*) exampleIOStartPoint, (void*) &inpToIO, &otpFromIO);

  long inpToProg = 1000;
  long otpFromProg = 0;
  void* progMids[1] = { (void*) exampleProgramStartPoint };
  runProgram(1, progMids, (void*) &inpToProg, (void*) &otpFromProg);

  char atoiVal[11];

  _atoi(atoiVal, otpFromIO);
  print("IO output: ");
  print(atoiVal);
  print("\n");

  _atoi(atoiVal, otpFromProg);
  print("Program output: ");
  print(atoiVal);
  print("\n");

  _atoi(atoiVal, (long) memAllowedStart);
  print("Memory start: ");
  print(atoiVal);
  print("\n");

  _atoi(atoiVal, (long) memAllowedEnd);
  print("Memory end: ");
  print(atoiVal);
  print("\n");

  void* midA = allocMID(1000);
  // void* midB = allocMID(1000);
  struct Value midAVal = {
    valEnum: IDVal,
    valUnion: { idAndType: {
      id: (long) midA,
      idType: MIDLinType
    }}
  };
  /* struct Value midBVal = {
    valEnum: IDVal,
    valUnion: { idAndType: {
      id: (long) midB,
      idType: MIDLinType
    }}
  };
  struct Value mids = {
    valEnum: Tup,
    valUnion: { valuesAndBool: {
      va: &midAVal,
      vb: &midBVal,
      subsReady: true
    }}
  }; */

  struct Value adamVal = {
    valEnum: Undef
  };

  struct Value* headVal;
  void* execEnv;
  setupExecEnv(&midAVal /* &mids */, &adamVal, &headVal, &execEnv);
  ioMain(execEnv, headVal);
}
