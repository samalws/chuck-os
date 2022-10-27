#include "main.h"
#include "util.h"

void* memAllowedStart;
void* memAllowedEnd;
ID maxTID;
void* execEnvStack[1000]; // TODO what size?
int execEnvStackPtr = 0;

void* allocMID(int size) {}
void freeMID(void* mid) {}
int getMIDSize(void* mid) {}
void makeMIDReadOnly(void* mid) {}

void incRCMID(void* mid) {}
void decRCMID(void* mid) {}

void pushExecEnv(void* execEnv) {}
void* peekExecEnv() {}
void popExecEnv() {}

struct Value* allocExecEnv(void* execEnv) {}
void freeExecEnv(void* execEnv, struct Value* val) {}

void incRCExecEnv(void* execEnv, struct Value* val) {}
void decRCExecEnv(void* execEnv, struct Value* val) {}

void setupExecEnv(struct Value* mids, struct Value* toInitWith, struct Value** retHeadVal, void** execEnv) {}

bool pushExecStack(void* execEnv, struct Value* val) {}
struct Value* popExecStack(void* execEnv) {}

void callVals(void* execEnv, struct Value* f, struct Value* x, struct Value* retLoc) {}

struct Value* callIO(void* execEnv, void* execAt, struct Value* args) {}

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
    if (toEval == 0)
      popExecEnv();
    eval(execEnv, toEval);
  }
}

void ioMain(void* execEnv, struct Value* ioVal) {
  for (;;) {
    pushExecEnv(execEnv);
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

  print("Printed from the kernel!\n");

  char atoiVal[10];
  _atoi(atoiVal, otpFromIO + otpFromProg + (long) memAllowedStart + ((long) memAllowedEnd)*2);
  print(atoiVal);
  print("\n");

  void* midA = allocMID(1000);
  void* midB = allocMID(1000);
  struct Value midAVal = {
    valEnum: IDVal,
    valUnion: { idAndType: {
      id: (long) midA,
      idType: MIDLinType
    }}
  };
  struct Value midBVal = {
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
  };

  struct Value adamVal = {
    valEnum: Undef
  };

  struct Value* headVal;
  void* execEnv;
  setupExecEnv(&mids, &adamVal, &headVal, &execEnv);
  ioMain(execEnv, headVal);
}
