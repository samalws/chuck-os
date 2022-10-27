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
// figure out where to malloc stuff (prog arguments, ProgramOutput and ProgramInput)
// real memory allocation
// put process into user mode

#include "main.h"
#include "util.h"

void* memAllowedStart;
void* memAllowedEnd;

// TODO

void* kernelMain(void* _memAllowedStart, void* _memAllowedEnd) {
  memAllowedStart = _memAllowedStart;
  memAllowedEnd = _memAllowedEnd;

  long inpToIO = 100;
  long otpFromIO = 0;
  runIO((void*) exampleIOStartPoint, (void*) &inpToIO, &otpFromIO);

  long inpToProg = 1000;
  long otpFromProg = 0;
  void* progMids[1] = { (void*) exampleProgramStartPoint };
  runProgram(1, progMids, (void*) &inpToProg, (void*) &otpFromProg);

  printStrStd("Printed from the kernel!\n");

  return (void*) (otpFromIO + otpFromProg + (long) memAllowedStart + ((long) memAllowedEnd)*2);
}
