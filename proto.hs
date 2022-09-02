-- details present in the real OS but not present in this implementation:
-- function execution will be strict, not lazy
-- expressions that can be parallelized (taking the form of "(x, y)" or "f x y") will be parallelized
-- the OS will keep track of which processes own what in order to garbage collect unused procs, memblks, tokens, and IOs
-- [Int] literals (we'll use Bytes instead) will consist of the size followed by all the bytes listed one-by-one
-- IO literals will take the form of a pointer to some code to execute, and where to place the arguments in memory
-- function literals will be a pointer to a function to execute in user mode, as well as a list of (write-disabled) memblks that code is present in
-- functions return using a syscall
-- ProgramInps and ProgramOtps will be placed in a specified spot (virtually addressed so all cores can use the same addr) in memory in prefix notation
-- registers have undefined state when a process starts and when it exits; processes with secret data in registers should clear them before exiting
-- memblks will represent physical RAM locations; MIDs will be a pointer to the memory location in RAM
-- scheduler permissioning is done via IO; processes can call other functions but divide up what percent computation time they each get, or give them a timeout
-- unless a timeout is given, processes can run as long as they need
-- no pre-emption is done aside from IO scheduling mentioned above
-- the OS will boot into an "adam" process which returns an IO; can take up multiple cores by forking and by running parallelizable calls
-- relies on GRUB bootloader
-- x86-64

{-# LANGUAGE LambdaCase, NamedFieldPuns, ViewPatterns #-}

import Control.Monad.State
import Data.Default
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

mapMaybeSet :: (Ord b) => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeSet f = S.map (fromJust . f) . S.filter (isJust . f)

data PID = PID Int  deriving (Eq, Ord, Show)
data MID = MID Int  deriving (Eq, Ord, Show)
data TID = TID Int  deriving (Eq, Ord, Show)
data IID = IID Int  deriving (Eq, Ord, Show)
data ID = PIDID PID | MIDID MID | TIDID TID | IIDID IID  deriving (Eq, Ord, Show)
data ProgramInp = IUndef | ILiteral [Int] | ITup ProgramInp ProgramInp | IIDVal ID  deriving Show
data ProgramOtp = OUndef | OLiteral [Int] | OTup ProgramOtp ProgramOtp | OIDVal ID | OCall Bool {- linear result if partial -} ProgramOtp ProgramOtp | OLitProgram Bool {- linear -} Bool {- sudo -} Int {- arity -} Program | OMakeToken Bool {- linear -} | OLitIO {- only for sudo -} Bool {- linear -} (StateT Env IO ProgramInp)
data Program = Program ([ProgramInp] -> State (M.Map MID [Int]) ProgramOtp)
data Env = Env { programs :: M.Map PID (Bool, Int, [ProgramInp], Program), memVals :: M.Map MID [Int], tokens :: S.Set TID, ios :: M.Map IID (StateT Env IO ProgramInp), linearIDs :: S.Set ID }
-- TODO add read-only memory, also executable memory
-- TODO garbage collection

callNonlin = OCall False

instance Default Env where
  def = Env { programs = M.empty, memVals = M.empty, tokens = S.empty, ios = M.empty, linearIDs = S.empty }

inpToOtp :: ProgramInp -> ProgramOtp
inpToOtp IUndef = OUndef
inpToOtp (ILiteral x) = OLiteral x
inpToOtp (ITup a b) = OTup (inpToOtp a) (inpToOtp b)
inpToOtp (IIDVal id) = OIDVal id

genIDSetInp :: ProgramInp -> S.Set ID
genIDSetInp IUndef = mempty
genIDSetInp (ILiteral _) = mempty
genIDSetInp (ITup a b) = genIDSetInp a <> genIDSetInp b
genIDSetInp (IIDVal id) = S.singleton id

genIDSetOtp :: ProgramOtp -> S.Set ID
genIDSetOtp (OTup a b) = genIDSetOtp a <> genIDSetOtp b
genIDSetOtp (OIDVal id) = S.singleton id
genIDSetOtp (OCall _ a b) = genIDSetOtp a <> genIDSetOtp b
genIDSetOtp _ = mempty

containsLinearVals :: (Monad m) => S.Set ID -> StateT Env m Bool
containsLinearVals set = not . null . S.intersection set <$> gets linearIDs

failsLinearity :: (Monad m) => ProgramOtp -> ProgramOtp -> StateT Env m Bool
failsLinearity x y = containsLinearVals (genIDSetOtp x `S.intersection` genIDSetOtp y)

linearityCheck :: (Monad m) => ProgramOtp -> ProgramOtp -> StateT Env m ProgramInp -> StateT Env m ProgramInp
linearityCheck x y i = do
  fails <- failsLinearity x y
  if fails then pure IUndef else i

maybeAddLinearVal :: Bool -> ID -> Env -> Env
maybeAddLinearVal linear id env = env { linearIDs = if linear then S.insert id (linearIDs env) else linearIDs env }

addProgram :: (Monad m) => Bool -> Bool -> Int -> [ProgramInp] -> Program -> StateT Env m PID
addProgram linear sudo arity args prog = do
  env <- get
  let pid = PID $ M.size $ programs env -- TODO fill empty spaces
  put $ maybeAddLinearVal linear (PIDID pid) $ env { programs = M.insert pid (sudo, arity, args, prog) (programs env) }
  pure pid

evalZeroArityProgram :: Program -> ProgramOtp
evalZeroArityProgram (Program prog) = evalState (prog []) M.empty

evalProgram :: (Monad m) => Bool -> PID -> ProgramInp -> StateT Env m ProgramOtp
evalProgram forcePartialLinear callee inp = (M.lookup callee . programs <$> get) >>= \case
  Nothing -> pure OUndef
  Just (_, arity, args, Program prog) | (length args)+1 >= arity -> do -- should never be >
    mem <- gets memVals
    let (progOtp, mem') = runState runProg mem
    modify (\e -> e { memVals = mem' })
    pure progOtp
    where
      elimBadKeys = flip M.restrictKeys $ mapMaybeSet mididOnly $ genIDSetInp inp
      runProg = do
        mem <- get
        modify elimBadKeys
        otp <- prog $ reverse (inp:args)
        modify (`M.union` mem)
        pure otp
      mididOnly (MIDID mid) = Just mid
      mididOnly _ = Nothing
  Just (sudo, arity, args, prog) -> do
    linear <- or <$> sequence (containsLinearVals . genIDSetInp <$> (inp:args))
    OIDVal . PIDID <$> addProgram (linear || forcePartialLinear) sudo arity (inp:args) prog

evalProgramOtp :: (Monad m) => Bool -> S.Set ID -> ProgramOtp -> StateT Env m ProgramInp
evalProgramOtp _ _ OUndef = pure IUndef
evalProgramOtp _ _ (OLiteral x) = pure $ ILiteral x
evalProgramOtp sudo allowed (OTup x y) = linearityCheck x y $ ITup <$> evalProgramOtp sudo allowed x <*> evalProgramOtp sudo allowed y
evalProgramOtp _ allowed (OIDVal id) = pure $ if id `S.member` allowed then IIDVal id else IUndef
evalProgramOtp sudo allowed (OCall linear f x) = linearityCheck f x $ do
  f' <- evalProgramOtp sudo allowed f
  x' <- evalProgramOtp sudo allowed x
  case f' of
    IIDVal (PIDID pidF) -> evalProgram linear pidF x' >>= evalPidOtp pidF
    _ -> pure IUndef
evalProgramOtp False allowed (OLitProgram _ True _ _) = pure IUndef
evalProgramOtp _ allowed (OLitProgram _ sudo 0 prog) = evalProgramOtp sudo mempty $ evalZeroArityProgram prog
evalProgramOtp _ allowed (OLitProgram linear sudo arity prog) = IIDVal . PIDID <$> addProgram linear sudo arity [] prog
evalProgramOtp _ allowed (OMakeToken linear) = do
  env <- get
  let tid = TID $ S.size $ tokens env -- TODO fill empty spaces
  put $ maybeAddLinearVal linear (TIDID tid) $ env { tokens = S.insert tid (tokens env) }
  pure $ IIDVal $ TIDID tid
evalProgramOtp False allowed (OLitIO _ _) = pure IUndef
evalProgramOtp True allowed (OLitIO linear io) = do
  env <- get
  let iid = IID $ M.size $ ios env -- TODO fill empty spaces
  put $ maybeAddLinearVal linear (IIDID iid) $ env { ios = M.insert iid io (ios env) }
  pure $ IIDVal $ IIDID iid

evalPidOtp :: (Monad m) => PID -> ProgramOtp -> StateT Env m ProgramInp
evalPidOtp callee otp = do
  sudo <- gets (maybe False (\(sudo,_,_,_) -> sudo) . M.lookup callee . programs)
  -- "maybe False" doesn't matter much; otp should be OUndef if M.lookup returns Nothing
  evalProgramOtp sudo (genIDSetOtp otp) otp

runIO :: IID -> StateT Env IO ProgramInp
runIO iid = gets (M.lookup iid . ios) >>= fromMaybe (pure IUndef)

runInp :: ProgramInp -> StateT Env IO ProgramInp
runInp (IIDVal (IIDID iid)) = runIO iid
runInp _ = pure IUndef

runOtp :: Bool -> ProgramOtp -> StateT Env IO ProgramInp
runOtp sudo o = evalProgramOtp sudo (genIDSetOtp o) o >>= runInp

runProgram :: PID -> ProgramInp -> StateT Env IO ProgramInp
runProgram callee = evalProgram False callee >=> evalPidOtp callee >=> runInp

pureProg :: Program
pureProg = Program f where
  f [a] = pure $ OLitIO True {- TODO -} $ pure a
  f _ = pure OUndef

-- TODO what is this called other than bind?
thenProg :: Program
thenProg = Program f where
  f [a, b] = pure $ OLitIO True {- TODO -} $ runInp a >> runInp b
  f _ = pure OUndef

bindProg :: Program
bindProg = Program f where
  f [a, b] = pure $ OLitIO True {- TODO -} $ runInp a >>= (\ioOtp -> runOtp True (inpToOtp b `callNonlin` inpToOtp ioOtp))
  -- the True is irrelevant, since sudo doesn't matter for OCall, or for anything returned by inpToOtp
  f _ = pure OUndef

printProg :: Program
printProg = Program f where
  f [ILiteral x] = pure $ OLitIO False $ lift (print x) >> pure (ILiteral [])
  f _ = pure OUndef

readLnIO :: StateT Env IO ProgramInp
readLnIO = ILiteral <$> lift readLn

getMemIO :: StateT Env IO ProgramInp
getMemIO = do
  env <- get
  let mid = MID $ M.size $ memVals env -- TODO fill empty spaces
  put $ env { memVals = M.insert mid [] (memVals env), linearIDs = S.insert (MIDID mid) (linearIDs env) }
  pure $ IIDVal $ MIDID mid

starterEnv :: Env
starterEnv = def { programs = M.fromList [(PID 0, (True, 1, [], pureProg)), (PID 1, (True, 2, [], thenProg)), (PID 2, (True, 2, [], bindProg)), (PID 3, (True, 1, [], printProg))], ios = M.fromList [(IID 0, readLnIO), (IID 1, getMemIO)] }

helloWorldProg :: Program
helloWorldProg = Program f where
  f [print_] = pure $ callNonlin (inpToOtp print_) (OLiteral [1,2,3])
  f _ = pure OUndef

exampleProg :: Program
exampleProg = Program f where
  f (fmap inpToOtp -> [pure_, then_, bind_, print_, readLn_, getMem_])
    = pure $ callNonlin (callNonlin bind_ readLn_) (callNonlin (OLitProgram False False 2 $ Program g) print_)
  f _ = pure OUndef
  g [print_, ILiteral l] = pure $ callNonlin (inpToOtp print_) (OLiteral (65:l))
  g _ = pure OUndef

progsEnv :: Env
progsEnv = starterEnv { programs = programs starterEnv <> M.fromList [(PID 4, (False, 1, [], helloWorldProg)), (PID 5, (False, 6, [], exampleProg))] }

main = runStateT (runOtp False prog) progsEnv >>= putStrLn . ("final output: " ++) . show . fst where
  prog = ((((((fn 5 `cn` fn 0) `cn` fn 1) `cn` fn 2) `cn` fn 3) `cn` io 0) `cn` io 1)
  cn = callNonlin
  fn = OIDVal . PIDID . PID
  io = OIDVal . IIDID . IID
