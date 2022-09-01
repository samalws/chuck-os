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

{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

import Control.Monad.State
import Data.Default
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data PID = PID Int  deriving (Eq, Ord, Show)
data MID = MID Int  deriving (Eq, Ord, Show)
data TID = TID Int  deriving (Eq, Ord, Show)
data IID = IID Int  deriving (Eq, Ord, Show)
data ProgramInp = IUndef | ILiteral [Int] | ITup ProgramInp ProgramInp | IFn PID | IMemBlk MID | IToken TID | IIO IID  deriving Show
data ProgramOtp = OUndef | OLiteral [Int] | OTup ProgramOtp ProgramOtp | OFn PID | OMemBlk MID | OToken TID | OIO IID | OCall ProgramOtp ProgramOtp | OLitProgram Bool {- sudo -} Int {- arity -} Program | OMakeToken | OLitIO (StateT Env IO ProgramInp) {- OLitIO only for sudo -}
data Program = Program ([ProgramInp] -> State (M.Map MID [Int]) ProgramOtp)
data Env = Env { programs :: M.Map PID (Bool, Int, [ProgramInp], Program), memVals :: M.Map MID [Int], tokens :: S.Set TID, ios :: M.Map IID (StateT Env IO ProgramInp) }
data AllowedList = AllowedList { allowedPids :: S.Set PID, allowedMids :: S.Set MID, allowedTids :: S.Set TID, allowedIids :: S.Set IID }
-- TODO linear memory -> need linear functions
-- TODO also have read-only memory, also executable memory?
-- TODO garbage collection

instance Default Env where
  def = Env { programs = M.empty, memVals = M.empty, tokens = S.empty, ios = M.empty }

instance Semigroup AllowedList where
  (AllowedList pa ma ta ia) <> (AllowedList pb mb tb ib) = AllowedList (pa <> pb) (ma <> mb) (ta <> tb) (ia <> ib)

instance Monoid AllowedList where
  mempty = AllowedList mempty mempty mempty mempty

inpToOtp :: ProgramInp -> ProgramOtp
inpToOtp IUndef = OUndef
inpToOtp (ILiteral x) = OLiteral x
inpToOtp (ITup a b) = OTup (inpToOtp a) (inpToOtp b)
inpToOtp (IFn pid) = OFn pid
inpToOtp (IMemBlk mid) = OMemBlk mid
inpToOtp (IToken tid) = OToken tid
inpToOtp (IIO iid) = OIO iid

genAllowedListInp :: ProgramInp -> AllowedList
genAllowedListInp IUndef = mempty
genAllowedListInp (ILiteral _) = mempty
genAllowedListInp (ITup a b) = genAllowedListInp a <> genAllowedListInp b
genAllowedListInp (IFn     pid) = mempty { allowedPids = S.singleton pid }
genAllowedListInp (IMemBlk mid) = mempty { allowedMids = S.singleton mid }
genAllowedListInp (IToken  tid) = mempty { allowedTids = S.singleton tid }
genAllowedListInp (IIO     iid) = mempty { allowedIids = S.singleton iid }

genAllowedListOtp :: ProgramOtp -> AllowedList
genAllowedListOtp (OTup a b) = genAllowedListOtp a <> genAllowedListOtp b
genAllowedListOtp (OFn     pid) = mempty { allowedPids = S.singleton pid }
genAllowedListOtp (OMemBlk mid) = mempty { allowedMids = S.singleton mid }
genAllowedListOtp (OToken  tid) = mempty { allowedTids = S.singleton tid }
genAllowedListOtp (OIO     iid) = mempty { allowedIids = S.singleton iid }
genAllowedListOtp (OCall a b) = genAllowedListOtp a <> genAllowedListOtp b
genAllowedListOtp _ = mempty

addProgram :: (Monad m) => Bool -> Int -> [ProgramInp] -> Program -> StateT Env m PID
addProgram sudo arity args prog = do
  env <- get
  let pid = PID $ M.size $ programs env -- TODO fill empty spaces
  put $ env { programs = M.insert pid (sudo, arity, args, prog) (programs env) }
  pure pid

evalProgram :: (Monad m) => PID -> ProgramInp -> StateT Env m ProgramOtp
evalProgram callee inp = (M.lookup callee . programs <$> get) >>= \case
  Nothing -> pure OUndef
  Just (_, arity, args, Program prog) | (length args)+1 >= arity -> do -- should never be >
    mem <- gets memVals
    let (progOtp, mem') = runState runProg mem
    modify (\e -> e { memVals = mem' })
    pure progOtp
    where
      elimBadKeys = flip M.restrictKeys $ allowedMids $ genAllowedListInp inp
      runProg = do
        mem <- get
        modify elimBadKeys
        otp <- prog $ reverse (inp:args)
        modify (`M.union` mem)
        pure otp
  Just (sudo, arity, args, prog) -> OFn <$> addProgram sudo arity (inp:args) prog

evalProgramOtp :: (Monad m) => Bool -> AllowedList -> ProgramOtp -> StateT Env m ProgramInp
evalProgramOtp _ _ OUndef = pure IUndef
evalProgramOtp _ _ (OLiteral x) = pure $ ILiteral x
evalProgramOtp sudo allowed (OTup x y) = ITup <$> evalProgramOtp sudo allowed x <*> evalProgramOtp sudo allowed y
evalProgramOtp _ allowed (OFn     pid) = pure $ if pid `S.member` allowedPids allowed then IFn     pid else IUndef
evalProgramOtp _ allowed (OMemBlk mid) = pure $ if mid `S.member` allowedMids allowed then IMemBlk mid else IUndef
evalProgramOtp _ allowed (OToken  tid) = pure $ if tid `S.member` allowedTids allowed then IToken  tid else IUndef
evalProgramOtp _ allowed (OIO     iid) = pure $ if iid `S.member` allowedIids allowed then IIO     iid else IUndef
evalProgramOtp sudo allowed (OCall f x) = do
  f' <- evalProgramOtp sudo allowed f
  x' <- evalProgramOtp sudo allowed x
  case f' of
    IFn pidF -> evalProgram pidF x' >>= evalPidOtp pidF
    _ -> pure IUndef
evalProgramOtp False allowed (OLitProgram True _ _) = pure IUndef
evalProgramOtp _ allowed (OLitProgram sudo 0 (Program prog)) = evalProgramOtp sudo mempty $ evalState (prog []) M.empty
evalProgramOtp _ allowed (OLitProgram sudo arity prog) = IFn <$> addProgram sudo arity [] prog
evalProgramOtp _ allowed OMakeToken = do
  env <- get
  let tid = TID $ S.size $ tokens env -- TODO fill empty spaces
  put $ env { tokens = S.insert tid (tokens env) }
  pure $ IToken tid
evalProgramOtp False allowed (OLitIO _) = pure IUndef
evalProgramOtp True allowed (OLitIO io) = do
  env <- get
  let iid = IID $ M.size $ ios env -- TODO fill empty spaces
  put $ env { ios = M.insert iid io (ios env) }
  pure $ IIO iid

evalPidOtp :: (Monad m) => PID -> ProgramOtp -> StateT Env m ProgramInp
evalPidOtp callee otp = do
  sudo <- gets (maybe False (\(sudo,_,_,_) -> sudo) . M.lookup callee . programs)
  -- "maybe False" doesn't matter much; otp should be OUndef if M.lookup returns Nothing
  evalProgramOtp sudo (genAllowedListOtp otp) otp

runIO :: IID -> StateT Env IO ProgramInp
runIO iid = gets (M.lookup iid . ios) >>= fromMaybe (pure IUndef)

runInp :: ProgramInp -> StateT Env IO ProgramInp
runInp (IIO iid) = runIO iid
runInp _ = pure IUndef

runOtp :: Bool -> ProgramOtp -> StateT Env IO ProgramInp
runOtp sudo o = evalProgramOtp sudo (genAllowedListOtp o) o >>= runInp

runProgram :: PID -> ProgramInp -> StateT Env IO ProgramInp
runProgram callee = evalProgram callee >=> evalPidOtp callee >=> runInp

pureProg :: Program
pureProg = Program f where
  f [a] = pure $ OLitIO $ pure a
  f _ = pure OUndef

-- TODO what is this called other than bind?
thenProg :: Program
thenProg = Program f where
  f [a, b] = pure $ OLitIO $ runInp a >> runInp b
  f _ = pure OUndef

bindProg :: Program
bindProg = Program f where
  f [a, b] = pure $ OLitIO $ runInp a >>= (\ioOtp -> runOtp True (inpToOtp b `OCall` inpToOtp ioOtp))
  -- the True is irrelevant, since sudo doesn't matter for OCall, or for anything returned by inpToOtp
  f _ = pure OUndef

printProg :: Program
printProg = Program f where
  f [ILiteral x] = pure $ OLitIO $ lift (print x) >> pure (ILiteral [])
  f _ = pure OUndef

readLnProg :: Program
readLnProg = Program $ const $ pure $ OLitIO $ ILiteral <$> lift readLn

getMemProg :: Program
getMemProg = Program $ const $ pure $ OLitIO f where
  f = do
    env <- get
    let mid = MID $ M.size $ memVals env -- TODO fill empty spaces
    put $ env { memVals = M.insert mid [] (memVals env) }
    pure $ IMemBlk mid

freeMemProg :: Program
freeMemProg = Program f where
  f [IMemBlk mid] = pure $ OLitIO $ modify (\env -> env { memVals = M.delete mid (memVals env) }) >> pure (ILiteral [])
  f _ = pure OUndef

-- not how it would really be implemented; would take printProg etc as function arguments instead
helloWorldProg :: Program
helloWorldProg = Program $ const $ pure $ OCall (OLitProgram True 1 printProg) (OLiteral [1,2,3])

-- not how it would really be implemented; would take printProg etc as function arguments instead
exampleProg :: Program
exampleProg = Program $ const $ pure $ OCall (OCall (OLitProgram True 2 bindProg) (OLitProgram True 0 readLnProg)) (OLitProgram True 1 $ Program f) where
  f [ILiteral l] = pure $ OCall (OLitProgram True 1 printProg) (OLiteral (65:l))
  f _ = pure OUndef

main = runStateT (runOtp True $ OLitProgram True 0 exampleProg) def >>= putStrLn . ("final output: " ++) . show . fst
