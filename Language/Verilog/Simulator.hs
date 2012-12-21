module Language.Verilog.Simulator
  ( simulate
  , Assignment (..)
  , Path
  , AExpr (..)
  ) where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Exit
import System.IO
import Text.Printf

import Data.VCD ()

import Language.Verilog.AST

-- | Simulation given the top level module name, a list of modules, and a test bench function.
simulate :: [Module] -> Identifier -> Identifier -> IO ()
simulate modules top clock = do
  cs <- execStateT (compileModule topModule) (CompilerState 0 modules [top] top [] [] False $ Just clock)
  when (hitError cs) exitFailure
  return ()
  where
  topModule = case lookupModule top modules of
    Nothing -> error $ "Top module not found: " ++ top
    Just m  -> m

lookupModule :: Identifier -> [Module] -> Maybe Module
lookupModule name modules = case [ m | m@(Module n _ _ ) <- modules, name == n ] of
  [m] -> Just m
  []  -> Nothing
  _   -> error $ "Duplicate modules of same name: " ++ name


data CompilerState = CompilerState
  { nextId      :: Int
  , modules     :: [Module]
  , path        :: Path
  , moduleName  :: Identifier
  , assignments :: [Assignment]
  , environment :: [(Identifier, Var)]
  , hitError    :: Bool
  , clock       :: Maybe Identifier
  }

type VC = StateT CompilerState IO

compileModule :: Module -> VC ()
compileModule (Module _ _ items) = mapM_ compileModuleItem items

extendEnv :: Int -> Identifier -> VC ()
extendEnv width name = do
  c <- get
  when (clock c /= Just name) $ do
    put c { nextId = nextId c + 1, environment = (name, Var (nextId c) width [path c ++ [name]]) : environment c }

extendEnv' :: Maybe Range -> [(Identifier, Maybe Range)] -> VC ()
extendEnv' range vars = case range of
  Nothing                       -> mapM_ (f 1)         vars
  Just (Number msb, Number "0") -> mapM_ (f $ num msb) vars
  Just r -> error' $ "Invalid range in variable declaration: " ++ show r
  where
  num :: String -> Int
  num = read  -- XXX This will fail on 'h format.
  f :: Int -> (Identifier, Maybe Range) -> VC ()
  f width (var, array) = case array of
    Nothing -> extendEnv width var
    Just _  -> error' $ "Arrays not supported: " ++ var

getVar :: Identifier -> VC Var
getVar a = do
  c <- get
  case lookup a $ environment c of
    Nothing -> do
      error' $ "Variable not found: " ++ a
      return $ Var 0 0 []
    Just a -> return a

inEnv :: Identifier -> VC Bool
inEnv a = do
  c <- get
  return $ elem a $ fst $ unzip $ environment c

assignVar :: (Identifier, Expr) -> VC ()
assignVar (v, e) = do
  c <- get
  v <- getVar v
  e <- compileExpr e
  put c { assignments = assignments c ++ [AssignVar v e] }

assignReg :: (Identifier, Expr) -> VC ()
assignReg (v, e) = do
  c <- get
  v <- getVar v
  e <- compileExpr e
  put c { assignments = assignments c ++ [AssignReg v e] }

compileModuleItem :: ModuleItem -> VC ()
compileModuleItem a = case a of
  Parameter range var expr -> do
    inEnv <- inEnv var
    when (not inEnv) $ do
      extendEnv' range [(var, Nothing)]
      assignVar (var, expr)
    
  Inout   _ _ -> error' "inout not supported."
  Input  range vars -> extendEnv' range vars
  Output range vars -> extendEnv' range vars
  Wire   range vars -> extendEnv' range vars
  Reg    range vars -> extendEnv' range vars

  Initial _ -> warning "initial statement ignored."
  Always     sense stmt -> do
    s <- checkSense sense
    case s of
      Combinational -> compileCombStmt stmt
      Posedge       -> compileSeqStmt  stmt
      Negedge       -> warning "negedge always block ignored."

  Assign (LHS v) e -> assignVar (v, e)
  Assign l       _ -> error' $ "Unsupported assignment LHS: " ++ show l

  Instance mName parameters iName bindings -> do
    c <- get 
    case lookupModule mName $ modules c of
      Nothing -> return () --XXX Need to handle externals.
      Just m -> do
        c0 <- get
        put c0 { moduleName = mName, path = path c0 ++ [iName], environment = [], clock = subClock c0 }
        sequence_ [ extendEnv undefined param >> assignVar (param, value) | (param, Just value) <- parameters ]  --XXX How to handle unknown parameter width?
        compileModule m
        --XXX Do bindings after module elaboration.  How to determine direction?
        c1 <- get
        put c1 { moduleName = moduleName c0, path = path c0, environment = environment c0, clock = clock c0 }
    where
    subClock :: CompilerState -> Maybe Identifier
    subClock c = case clock c of
      Nothing  -> Nothing
      Just clk -> case [ newClock | (var, Just newClock) <- bindings, var == clk ] of
        [c] -> Just c
        _ -> Nothing

data SenseType = Combinational | Posedge | Negedge

checkSense :: Sense -> VC SenseType
checkSense sense = case sense of
  Sense   _   -> return Combinational
  SenseOr a b -> do
    a <- checkSense a
    b <- checkSense b
    case (a, b) of
      (Combinational, Combinational) -> return Combinational
      _ -> invalid
  SensePosedge (LHS a) -> do
    c <- get
    if Just a == clock c then return Posedge else invalid
  SenseNegedge _ -> return Negedge
  _ -> invalid
  where
  invalid = do
    error' $ "Unsupported sense: " ++ show sense
    return Combinational

compileCombStmt :: Stmt -> VC ()
compileCombStmt a = compileStmt False a >>= mapM_ assignVar

compileSeqStmt :: Stmt -> VC ()
compileSeqStmt a = compileStmt True a >>= mapM_ assignReg

compileStmt :: Bool -> Stmt -> VC [(Identifier, Expr)]
compileStmt seq stmt = case stmt of

  Block _ stmts -> mapM (compileStmt seq) stmts >>= return . concat

  Case scrutee cases def -> compileStmt seq $ f cases
    where
    f :: [([Expr], Stmt)] -> Stmt
    f a = case a of
      [] -> def
      (values, stmt) : rest -> If (foldl1 Or [ Eq value scrutee | value <- values ]) stmt $ f rest

  BlockingAssignment    (LHS v) e | not seq -> return [(v, e)]
  NonBlockingAssignment (LHS v) e | seq     -> return [(v, e)]

  If pred onTrue onFalse -> do  --XXX How to handle empty, but unreachable branches?  Bluespec code is filled with them.
    t <- compileStmt seq onTrue
    f <- compileStmt seq onFalse
    mapM (merge t f) $ nub $ fst $ unzip $ t ++ f
    where
    merge :: [(Identifier, Expr)] -> [(Identifier, Expr)] -> Identifier -> VC (Identifier, Expr)
    merge t f v = case (lookup v t, lookup v f) of
      (Just a , Just b )       -> return (v, Mux pred a                 b                )
      (Just a , Nothing) | seq -> return (v, Mux pred a                 (ExprLHS $ LHS v))
      (Nothing, Just b ) | seq -> return (v, Mux pred (ExprLHS $ LHS v) b                )
      _ -> do
        error' $ printf "Invalid branch in %s always block regarding variable %s." (if seq then "sequential" else "combinational") v
        return (v, Number "0")

  Null -> return []
  _ -> do
    error' $ printf "Unsupported statement in %s always block: %s" (if seq then "sequential" else "combinational") (show stmt)
    return []

compileExpr :: Expr -> VC AExpr
compileExpr expr = case expr of
  {-
  String     String
  Number     String
  ConstBool  Bool
  ExprLHS    LHS
  ExprCall   Call
  Not        Expr
  And        Expr Expr
  Or         Expr Expr
  BWNot      Expr
  BWAnd      Expr Expr
  BWXor      Expr Expr
  BWOr       Expr Expr
  Mul        Expr Expr
  Div        Expr Expr
  Mod        Expr Expr
  Add        Expr Expr
  Sub        Expr Expr
  ShiftL     Expr Expr
  ShiftR     Expr Expr
  Eq         Expr Expr
  Ne         Expr Expr
  Lt         Expr Expr
  Le         Expr Expr
  Gt         Expr Expr
  Ge         Expr Expr
  Mux        Expr Expr Expr
  Repeat     Expr [Expr]
  Concat     [Expr]
  -}
  _ -> do
    --error' $ "Unsupported expression: " ++ show expr
    return $ AConst 0 0

warning :: String -> VC ()
warning msg = do
  c <- get
  liftIO $ do
    printf "Warning (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout

error' :: String -> VC ()
error' msg = do
  c <- get
  liftIO $ do
    printf "ERROR   (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout
  put c { hitError = True }

-- | A Path is a hierarchical path of identifiers.
type Path = [Identifier]

-- | A sequence of variable assignments and memory updates in A-normal form.
data Assignment
  = AssignVar Var AExpr
  | AssignReg Var AExpr
  | AssignMem Var AExpr AExpr

data Var = Var Int Int [Path]  -- ^ Uid, width, path list of all signals tied together.

data AExpr
  = AConst      Int Integer  -- ^ Width, value.
  | ASelect     Var Int Int  -- ^ LSB is 0.
  | ABWNot      Var
  | ABWAnd      Var Var
  | ABWXor      Var Var
  | ABWOr       Var Var
  | AMul        Var Var
  | AAdd        Var Var
  | ASub        Var Var
  | AShiftL     Var Int
  | AShiftR     Var Int
  | AEq         Var Var
  | ANe         Var Var
  | ALt         Var Var
  | ALe         Var Var
  | AGt         Var Var
  | AGe         Var Var
  | AMux        Var Var Var
  | AConcat     [Var]

