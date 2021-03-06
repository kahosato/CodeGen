> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration
One can assume that the variable is at paramReg

> transFunction :: Function -> [Instr]
>
> transFunction (Defun fname paramname body)
>    = [Define fname] ++ transExp body (allRegs \\ [paramReg]) ++ [Ret]

Part (2): saving registers

> saveRegs :: [Register] -> [Instr]
> 
> saveRegs regsNotInUse
>  = [(Mov (Reg reg) Push) | reg <- (allRegs \\ regsNotInUse)]

Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var x) (dst:rest) = [Mov (Reg paramReg) (Reg dst)]
> transExp (Apply s e) (dst:rest)
>    = saveRegs (dst:rest) ++ transExp e allRegs ++ [Jsr s] ++ [Mov (Reg resultReg) (Reg dst)]
>    ++ restoreRegs(allRegs \\ (dst:rest))
>
> transExp (Plus e1 e2) regs = transBiopExp 'p' e1 e2 regs 
> transExp (Minus e1 e2) regs = transBiopExp 'm' e1 e2 regs 

> transBiopExp :: Char -> Exp -> Exp -> [Register] -> [Instr]
>
> transBiopExp op e1 e2  (dst:next:rest)
>   =if weight e1 > weight e2
>   then
>   transExp e1 (dst:next:rest) ++
>   transExp e2 (next:rest) ++
>   transBiop op dst next 
>   else
>   transExp e2 (next:dst:rest) ++
>   transExp e1 (dst:rest) ++
>   transBiop op dst next
>       where 
>           transBiop 'p' r1 r2 = [Add (Reg r1) (Reg r2)]
>           transBiop 'm' r1 r2 = [Sub (Reg r2) (Reg r1)]

> weight :: Exp -> Int
> weight (Const x) = 1
> weight (Var x) = 1
> weight (Apply s e) = length allRegs + 1  
> weight (Plus e1 e2) = weightBiop e1 e2 
> weight (Minus e1 e2) = weightBiop e1 e2

> weightBiop :: Exp -> Exp -> Int 
> weightBiop e1 e2 = min w1 w2
>       where 
>           w1 = max (weight e1) ((weight e2) + 1)
>           w2 = max ((weight e1) + 1) (weight e2)
        
> restoreRegs :: [Register] -> [Instr]
> restoreRegs regsPushed
>       =[Mov Pop (Reg reg) | reg <- (reverse regsPushed)]
