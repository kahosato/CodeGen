> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes

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
>    = [Define fname] ++ transExp body regs ++ [ret]


Part (2): saving registers

> saveRegs :: [Register] -> [Instr]
> 
> saveRegs regsNotInUse
>  = concatMap (push allRegs not in regsNotInUse )
>       where
>            push reg = [Mov (Reg reg) Push]


Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var x) (dst:rest) = [Mov paramReg dst]
> transExp (Apply s e) (dst:rest)
>    =  first ++ second
>        where 
>            second = transExp e (dst:rest) ++ [Mov dst paramReg] ++ [Jsr s]
>            first =
>            if not elem paramReg avail 
>            then saveRegs[paramReg]
>            else []

> transExp (op e1 e2)(dst:next:rest)
>   =if weight e1 > weight e2
>   then
>   transExp e1 (dst:next:rest) ++
>   transExp e2 (next:rest) ++
>   transBiop op dst next 
>   else
>   transExp e2 (next:dst:rest) ++
>   transExp e1 (dst:rest) ++
>   transBiop op dst next
>      where 
>       transBiop Add r1 r2 = [Add r1 r2]
>       transBiop Sub r1 r2 = [Sub r1 r2]

> weight :: Exp -> Int
> weight (Const x) = 1
> weight (Var x) = 1
> weight (Apply s e) = weight e 
> weight (Biop e1 e2)
>    = min w1 w2
>       where 
>           w1 = max (weight e1) (weight e1) + 1
>           w2 = max (weight e1) + 1 (weight e)

> restoreRegs :: [Register] -> [Instr]
> restoreRegs regsPushed
>    = concatMap(pop (reverse regsPushed))
>       where 
>           pop reg = [Mov Pop (Reg reg)] 

