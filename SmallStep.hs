-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   ---- Executa o comando C 10 vezes
    | Repeat C B --- Repeat C until B: executa C enquanto B é falso
    | Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C 
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
   deriving(Eq,Show)         

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):      

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)

smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)


smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not TRUE, s) = (FALSE, s)
smallStepB (Not FALSE, s) = (TRUE, s)
smallStepB (Not b, s) = let (bl, sl) = smallStepB (b, s) 
                        in (Not bl, sl)

smallStepB (And TRUE b, s)  = (b, s)
smallStepB (And FALSE b, s) = (FALSE, s)
smallStepB (And b1 b2, s)   = let (bl, sl) = smallStepB (b1, s) 
                              in (And bl b2, sl)

smallStepB (Or TRUE b, s)  = (TRUE, s)
smallStepB (Or FALSE b, s) = (b, s)
smallStepB (Or b1 b2, s)   = let (bl, sl) = smallStepB (b1, s) 
                             in (Or bl b2, sl)

smallStepB (Leq (Num n1) (Num n2), s)
    | n1 <= n2  = (TRUE, s)
    | otherwise = (FALSE, s)
smallStepB (Leq (Num n) e, s) = let (el, sl) = smallStepE (e, s) 
                                in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)     = let (el, sl) = smallStepE (e1, s) 
                                in (Leq el e2, sl)

smallStepB (Igual (Num n1) (Num n2), s)
    | n1 == n2  = (TRUE, s)
    | otherwise = (FALSE, s)
smallStepB (Igual (Num n) e, s) = let (el, sl) = smallStepE (e, s) 
                                  in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)     = let (el, sl) = smallStepE (e1, s) 
                                  in (Igual el e2, sl)


smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If TRUE c1 c2, s)  = (c1, s)
smallStepC (If FALSE c1 c2, s) = (c2, s)
smallStepC (If b c1 c2, s)     = let (bl, sl) = smallStepB (b, s) 
                                 in (If bl c1 c2, sl)

smallStepC (Seq Skip c2, s) = (c2, s)
smallStepC (Seq c1 c2, s)   = let (cl, sl) = smallStepC (c1, s) 
                              in (Seq cl c2, sl)

smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s)       = let (el, sl) = smallStepE (e, s) 
                                        in (Atrib (Var x) el, sl)

smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

smallStepC (TenTimes c, s) = (Loop (Num 0) (Num 10) c, s)

smallStepC (Repeat c b, s) = (Seq c (If b Skip (Repeat c b)), s)

smallStepC (Loop (Num n1) (Num n2) c, s)
    | n1 < n2   = (Seq c (Loop (Num (n1 + 1)) (Num n2) c), s)
    | otherwise = (Skip, s)
smallStepC (Loop (Num n) e2 c, s) = let (el, sl) = smallStepE (e2, s)
                                    in (Loop (Num n) el c, sl)
smallStepC (Loop e1 e2 c, s)      = let (el, sl) = smallStepE (e1, s)
                                    in (Loop el e2 c, sl)

smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) (Num n2), s) =
    let s1 = mudaVar s v1 n1
        s2 = mudaVar s1 v2 n2
    in (Skip, s2)
smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) e2, s) =
    let (el, sl) = smallStepE (e2, s)
    in (DuplaATrib (Var v1) (Var v2) (Num n1) el, sl)
smallStepC (DuplaATrib (Var v1) (Var v2) e1 e2, s) =
    let (el, sl) = smallStepE (e1, s)
    in (DuplaATrib (Var v1) (Var v2) el e2, sl)

smallStepC (AtribCond TRUE (Var v1) e1 e2, s)  = (Atrib (Var v1) e1, s)
smallStepC (AtribCond FALSE (Var v1) e1 e2, s) = (Atrib (Var v1) e2, s)
smallStepC (AtribCond b (Var v1) e1 e2, s)     = let (bl, sl) = smallStepB (b, s)
                                                 in (AtribCond bl (Var v1) e1 e2, sl)

smallStepC (Swap (Var x) (Var y), s) =
    let valX = procuraVar s x
        valY = procuraVar s y
        s1   = mudaVar s x valY
        s2   = mudaVar s1 y valX
    in (Skip, s2)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False

interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas

isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

progLoop :: C
progLoop = Loop (Num 0) (Num 3) (Atrib (Var "x") (Soma (Var "x") (Num 1)))

progDuplaAtrib :: C
progDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Num 10) (Num 20)

progRepeat :: C
progRepeat = Repeat (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Num 5) (Var "x"))

progSwap :: C
progSwap = Seq (DuplaATrib (Var "x") (Var "y") (Num 1) (Num 2)) (Swap (Var "x") (Var "y"))

progAtribCond :: C
progAtribCond = AtribCond (Leq (Var "x") (Num 0)) (Var "y") (Num 100) (Num 200)