-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
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

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s) = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s) = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)  = div (ebigStep (e1,s)) (ebigStep (e2,s))


bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
    | bbigStep (b,s) == True     = False
    | otherwise                  = True 
bbigStep (Or b1 b2,s )
    | (bbigStep (b1,s) == True) || (bbigStep (b2,s) == True) = True
    | otherwise = False
bbigStep (And b1 b2,s )
    | (bbigStep (b1,s) == False) || (bbigStep (b2,s) == False) = False
    | otherwise = True
bbigStep (Leq e1 e2,s)
    | ebigStep (e1,s) <= ebigStep (e2,s)  = True
    | otherwise = False
bbigStep (Igual e1 e2,s)
    | ebigStep (e1,s) == ebigStep (e2,s) = True
    | otherwise = False


cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

cbigStep (Atrib (Var x) e,s) = (Skip, mudaVar s x (ebigStep (e,s)))

cbigStep (Seq c1 c2, s) = cbigStep (c2, s')
  where 
    (Skip, s') = cbigStep (c1, s)

cbigStep (If b c1 c2,s)
    | bbigStep (b,s) == True = cbigStep (c1,s)
    | otherwise              = cbigStep (c2,s)

cbigStep (While b c, s) = cbigStep (If b (Seq c (While b c)) Skip, s)

cbigStep (TenTimes c, s) = cbigStep (Loop (Num 0) (Num 10) c, s)

cbigStep (Repeat c b, s) = cbigStep (Seq c (If b Skip (Repeat c b)), s)

cbigStep (Loop e1 e2 c, s)
    | v1 < v2   = cbigStep (Seq c (Loop (Num (v1 + 1)) (Num v2) c), s)
    | otherwise = (Skip, s)
  where 
    v1 = ebigStep (e1, s)
    v2 = ebigStep (e2, s)

cbigStep (DuplaATrib (Var v1) (Var v2) e1 e2, s) = (Skip, s2)
  where 
    ve1 = ebigStep (e1, s)
    ve2 = ebigStep (e2, s)
    s1    = mudaVar s v1 ve1     
    s2    = mudaVar s1 v2 ve2

cbigStep (AtribCond b (Var v1) e1 e2, s) = cbigStep (If b (Atrib (Var v1) e1) (Atrib (Var v1) e2), s)

cbigStep (Swap (Var x) (Var y), s) = (Skip, s2)
  where 
    vx = procuraVar s x
    vy = procuraVar s y
    s1   = mudaVar s x vy
    s2   = mudaVar s1 y vx


--------------------------------------
---
--- Exemplos de programas para teste
---
-------------------------------------

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

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))


---
--- Exemplos de expressões booleanas:

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))