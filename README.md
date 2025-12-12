# Linguagens_de_Programacao
Trabalho de Linguagens de Programação

Para testar em Linux:

1 - happy Parser.y
2 - runghc Main.hs
3 - Expressão
4 - Ctrl + D

Testes:

Input: 10 + 5 * 2                                        Output: Num20

Input: true && (false || true)                           Output: BTrue

INput: if 5 + 5 then 1 else 0                            Output: Type error!

Input: (10 * 2, true && false, 5)                        Output: Tupla [Num 20, BFalse, Num 5]

Input: (1, (2, 3), 4)                                    Output: Tupla [Num 1, Tupla [Num 2, Num 3], Num 4]

Input: (\x : Num -> x) 42                                Output: Num 100

Input: (\b : Bool -> if b then 1 else 0) true            Output: Num 1

INput: ((\x : Num -> x + 1) 10, (\y : Bool -> y) false)  Output: Tupla [Num 11, BFalse]



