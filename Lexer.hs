--Recebe o texto digitado pelo usuário (String)
--Retorna os elementos significativos (Tokens)
--Detecta erros léxicos

module Lexer where 

import Data.Char 

--tokens possiveis da linguagem
data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen 
           | TokenVirg
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenLam
           | TokenArrow
           | TokenDPontos
           | TokenTBool
           | TokenTNum
           | TokenVar String
           deriving Show 

--esqueleto da arvore AST
data Expr = Num Int 
          | BTrue 
          | BFalse 
          | Add Expr Expr 
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Paren Expr 
          | If Expr Expr Expr 
          | Var String --variavel
          | Lam String Ty Expr -- define funcao com parametro string de tipo ty com corpo expr
          | App Expr Expr --aplicar funcao expr com valor expr
          | Tupla [Expr]
          deriving Show 

--os tipos da minha linguagem
data Ty = TNum 
        | TBool 
        | TFun Ty Ty 
        | TTupla [Ty]
        deriving (Show, Eq) 

--codifica input em uma lista de tokens da linguagem
-- transforma o header e chama a funcao novamente pro tail
lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs  
lexer (',':cs) = TokenVirg : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer (':':cs) = TokenDPontos : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

--definindo palavras reservadas e considerando todo o resto como variavel
lexKw cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("Num", rest)  -> TokenTNum : lexer rest 
             ("Bool", rest) -> TokenTBool : lexer rest
             (var, rest) -> TokenVar var : lexer rest 