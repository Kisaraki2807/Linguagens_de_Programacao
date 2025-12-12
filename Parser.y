--Recebe os Tokens identificados pelo Lexer
--Produz a AST (Expr)
--Define a estrutura lógica, constrói os nós da árvore, remove ambifuidades
--Aqui é a "receita", o Happy vai gerar em .hs

{
module Parser where 

import Lexer 
}

%name parser Exp
%tokentype { Token }
%error { parseError }


%right '->'
%left '+' '-'
%left '*'
%left "&&" "||"



--aqui é basicamente um glossário de tokens, ligndo lexer com parser
%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    ','             { TokenVirg }
    'if'            { TokenIf }
    'then'          { TokenThen }
    'else'          { TokenElse }
    '\\'            { TokenLam }
    '->'            { TokenArrow }
    ':'             { TokenDPontos }
    Num             { TokenTNum }   
    Bool            { TokenTBool }
    var             { TokenVar $$ }
%% 

--regras de construcao da AST
--aqui é definida a GLC que vai ser usada para construir a arvore da linguagem
Exp     : num                            { Num $1 }
        | true                           { BTrue }
        | false                          { BFalse }
        | Exp '+' Exp                    { Add $1 $3 }
        | Exp '*' Exp                    { Times $1 $3 }
        | Exp "&&" Exp                   { And $1 $3 }
        | Exp "||" Exp                   { Or $1 $3 }
        | '(' Exp ')'                    { Paren $2 }
        | 'if' Exp 'then' Exp 'else' Exp { If $2 $4 $6 }
        | '(' ElemTupla ')'              { Tupla $2 }
        | '\\' var ':' Type '->' Exp     { Lam $2 $4 $6 }
        | var                            { Var $1 }
        | Exp Exp                        { App $1 $2 }


--uma tupla deve aceitar n elementos, isso pode ser alcançado por recursão
--considerei um caso base de 2 elementos pra ser uma tupla e uma recusao para o resto
ElemTupla    : Exp ',' Exp          { [$1, $3] }    
        | Exp ',' ElemTupla         { $1 : $3 }

--fixei uma regra simples pra aceitar tipos bool, num ou function para lambda calculos
--para funcoes ele usa recursao para identificar funcoes aninhadas
--o ultimo caso é pra possibilitar procedencia a esquerda tambem
Type    : Num       { TNum }
        | Bool      { TBool }
        | Type '->' Type    { TFun $1 $3 }  
        | '(' Type ')'      { $2 }
{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}