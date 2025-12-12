--Recebe
--Input= Árvore Parser Output=
module TypeChecker where 

import Lexer 

--memoria do typechecker, cara variavel e seu tipo
type Ctx = [(String, Ty)]


--o verificador de tipo de fato
--
typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx BTrue = Just TBool 
typeof ctx BFalse = Just TBool 
typeof ctx (Num n) = Just TNum 

--typeof de parenteses,
typeof ctx (Paren e) = typeof ctx e

--typeof de add, verifica e1, e2 e retorna so se forem do mesmo tipo
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

--typeof de times, verifica e1, e2 e retorna so se forem do mesmo tipo
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

--typeof de and, verifica e1, e2 e retorna so se forem do mesmo tipo
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing

--typeof de or, verifica e1, e2 e retorna so se forem do mesmo tipo
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing

--typeof do if
typeof ctx (If e e1 e2) = case typeof ctx e of 
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of 
                                            (Just t1, Just t2) | t1 == t2  -> Just t1 
                                                               | otherwise -> Nothing 
                                            _ -> Nothing  
                            _ -> Nothing 

--typeof do var, captura variavel
typeof ctx (Var x) = lookup x ctx 

--typeof do lam
typeof ctx (Lam x tp b) = let ctx' = (x,tp) : ctx 
                            in case (typeof ctx' b) of 
                                 Just tr -> Just (TFun tp tr)
                                 _ -> Nothing 

--typeof do app, verifica se o tipo da entrada da funcao é igual ao tipo do argumento
typeof ctx (App e1 e2) = case typeof ctx e1 of  
                           Just (TFun tp tr) -> case typeof ctx e2 of 
                                                  Just t2 | t2 == tp -> Just tr 
                                                  _ -> Nothing 
                           _ -> Nothing 

--typeof da tupla, percorre a tupla por recursao e valida se todos os elementos tem tipo valido
typeof ctx (Tupla exps) = case verificaTupla exps of
                            Just listaTipos -> Just (TTupla listaTipos)
                            Nothing         -> Nothing
  where
    -- percorre a tupla
    verificaTupla :: [Expr] -> Maybe [Ty]
    verificaTupla [] = Just []  -- tupla vazia
    verificaTupla (e:es) = case typeof ctx e of
                             Just t -> case verificaTupla es of
                                         Just ts -> Just (t : ts) 
                                         Nothing -> Nothing
                             Nothing -> Nothing

typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"
