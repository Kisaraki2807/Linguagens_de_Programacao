{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,179) ([8640,265,1,0,0,20464,66,0,0,0,37404,28688,16968,0,256,0,0,512,53232,49218,2559,1,1,0,28672,16968,8640,265,9351,7172,4242,0,0,0,24576,0,448,0,0,0,0,9351,7172,4242,2048,48,4096,32768,768,0,0,0,16320,269,9983,4,0,18544,66,4160,34560,1828,480,0,1024,8640,457,0,0,3074,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","ElemTupla","Type","num","true","false","'+'","'*'","\"&&\"","\"||\"","'('","')'","','","'if'","'then'","'else'","'\\\\'","'->'","':'","Num","Bool","var","%eof"]
        bit_start = st Prelude.* 26
        bit_end = (st Prelude.+ 1) Prelude.* 26
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..25]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (7) = happyShift action_2
action_0 (8) = happyShift action_4
action_0 (9) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (17) = happyShift action_7
action_0 (20) = happyShift action_8
action_0 (25) = happyShift action_9
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (7) = happyShift action_2
action_3 (8) = happyShift action_4
action_3 (9) = happyShift action_5
action_3 (10) = happyShift action_15
action_3 (11) = happyShift action_16
action_3 (12) = happyShift action_17
action_3 (13) = happyShift action_18
action_3 (14) = happyShift action_6
action_3 (17) = happyShift action_7
action_3 (20) = happyShift action_8
action_3 (25) = happyShift action_9
action_3 (26) = happyAccept
action_3 (4) = happyGoto action_14
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 (7) = happyShift action_2
action_6 (8) = happyShift action_4
action_6 (9) = happyShift action_5
action_6 (14) = happyShift action_6
action_6 (17) = happyShift action_7
action_6 (20) = happyShift action_8
action_6 (25) = happyShift action_9
action_6 (4) = happyGoto action_12
action_6 (5) = happyGoto action_13
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (7) = happyShift action_2
action_7 (8) = happyShift action_4
action_7 (9) = happyShift action_5
action_7 (14) = happyShift action_6
action_7 (17) = happyShift action_7
action_7 (20) = happyShift action_8
action_7 (25) = happyShift action_9
action_7 (4) = happyGoto action_11
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (25) = happyShift action_10
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_12

action_10 (22) = happyShift action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (7) = happyShift action_2
action_11 (8) = happyShift action_4
action_11 (9) = happyShift action_5
action_11 (10) = happyShift action_15
action_11 (11) = happyShift action_16
action_11 (12) = happyShift action_17
action_11 (13) = happyShift action_18
action_11 (14) = happyShift action_6
action_11 (17) = happyShift action_7
action_11 (18) = happyShift action_26
action_11 (20) = happyShift action_8
action_11 (25) = happyShift action_9
action_11 (4) = happyGoto action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (7) = happyShift action_2
action_12 (8) = happyShift action_4
action_12 (9) = happyShift action_5
action_12 (10) = happyShift action_15
action_12 (11) = happyShift action_16
action_12 (12) = happyShift action_17
action_12 (13) = happyShift action_18
action_12 (14) = happyShift action_6
action_12 (15) = happyShift action_24
action_12 (16) = happyShift action_25
action_12 (17) = happyShift action_7
action_12 (20) = happyShift action_8
action_12 (25) = happyShift action_9
action_12 (4) = happyGoto action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_23
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (7) = happyShift action_2
action_14 (8) = happyShift action_4
action_14 (9) = happyShift action_5
action_14 (10) = happyShift action_15
action_14 (11) = happyShift action_16
action_14 (12) = happyShift action_17
action_14 (13) = happyShift action_18
action_14 (14) = happyShift action_6
action_14 (17) = happyShift action_7
action_14 (20) = happyShift action_8
action_14 (25) = happyShift action_9
action_14 (4) = happyGoto action_14
action_14 _ = happyReduce_13

action_15 (7) = happyShift action_2
action_15 (8) = happyShift action_4
action_15 (9) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (17) = happyShift action_7
action_15 (20) = happyShift action_8
action_15 (25) = happyShift action_9
action_15 (4) = happyGoto action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (7) = happyShift action_2
action_16 (8) = happyShift action_4
action_16 (9) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (17) = happyShift action_7
action_16 (20) = happyShift action_8
action_16 (25) = happyShift action_9
action_16 (4) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (7) = happyShift action_2
action_17 (8) = happyShift action_4
action_17 (9) = happyShift action_5
action_17 (14) = happyShift action_6
action_17 (17) = happyShift action_7
action_17 (20) = happyShift action_8
action_17 (25) = happyShift action_9
action_17 (4) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (7) = happyShift action_2
action_18 (8) = happyShift action_4
action_18 (9) = happyShift action_5
action_18 (14) = happyShift action_6
action_18 (17) = happyShift action_7
action_18 (20) = happyShift action_8
action_18 (25) = happyShift action_9
action_18 (4) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (7) = happyShift action_2
action_19 (8) = happyShift action_4
action_19 (9) = happyShift action_5
action_19 (14) = happyShift action_6
action_19 (17) = happyShift action_7
action_19 (20) = happyShift action_8
action_19 (25) = happyShift action_9
action_19 (4) = happyGoto action_14
action_19 _ = happyReduce_7

action_20 (7) = happyShift action_2
action_20 (8) = happyShift action_4
action_20 (9) = happyShift action_5
action_20 (14) = happyShift action_6
action_20 (17) = happyShift action_7
action_20 (20) = happyShift action_8
action_20 (25) = happyShift action_9
action_20 (4) = happyGoto action_14
action_20 _ = happyReduce_6

action_21 (7) = happyShift action_2
action_21 (8) = happyShift action_4
action_21 (9) = happyShift action_5
action_21 (12) = happyShift action_17
action_21 (13) = happyShift action_18
action_21 (14) = happyShift action_6
action_21 (17) = happyShift action_7
action_21 (20) = happyShift action_8
action_21 (25) = happyShift action_9
action_21 (4) = happyGoto action_14
action_21 _ = happyReduce_5

action_22 (7) = happyShift action_2
action_22 (8) = happyShift action_4
action_22 (9) = happyShift action_5
action_22 (11) = happyShift action_16
action_22 (12) = happyShift action_17
action_22 (13) = happyShift action_18
action_22 (14) = happyShift action_6
action_22 (17) = happyShift action_7
action_22 (20) = happyShift action_8
action_22 (25) = happyShift action_9
action_22 (4) = happyGoto action_14
action_22 _ = happyReduce_4

action_23 _ = happyReduce_10

action_24 _ = happyReduce_8

action_25 (7) = happyShift action_2
action_25 (8) = happyShift action_4
action_25 (9) = happyShift action_5
action_25 (14) = happyShift action_6
action_25 (17) = happyShift action_7
action_25 (20) = happyShift action_8
action_25 (25) = happyShift action_9
action_25 (4) = happyGoto action_33
action_25 (5) = happyGoto action_34
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (7) = happyShift action_2
action_26 (8) = happyShift action_4
action_26 (9) = happyShift action_5
action_26 (14) = happyShift action_6
action_26 (17) = happyShift action_7
action_26 (20) = happyShift action_8
action_26 (25) = happyShift action_9
action_26 (4) = happyGoto action_32
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (14) = happyShift action_29
action_27 (23) = happyShift action_30
action_27 (24) = happyShift action_31
action_27 (6) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_29
action_29 (23) = happyShift action_30
action_29 (24) = happyShift action_31
action_29 (6) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_16

action_31 _ = happyReduce_17

action_32 (7) = happyShift action_2
action_32 (8) = happyShift action_4
action_32 (9) = happyShift action_5
action_32 (10) = happyShift action_15
action_32 (11) = happyShift action_16
action_32 (12) = happyShift action_17
action_32 (13) = happyShift action_18
action_32 (14) = happyShift action_6
action_32 (17) = happyShift action_7
action_32 (19) = happyShift action_35
action_32 (20) = happyShift action_8
action_32 (25) = happyShift action_9
action_32 (4) = happyGoto action_14
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (7) = happyShift action_2
action_33 (8) = happyShift action_4
action_33 (9) = happyShift action_5
action_33 (10) = happyShift action_15
action_33 (11) = happyShift action_16
action_33 (12) = happyShift action_17
action_33 (13) = happyShift action_18
action_33 (14) = happyShift action_6
action_33 (16) = happyShift action_25
action_33 (17) = happyShift action_7
action_33 (20) = happyShift action_8
action_33 (25) = happyShift action_9
action_33 (4) = happyGoto action_14
action_33 _ = happyReduce_14

action_34 _ = happyReduce_15

action_35 (7) = happyShift action_2
action_35 (8) = happyShift action_4
action_35 (9) = happyShift action_5
action_35 (14) = happyShift action_6
action_35 (17) = happyShift action_7
action_35 (20) = happyShift action_8
action_35 (25) = happyShift action_9
action_35 (4) = happyGoto action_43
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (15) = happyShift action_41
action_36 (21) = happyShift action_42
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (7) = happyShift action_2
action_37 (8) = happyShift action_4
action_37 (9) = happyShift action_5
action_37 (14) = happyShift action_40
action_37 (17) = happyShift action_7
action_37 (20) = happyShift action_8
action_37 (23) = happyShift action_30
action_37 (24) = happyShift action_31
action_37 (25) = happyShift action_9
action_37 (4) = happyGoto action_38
action_37 (6) = happyGoto action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (7) = happyShift action_2
action_38 (8) = happyShift action_4
action_38 (9) = happyShift action_5
action_38 (10) = happyShift action_15
action_38 (11) = happyShift action_16
action_38 (12) = happyShift action_17
action_38 (13) = happyShift action_18
action_38 (14) = happyShift action_6
action_38 (17) = happyShift action_7
action_38 (20) = happyShift action_8
action_38 (25) = happyShift action_9
action_38 (4) = happyGoto action_14
action_38 _ = happyReduce_11

action_39 (21) = happyShift action_42
action_39 _ = happyReduce_18

action_40 (7) = happyShift action_2
action_40 (8) = happyShift action_4
action_40 (9) = happyShift action_5
action_40 (14) = happyShift action_40
action_40 (17) = happyShift action_7
action_40 (20) = happyShift action_8
action_40 (23) = happyShift action_30
action_40 (24) = happyShift action_31
action_40 (25) = happyShift action_9
action_40 (4) = happyGoto action_12
action_40 (5) = happyGoto action_13
action_40 (6) = happyGoto action_36
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_19

action_42 (14) = happyShift action_29
action_42 (23) = happyShift action_30
action_42 (24) = happyShift action_31
action_42 (6) = happyGoto action_39
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (7) = happyShift action_2
action_43 (8) = happyShift action_4
action_43 (9) = happyShift action_5
action_43 (10) = happyShift action_15
action_43 (11) = happyShift action_16
action_43 (12) = happyShift action_17
action_43 (13) = happyShift action_18
action_43 (14) = happyShift action_6
action_43 (17) = happyShift action_7
action_43 (20) = happyShift action_8
action_43 (25) = happyShift action_9
action_43 (4) = happyGoto action_14
action_43 _ = happyReduce_9

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Times happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 6 4 happyReduction_9
happyReduction_9 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Tupla happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  4 happyReduction_12
happyReduction_12 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1, happy_var_3]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn6
		 (TNum
	)

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn6
		 (TBool
	)

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 26 26 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 7;
	TokenTrue -> cont 8;
	TokenFalse -> cont 9;
	TokenPlus -> cont 10;
	TokenTimes -> cont 11;
	TokenAnd -> cont 12;
	TokenOr -> cont 13;
	TokenLParen -> cont 14;
	TokenRParen -> cont 15;
	TokenVirg -> cont 16;
	TokenIf -> cont 17;
	TokenThen -> cont 18;
	TokenElse -> cont 19;
	TokenLam -> cont 20;
	TokenArrow -> cont 21;
	TokenDPontos -> cont 22;
	TokenTNum -> cont 23;
	TokenTBool -> cont 24;
	TokenVar happy_dollar_dollar -> cont 25;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 26 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
