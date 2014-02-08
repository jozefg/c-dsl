{-# LANGUAGE FlexibleInstances #-}
module Language.C.DSL.Exp where
import Language.C
import Data.String
import Language.C.DSL.StringLike

str :: String -> CExpr
str = CConst . flip CStrConst undefNode . cString
    
cOp :: CBinaryOp -> CExpr -> CExpr -> CExpr
cOp op a b = CBinary op a b undefNode

-- Comparison operators, normal haskell operators + :
(==:), (/=:), (<:), (>:), (<=:), (>=:) :: CExpr -> CExpr -> CExpr
(==:) = cOp CEqOp
(/=:) = cOp CNeqOp
(<:)  = cOp CLeOp
(>:)  = cOp CGrOp
(<=:) = cOp CLeqOp
(>=:) = cOp CGeqOp

ternary :: CExpr -> CExpr -> CExpr -> CExpr
ternary i t e = CCond i (Just t) e undefNode

instance Num CExpr where
  fromInteger = CConst . flip CIntConst undefNode . cInteger
  (*)         = cOp CMulOp
  (+)         = cOp CAddOp
  (-)         = cOp CSubOp
  abs a       = ternary (a >=: 0) a (negate a)
  signum a    = ternary (a >=: 0) (ternary (a ==: 0) a 1) (-1)
instance Fractional CExpr where
  (/)          = cOp CDivOp
  fromRational = CConst . flip CFloatConst undefNode . cFloat . fromRational

var :: Ident -> CExpr
var = flip CVar undefNode

-- | Function calls
(#) :: CExpr -> [CExpr] -> CExpr
f # args = CCall f args undefNode

(<--) :: CExpr -> CExpr -> CExpr
var <-- val = CAssign CAssignOp var val undefNode
infixl 3 <--

assign :: CAssignOp -> CExpr -> CExpr -> CExpr
assign mode var val = CAssign mode var val undefNode


data UnOp = PlusPlus
          | MinusMinus
          | Minus
          | Plus
          | Not
          | Addr
          | Ind
          deriving(Eq, Show)

toCUnaryOp :: UnOp -> CUnaryOp
toCUnaryOp Minus = CMinOp
toCUnaryOp Plus  = CPlusOp
toCUnaryOp Not   = CNegOp
toCUnaryOp Addr  = CAdrOp
toCUnaryOp Ind   = CIndOp

pre   :: UnOp -> CExpr -> CExpr
PlusPlus   `pre` exp = CUnary CPreIncOp exp undefNode
MinusMinus `pre` exp = CUnary CPreDecOp exp undefNode
op         `pre` exp = CUnary (toCUnaryOp op) exp undefNode

star :: CExpr -> CExpr
star = pre Ind

post  :: CExpr -> UnOp -> CExpr
exp `post` PlusPlus   = CUnary CPostIncOp exp undefNode
exp `post` MinusMinus = CUnary CPostDecOp exp undefNode
exp `post` op         = CUnary (toCUnaryOp op) exp undefNode


comma :: [CExpr] -> CExpr
comma = flip CComma undefNode

castTo :: CExpr -> CDecl -> CExpr
exp `castTo` ty = CCast ty exp undefNode

sizeOfDecl :: CDecl -> CExpr
sizeOfDecl = flip CSizeofType undefNode

sizeOf :: CExpr -> CExpr
sizeOf = flip CSizeofExpr undefNode

(&) :: CExpr -> String -> CExpr
struct & field = CMember struct (fromString field) False undefNode
infixl 8 &

(&*) :: CExpr -> String -> CExpr
struct &* field = CMember struct (fromString field) True undefNode
infixl 8 &*

(!) :: CExpr -> CExpr -> CExpr
arr ! ind = CIndex arr ind undefNode
infixl 8 !
