{-# LANGUAGE FlexibleInstances #-}
module Language.C.DSL.Decl where
import Language.C
import Data.String
import Language.C.DSL.StringLike


decl :: CDeclSpec -> CDeclr -> Maybe CExpr -> CDecl
decl ty name exp = CDecl [ty] [(Just name, flip CInitExpr undefNode `fmap` exp, Nothing)] undefNode

voidTy, charTy, shortTy, intTy, longTy, floatTy :: CDeclSpec
voidTy   = CTypeSpec $ CVoidType undefNode
charTy   = CTypeSpec $ CCharType undefNode
shortTy  = CTypeSpec $ CShortType undefNode
intTy    = CTypeSpec $ CIntType undefNode
longTy   = CTypeSpec $ CLongType undefNode
floatTy  = CTypeSpec $ CFloatType undefNode
doubleTy = CTypeSpec $ CDoubleType undefNode

ptr :: CDeclr -> CDeclr
ptr (CDeclr nm mods cstr attrs node) = CDeclr nm (CPtrDeclr [] undefNode : mods) cstr attrs node

char, short, int, long, float, double :: CDeclr -> Maybe CExpr -> CDecl
char   = decl charTy
short  = decl shortTy
int    = decl intTy
long   = decl longTy
float  = decl floatTy
double = decl doubleTy

charPtr, shortPtr, intPtr, longPtr, floatPtr, doublePtr :: CDeclr -> Maybe CExpr -> CDecl
charPtr   = char . ptr
shortPtr  = short . ptr
intPtr    = int . ptr
longPtr   = long . ptr
floatPtr  = float . ptr
doublePtr = double . ptr


(.=) :: (Maybe CExpr -> CDecl) -> CExpr -> CDecl
f .= e = f (Just e)
infixl 7 .=

uninit :: (Maybe CExpr -> CDecl) -> CDecl
uninit = ($ Nothing)

csu :: CStructTag -> String -> [(String, CTypeSpec)] -> CDecl
csu tag ident fields = CDecl
                       [CStorageSpec $ CTypedef undefNode, CTypeSpec $ CSUType structTy undefNode]
                       [(Just $ fromString ident, Nothing, Nothing)]
                       undefNode
  where structTy  = CStruct tag (Just $ fromString ident) (Just $ map structify fields) [] undefNode
        structify (name, ty) = CDecl [CTypeSpec ty] [(Just (fromString name), Nothing, Nothing)] undefNode

struct, union :: String -> [(String, CTypeSpec)] -> CDecl
struct = csu CStructTag
union  = csu CUnionTag

fun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> CStat -> CFunDef
fun specs name args body = CFunDef specs decl [] body undefNode
  where decl =CDeclr (Just $ fromString name)
               [CFunDeclr (Right (fmap ($Nothing) args, False)) [] undefNode]
               Nothing [] undefNode

class External a where
  export :: a -> CExtDecl
instance External CFunDef where
  export = CFDefExt
instance External CDecl where
  export = CDeclExt
instance External CStrLit where
  export = flip CAsmExt undefNode

transUnit :: [CExtDecl] -> CTranslUnit
transUnit = flip CTranslUnit undefNode

