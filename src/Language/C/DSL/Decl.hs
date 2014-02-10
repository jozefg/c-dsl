{-# LANGUAGE FlexibleInstances #-}
module Language.C.DSL.Decl where
import Language.C
import Data.String
import Language.C.DSL.StringLike

-- | A low level way to declare something.
decl :: CDeclSpec -- ^ The declaration specifier, usually this is a type
        -> CDeclr -- ^ Equivalent to the name of the object being declared. Often this will
                  -- make use of the overloaded string instance for 'CDeclr's
        -> Maybe CExpr -- ^ The optional init expression
        -> CDecl
decl ty name exp = CDecl [ty] [(Just name, flip CInitExpr undefNode `fmap` exp, Nothing)] undefNode

-- | Simple types that can be used in declarations.
voidTy, charTy, shortTy, intTy, longTy, floatTy :: CDeclSpec
voidTy   = CTypeSpec $ CVoidType undefNode
charTy   = CTypeSpec $ CCharType undefNode
shortTy  = CTypeSpec $ CShortType undefNode
intTy    = CTypeSpec $ CIntType undefNode
longTy   = CTypeSpec $ CLongType undefNode
floatTy  = CTypeSpec $ CFloatType undefNode
doubleTy = CTypeSpec $ CDoubleType undefNode

-- | Modifies a declarator to be a pointer. For example
-- @ptr "x"@ would be @*x@ in C.
ptr :: CDeclr -> CDeclr
ptr (CDeclr nm mods cstr attrs node) = CDeclr nm (CPtrDeclr [] undefNode : mods) cstr attrs node

-- | Clever functions that can be applied to declarators, for example @int "x" .= 1@ is equivalent
-- to @int x = 1@. To leave a variable uninitialized @int "x" Nothing@ does the job.
char, short, int, long, float, double :: CDeclr -> Maybe CExpr -> CDecl
char   = decl charTy
short  = decl shortTy
int    = decl intTy
long   = decl longTy
float  = decl floatTy
double = decl doubleTy

-- | Equivalent to the above but with pointer wrappers.
charPtr, shortPtr, intPtr, longPtr, floatPtr, doublePtr :: CDeclr -> Maybe CExpr -> CDecl
charPtr   = char . ptr
shortPtr  = short . ptr
intPtr    = int . ptr
longPtr   = long . ptr
floatPtr  = float . ptr
doublePtr = double . ptr

-- | Supplies an initializer for an expression.
(.=) :: (Maybe CExpr -> CDecl) -> CExpr -> CDecl
f .= e = f (Just e)
infixl 7 .=

-- | Leave a declaration uninitialized. For example @uninit $ char "x"@.
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
-- ^ Create a structure or union, for example @struct "foo" [("bar", intTy)]@ is
-- @typedef struct foo {int bar;} foo;@
struct = csu CStructTag
union  = csu CUnionTag

-- Defines a C function. For example
-- >    test =
-- >       fun [intTy] "test"[int "a", int "b"] $ hblock [
-- >           creturn ("a" + "b")
-- >       ]
-- Would be the equivalent of
-- >   int test(int a, int b)
-- >   {
-- >      return a + b;
-- >   }
fun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> CStat -> CFunDef
fun specs name args body = CFunDef specs decl [] body undefNode
  where decl = CDeclr (Just $ fromString name)
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

-- | Exports a series of declarations to a translation unit.
transUnit :: [CExtDecl] -> CTranslUnit
transUnit = flip CTranslUnit undefNode

