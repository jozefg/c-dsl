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

char :: CDeclr -> Maybe CExpr -> CDecl
-- ^ A short cut for declaring a @char@.
-- >     char "x" .= 1
-- >     uninit $ char "y"
char   = decl charTy

short :: CDeclr -> Maybe CExpr -> CDecl
short  = decl shortTy

int :: CDeclr -> Maybe CExpr -> CDecl
int    = decl intTy

long :: CDeclr -> Maybe CExpr -> CDecl
long   = decl longTy

float :: CDeclr -> Maybe CExpr -> CDecl
float  = decl floatTy

double :: CDeclr -> Maybe CExpr -> CDecl
double = decl doubleTy

-- | Equivalent to 'char' but wraps the 'CDeclr' in a pointer.
-- This means that @charPtr "x" .= ...@ is equivalent to @char *x = ..."
charPtr  :: CDeclr -> Maybe CExpr -> CDecl
charPtr   = char . ptr

shortPtr :: CDeclr -> Maybe CExpr -> CDecl
shortPtr  = short . ptr

intPtr   :: CDeclr -> Maybe CExpr -> CDecl
intPtr    = int . ptr

longPtr  :: CDeclr -> Maybe CExpr -> CDecl
longPtr   = long . ptr

floatPtr :: CDeclr -> Maybe CExpr -> CDecl
floatPtr  = float . ptr

doublePtr:: CDeclr -> Maybe CExpr -> CDecl
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

-- | Create a structure, for example @struct "foo" [("bar", intTy)]@ is
-- @typedef struct foo {int bar;} foo;@
struct ::  String -> [(String, CTypeSpec)] -> CDecl
struct = csu CStructTag

-- | Equivalent to 'struct' but generates a C union instead.
union :: String -> [(String, CTypeSpec)] -> CDecl
union  = csu CUnionTag

-- | Defines a C function. For example
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

