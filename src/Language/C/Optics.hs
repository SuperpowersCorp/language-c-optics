{-# LANGUAGE LambdaCase #-}

module Language.C.Optics where

import Language.C.Prelude

import Data.ByteString.Char8       ( pack )
import Data.Tuple.Curry
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Text.PrettyPrint            ( render )

_C :: Prism' ByteString CTranslUnit
_C = prism g s
  where
    g   = pack . render . pretty
    s x = first (const x) . flip parseC startPos $ x
      where
        startPos = position 0 "src" 0 0

_Ident :: Prism' Ident ([Char], Int, NodeInfo)
_Ident = iso g (uncurryN Ident)
  where
    g (Ident a b c) = (a, b, c)

_CTranslUnit :: Iso' (CTranslationUnit NodeInfo) ([CExternalDeclaration NodeInfo], NodeInfo)
_CTranslUnit = iso g (uncurry CTranslUnit)
  where
    g (CTranslUnit a b) = (a, b)

_C_Code :: Prism' ByteString ([CExternalDeclaration NodeInfo], NodeInfo)
_C_Code = _C . _CTranslUnit

_CDeclExt :: Prism' (CExternalDeclaration NodeInfo) (CDeclaration NodeInfo)
_CDeclExt = prism CDeclExt $ \case
  CDeclExt x -> Right x
  other      -> Left other

_CFDefExt :: Prism' (CExternalDeclaration NodeInfo) (CFunctionDef NodeInfo)
_CFDefExt = prism CFDefExt $ \case
  CFDefExt x -> Right x
  other      -> Left other

_CAsmExt :: Prism' (CExternalDeclaration NodeInfo) (CStringLiteral NodeInfo, NodeInfo)
_CAsmExt = prism (uncurry CAsmExt) $ \case
  CAsmExt a b -> Right (a, b)
  other       -> Left other

_CFunDef :: Iso' (CFunctionDef NodeInfo)
            ( [CDeclarationSpecifier NodeInfo]
            , CDeclarator NodeInfo
            , [CDeclaration NodeInfo]
            , CStatement NodeInfo
            , NodeInfo
            )
_CFunDef = iso g s
  where
    g (CFunDef a b c d e) = (a, b, c, d, e)
    s = uncurryN CFunDef

_CDecl :: Prism' (CDeclaration NodeInfo)
          ( [CDeclarationSpecifier NodeInfo]
          , [ ( Maybe (CDeclarator NodeInfo)
              , Maybe (CInitializer NodeInfo)
              , Maybe (CExpression NodeInfo)
              )
            ]
          , NodeInfo
          )
_CDecl = prism (uncurryN CDecl) $ \case
  (CDecl a b c) -> Right (a, b, c)
  other         -> Left other

_CStaticAssert :: Prism' (CDeclaration NodeInfo)
                  ((CExpression NodeInfo), (CStringLiteral NodeInfo), NodeInfo)
_CStaticAssert = prism g s
  where
    g = uncurryN CStaticAssert
    s = \case
      (CStaticAssert a b c) -> Right (a, b, c)
      other                 -> Left other

_CStructTag :: Prism' CStructTag ()
_CStructTag = prism (const CStructTag) $ \case
  CStructTag -> Right ()
  other      -> Left other

_CUnionTag :: Prism' CStructTag ()
_CUnionTag = prism (const CUnionTag) $ \case
  CUnionTag -> Right ()
  other     -> Left other

_CStruct :: Iso' (CStructureUnion NodeInfo)
            ( CStructTag
            , (Maybe Ident)
            , (Maybe [CDeclaration NodeInfo])
            , [CAttribute NodeInfo]
            , NodeInfo
            )
_CStruct = iso g (uncurryN CStruct)
  where
    g (CStruct a b c d e) = (a, b, c, d, e)

_CEnumeration :: Iso' (CEnumeration NodeInfo)
                 ( (Maybe Ident)
                 , (Maybe [(Ident, Maybe (CExpression NodeInfo))])
                 , [CAttribute NodeInfo]
                 , NodeInfo
                 )
_CEnumeration = iso g (uncurryN CEnum)
  where
    g (CEnum a b c d)  = (a, b, c, d)

_CInlineQual :: Prism' (CFunctionSpecifier NodeInfo) NodeInfo
_CInlineQual = prism CInlineQual $ \case
  CInlineQual a -> Right a
  other         -> Left other

_CNoreturnQual :: Prism' (CFunctionSpecifier NodeInfo) NodeInfo
_CNoreturnQual = prism CNoreturnQual $ \case
  CNoreturnQual a -> Right a
  other           -> Left other

_CStorageSpec :: Prism' (CDeclarationSpecifier NodeInfo)  (CStorageSpecifier NodeInfo)
_CStorageSpec = prism CStorageSpec $ \case
  CStorageSpec x -> Right x
  other          -> Left other

_CTypeSpec :: Prism' (CDeclarationSpecifier NodeInfo)  (CTypeSpecifier NodeInfo)
_CTypeSpec = prism CTypeSpec $ \case
  CTypeSpec x -> Right x
  other       -> Left other

_CTypeQual :: Prism' (CDeclarationSpecifier NodeInfo)  (CTypeQualifier NodeInfo)
_CTypeQual = prism CTypeQual $ \case
  CTypeQual x -> Right x
  other       -> Left other

_CFunSpec :: Prism' (CDeclarationSpecifier NodeInfo)  (CFunctionSpecifier NodeInfo)
_CFunSpec = prism CFunSpec $ \case
  CFunSpec x -> Right x
  other      -> Left other

_CAlignSpec :: Prism' (CDeclarationSpecifier NodeInfo)  (CAlignmentSpecifier NodeInfo)
_CAlignSpec = prism CAlignSpec $ \case
  CAlignSpec x -> Right x
  other        -> Left other

_CAuto :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CAuto = prism CAuto $ \case
  CAuto x -> Right x
  other   -> Left other

_CRegister :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CRegister = prism CRegister $ \case
  CRegister x -> Right x
  other       -> Left other

_CStatic :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CStatic = prism CStatic $ \case
  CStatic x -> Right x
  other     -> Left other

_CExtern :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CExtern = prism CExtern $ \case
  CExtern x -> Right x
  other     -> Left other

_CTypedef :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CTypedef = prism CTypedef $ \case
  CTypedef x -> Right x
  other      -> Left other

_CThread :: Prism' (CStorageSpecifier NodeInfo) NodeInfo
_CThread = prism CThread $ \case
  CThread x -> Right x
  other     -> Left other

_CVoidType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CVoidType = prism CVoidType $ \case
  CVoidType x -> Right x
  other       -> Left other

_CCharType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CCharType = prism CCharType $ \case
  CCharType x -> Right x
  other       -> Left other

_CShortType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CShortType = prism CShortType $ \case
  CShortType x -> Right x
  other        -> Left other

_CIntType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CIntType = prism CIntType $ \case
  CIntType x -> Right x
  other      -> Left other

_CLongType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CLongType = prism CLongType $ \case
  CLongType x -> Right x
  other       -> Left other

_CFloatType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CFloatType = prism CFloatType $ \case
  CFloatType x -> Right x
  other        -> Left other

_CDoubleType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CDoubleType = prism CDoubleType $ \case
  CDoubleType x -> Right x
  other         -> Left other

_CSignedType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CSignedType = prism CSignedType $ \case
  CSignedType x -> Right x
  other         -> Left other

_CUnsigType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CUnsigType = prism CUnsigType $ \case
  CUnsigType x -> Right x
  other        -> Left other

_CBoolType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CBoolType = prism CBoolType $ \case
  CBoolType x -> Right x
  other       -> Left other

_CComplexType :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CComplexType = prism CComplexType $ \case
  CComplexType x -> Right x
  other          -> Left other

_CInt128Type :: Prism' (CTypeSpecifier NodeInfo)  NodeInfo
_CInt128Type = prism CInt128Type $ \case
  CInt128Type x -> Right x
  other         -> Left other

-- _CFloatNType :: Prism' (CTypeSpecifier NodeInfo)  (Int, Bool, NodeInfo)
-- _CFloatNType = prism g s
--   where
--     g (a, b, c) = CFloatNType a b c
--     s = \case
--   CFloatNType a b c -> Right (a, b, c)
--   other             -> Left other

_CSUType :: Prism' (CTypeSpecifier NodeInfo)  (CStructureUnion NodeInfo, NodeInfo)
_CSUType = prism (uncurry CSUType) $ \case
  CSUType a b -> Right (a, b)
  other       -> Left other

_CEnumType :: Prism' (CTypeSpecifier NodeInfo)  (CEnumeration NodeInfo, NodeInfo)
_CEnumType = prism (uncurry CEnumType) $ \case
  CEnumType a b -> Right (a, b)
  other         -> Left other

_CTypeDef :: Prism' (CTypeSpecifier NodeInfo)  (Ident, NodeInfo)
_CTypeDef = prism (uncurry CTypeDef) $ \case
  CTypeDef a b -> Right (a, b)
  other         -> Left other

_CTypeOfExpr :: Prism' (CTypeSpecifier NodeInfo) (CExpression NodeInfo, NodeInfo)
_CTypeOfExpr = prism (uncurry CTypeOfExpr) $ \case
  CTypeOfExpr a b -> Right (a, b)
  other           -> Left other

_CTypeOfType :: Prism' (CTypeSpecifier NodeInfo)  (CDeclaration NodeInfo, NodeInfo)
_CTypeOfType = prism (uncurry CTypeOfType) $ \case
  CTypeOfType a b -> Right (a, b)
  other           -> Left other

_CAtomicType :: Prism' (CTypeSpecifier NodeInfo)  (CDeclaration NodeInfo, NodeInfo)
_CAtomicType = prism (uncurry CAtomicType) $ \case
  CAtomicType a b -> Right (a, b)
  other           -> Left other

_CAlignAsType :: Prism' (CAlignmentSpecifier NodeInfo) (CDeclaration NodeInfo, NodeInfo)
_CAlignAsType = prism (uncurry CAlignAsType) $ \case
  CAlignAsType a b -> Right (a, b)
  other            -> Left other

_CAlignAsExpr :: Prism' (CAlignmentSpecifier NodeInfo) (CExpression NodeInfo, NodeInfo)
_CAlignAsExpr = prism (uncurry CAlignAsExpr) $ \case
  CAlignAsExpr a b -> Right (a, b)
  other            -> Left other

_CConstQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CConstQual = prism CConstQual $ \case
  CConstQual x -> Right x
  other        -> Left other

_CVolatQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CVolatQual = prism CVolatQual $ \case
  CVolatQual x -> Right x
  other        -> Left other

_CRestrQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CRestrQual = prism CRestrQual $ \case
  CRestrQual x -> Right x
  other        -> Left other

_CAtomicQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CAtomicQual = prism CAtomicQual $ \case
  CAtomicQual x -> Right x
  other         -> Left other

_CAttrQual :: Prism' (CTypeQualifier NodeInfo) (CAttribute NodeInfo)
_CAttrQual = prism CAttrQual $ \case
  CAttrQual x -> Right x
  other       -> Left other

_CNullableQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CNullableQual = prism CNullableQual $ \case
  CNullableQual x -> Right x
  other           -> Left other

_CNonnullQual :: Prism' (CTypeQualifier NodeInfo) NodeInfo
_CNonnullQual = prism CNonnullQual $ \case
  CNonnullQual x -> Right x
  other          -> Left other

_CAttr :: Iso' (CAttribute NodeInfo) (Ident, [CExpression NodeInfo], NodeInfo)
_CAttr = iso g s
  where
    g (CAttr a b c) = (a, b, c)
    s = uncurryN CAttr

_CDeclr :: Prism' (CDeclarator NodeInfo)
           ( Maybe Ident
           , [CDerivedDeclarator NodeInfo]
           , (Maybe (CStringLiteral NodeInfo))
           , [CAttribute NodeInfo]
           , NodeInfo
           )
_CDeclr = iso g s
  where
    g (CDeclr a b c d e) = (a, b, c, d, e)
    s = uncurryN CDeclr

_CPtrDeclr :: Prism' (CDerivedDeclarator NodeInfo)
              ([CTypeQualifier NodeInfo], NodeInfo)
_CPtrDeclr = prism g s
  where
    g = uncurry CPtrDeclr
    s = \case
      CPtrDeclr a b -> Right (a, b)
      other         -> Left other

_CArrDeclr :: Prism' (CDerivedDeclarator NodeInfo)
              ([CTypeQualifier NodeInfo], CArraySize NodeInfo, NodeInfo)
_CArrDeclr = prism g s
  where
    g = uncurryN CArrDeclr
    s = \case
      CArrDeclr a b c -> Right (a, b, c)
      other           -> Left other

_CFunDeclr :: Prism' (CDerivedDeclarator NodeInfo)
              ( (Either [Ident] ([CDeclaration NodeInfo], Bool))
              , [CAttribute NodeInfo]
              , NodeInfo
              )
_CFunDeclr = prism g s
  where
    g = uncurryN CFunDeclr
    s = \case
      CFunDeclr a b c -> Right (a, b, c)
      other           -> Left other

_CNoArrSize :: Prism' (CArraySize NodeInfo) Bool
_CNoArrSize = prism CNoArrSize $ \case
  CNoArrSize a -> Right a
  other        -> Left other

_CArrSize :: Prism' (CArraySize NodeInfo) (Bool, CExpression NodeInfo)
_CArrSize = prism (uncurry CArrSize) $ \case
  CArrSize a b -> Right (a, b)
  other        -> Left other

_CInitExpr :: Prism' (CInitializer NodeInfo) (CExpression NodeInfo, NodeInfo)
_CInitExpr = prism (uncurry CInitExpr) $ \case
  CInitExpr a b -> Right (a, b)
  other         -> Left other

_CInitList :: Prism' (CInitializer NodeInfo) (CInitializerList NodeInfo, NodeInfo)
_CInitList = prism (uncurry CInitList) $ \case
  CInitList a b -> Right (a, b)
  other         -> Left other

_CArrDesig :: Prism' (CPartDesignator NodeInfo) (CExpression NodeInfo, NodeInfo)
_CArrDesig = prism (uncurry CArrDesig) $ \case
  CArrDesig a b -> Right (a, b)
  other         -> Left other

_CMemberDesig :: Prism' (CPartDesignator NodeInfo) (Ident, NodeInfo)
_CMemberDesig = prism (uncurry CMemberDesig) $ \case
  CMemberDesig a b -> Right (a, b)
  other            -> Left other

_CRangeDesig :: Prism' (CPartDesignator NodeInfo) (CExpression NodeInfo, CExpression NodeInfo, NodeInfo)
_CRangeDesig = prism (uncurryN CRangeDesig) $ \case
  CRangeDesig a b c -> Right (a, b, c)
  other             -> Left other

_CLabel :: Prism' (CStatement NodeInfo) (Ident, CStatement NodeInfo, [CAttribute NodeInfo], NodeInfo)
_CLabel = prism (uncurryN CLabel) $ \case
  CLabel a b c d -> Right (a, b, c, d)
  other          -> Left other

_CCase :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, CStatement NodeInfo, NodeInfo)
_CCase = prism (uncurryN CCase) $ \case
  CCase a b c -> Right (a, b, c)
  other       -> Left other

_CCases :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, CExpression NodeInfo,  CStatement NodeInfo, NodeInfo)
_CCases = prism (uncurryN CCases) $ \case
  CCases a b c d -> Right (a, b, c, d)
  other          -> Left other

_CDefault :: Prism' (CStatement NodeInfo) (CStatement NodeInfo, NodeInfo)
_CDefault = prism (uncurryN CDefault) $ \case
  CDefault a b -> Right (a, b)
  other        -> Left other

_CExpr :: Prism' (CStatement NodeInfo) (Maybe (CExpression NodeInfo), NodeInfo)
_CExpr = prism (uncurryN CExpr) $ \case
  CExpr a b -> Right (a, b)
  other     -> Left other

_CCompound :: Prism' (CStatement NodeInfo) ([Ident], [CCompoundBlockItem NodeInfo], NodeInfo)
_CCompound = prism (uncurryN CCompound) $ \case
  CCompound a b c -> Right (a, b, c)
  other           -> Left other

_CIf :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, CStatement NodeInfo, Maybe (CStatement NodeInfo), NodeInfo)
_CIf = prism (uncurryN CIf) $ \case
  CIf a b c d -> Right (a, b, c, d)
  other       -> Left other

_CSwitch :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, CStatement NodeInfo, NodeInfo)
_CSwitch = prism (uncurryN CSwitch) $ \case
  CSwitch a b c -> Right (a, b, c)
  other         -> Left other

_CWhile :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, CStatement NodeInfo, Bool, NodeInfo)
_CWhile = prism (uncurryN CWhile) $ \case
  CWhile a b c d -> Right (a, b, c, d)
  other          -> Left other

_CFor :: Prism' (CStatement NodeInfo) ( (Either (Maybe (CExpression NodeInfo)) (CDeclaration NodeInfo))
                                      , Maybe (CExpression NodeInfo)
                                      , Maybe (CExpression NodeInfo)
                                      , CStatement NodeInfo
                                      , NodeInfo
                                      )
_CFor = prism (uncurryN CFor) $ \case
  CFor a b c d e -> Right (a, b, c, d, e)
  other          -> Left other

_CGoto :: Prism' (CStatement NodeInfo) (Ident, NodeInfo)
_CGoto = prism (uncurryN CGoto) $ \case
  CGoto a b -> Right (a, b)
  other     -> Left other

_CGotoPtr :: Prism' (CStatement NodeInfo) (CExpression NodeInfo, NodeInfo)
_CGotoPtr = prism (uncurryN CGotoPtr) $ \case
  CGotoPtr a b -> Right (a, b)
  other        -> Left other

_CCont :: Prism' (CStatement NodeInfo) NodeInfo
_CCont = prism CCont $ \case
  CCont a -> Right a
  other   -> Left other

_CBreak :: Prism' (CStatement NodeInfo) NodeInfo
_CBreak = prism CBreak $ \case
  CBreak a -> Right a
  other    -> Left other

_CReturn :: Prism' (CStatement NodeInfo) (Maybe (CExpression NodeInfo), NodeInfo)
_CReturn = prism (uncurry CReturn) $ \case
  CReturn a b -> Right (a, b)
  other       -> Left other

_CAsm :: Prism' (CStatement NodeInfo) (CAssemblyStatement NodeInfo, NodeInfo)
_CAsm = prism (uncurry CAsm) $ \case
  CAsm a b -> Right (a, b)
  other    -> Left other

_CBlockStmt :: Prism' (CCompoundBlockItem NodeInfo) (CStatement NodeInfo)
_CBlockStmt = prism CBlockStmt $ \case
  CBlockStmt a -> Right a
  other        -> Left other

_CBlockDecl :: Prism' (CCompoundBlockItem NodeInfo) (CDeclaration NodeInfo)
_CBlockDecl = prism CBlockDecl $ \case
  CBlockDecl a -> Right a
  other        -> Left other

_CNestedFunDef :: Prism' (CCompoundBlockItem NodeInfo) (CFunctionDef NodeInfo)
_CNestedFunDef = prism CNestedFunDef $ \case
  CNestedFunDef a -> Right a
  other           -> Left other

_CAsmStmt :: Iso' (CAssemblyStatement NodeInfo)
             ( Maybe (CTypeQualifier NodeInfo)
             , CStringLiteral NodeInfo
             , [CAssemblyOperand NodeInfo]
             , [CAssemblyOperand NodeInfo]
             , [CStringLiteral NodeInfo]
             , NodeInfo
             )
_CAsmStmt = iso g (uncurryN CAsmStmt)
  where
    g (CAsmStmt a b c d e f) = (a, b, c, d, e, f)

_CAsmOperand :: Iso' (CAssemblyOperand NodeInfo)
                ( Maybe Ident
                , CStringLiteral NodeInfo
                , CExpression NodeInfo
                , NodeInfo
                )
_CAsmOperand = iso g (uncurryN CAsmOperand)
  where
    g (CAsmOperand a b c d) = (a, b, c, d)

_CComma :: Prism' (CExpression NodeInfo) ([CExpression NodeInfo], NodeInfo)
_CComma = prism (uncurryN CComma) $ \case
  CComma a b -> Right (a, b)
  other      -> Left other

_CAssign :: Prism' (CExpression NodeInfo) (CAssignOp, CExpression NodeInfo, CExpression NodeInfo, NodeInfo)
_CAssign = prism (uncurryN CAssign) $ \case
  CAssign a b c d -> Right (a, b, c, d)
  other           -> Left other

_CCond :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, Maybe (CExpression NodeInfo), CExpression NodeInfo, NodeInfo)
_CCond = prism (uncurryN CCond) $ \case
  CCond a b c d -> Right (a, b, c, d)
  other         -> Left other

_CBinary :: Prism' (CExpression NodeInfo) (CBinaryOp, CExpression NodeInfo, CExpression NodeInfo, NodeInfo)
_CBinary = prism (uncurryN CBinary) $ \case
  CBinary a b c d -> Right (a, b, c, d)
  other           -> Left other

_CCast :: Prism' (CExpression NodeInfo) (CDeclaration NodeInfo, CExpression NodeInfo, NodeInfo)
_CCast = prism (uncurryN CCast) $ \case
  CCast a b c -> Right (a, b, c)
  other       -> Left other

_CUnary :: Prism' (CExpression NodeInfo) (CUnaryOp, CExpression NodeInfo, NodeInfo)
_CUnary = prism (uncurryN CUnary) $ \case
  CUnary a b c -> Right (a, b, c)
  other        -> Left other

_CSizeofExpr :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, NodeInfo)
_CSizeofExpr = prism (uncurryN CSizeofExpr) $ \case
  CSizeofExpr a b -> Right (a, b)
  other           -> Left other

_CSizeofType :: Prism' (CExpression NodeInfo) (CDeclaration NodeInfo, NodeInfo)
_CSizeofType = prism (uncurryN CSizeofType) $ \case
  CSizeofType a b -> Right (a, b)
  other           -> Left other

_CAlignofExpr :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, NodeInfo)
_CAlignofExpr = prism (uncurryN CAlignofExpr) $ \case
  CAlignofExpr a b -> Right (a, b)
  other            -> Left other

_CAlignofType :: Prism' (CExpression NodeInfo) (CDeclaration NodeInfo, NodeInfo)
_CAlignofType = prism (uncurryN CAlignofType) $ \case
  CAlignofType a b -> Right (a, b)
  other            -> Left other

_CComplexReal :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, NodeInfo)
_CComplexReal = prism (uncurryN CComplexReal) $ \case
  CComplexReal a b -> Right (a, b)
  other            -> Left other

_CComplexImag :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, NodeInfo)
_CComplexImag = prism (uncurryN CComplexImag) $ \case
  CComplexImag a b -> Right (a, b)
  other            -> Left other

_CIndex :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, CExpression NodeInfo, NodeInfo)
_CIndex = prism (uncurryN CIndex) $ \case
  CIndex a b c -> Right (a, b, c)
  other        -> Left other

_CCall :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, [CExpression NodeInfo], NodeInfo)
_CCall = prism (uncurryN CCall) $ \case
  CCall a b c -> Right (a, b, c)
  other       -> Left other

_CMember :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, Ident, Bool, NodeInfo)
_CMember = prism (uncurryN CMember) $ \case
  CMember a b c d -> Right (a, b, c, d)
  other           -> Left other

_CVar :: Prism' (CExpression NodeInfo) (Ident, NodeInfo)
_CVar = prism (uncurryN CVar) $ \case
  CVar a b -> Right (a, b)
  other    -> Left other

_CConst :: Prism' (CExpression NodeInfo) (CConstant NodeInfo)
_CConst = prism CConst $ \case
  CConst a -> Right a
  other    -> Left other

_CCompoundLit :: Prism' (CExpression NodeInfo) (CDeclaration NodeInfo, CInitializerList NodeInfo, NodeInfo)
_CCompoundLit = prism (uncurryN CCompoundLit) $ \case
  CCompoundLit a b c -> Right (a, b, c)
  other              -> Left other

_CGenericSelection :: Prism' (CExpression NodeInfo) (CExpression NodeInfo, [(Maybe (CDeclaration NodeInfo), CExpression NodeInfo)], NodeInfo)
_CGenericSelection = prism (uncurryN CGenericSelection) $ \case
  CGenericSelection a b c -> Right (a, b, c)
  other                   -> Left other

_CStatExpr :: Prism' (CExpression NodeInfo) (CStatement NodeInfo, NodeInfo)
_CStatExpr = prism (uncurryN CStatExpr) $ \case
  CStatExpr a b -> Right (a, b)
  other         -> Left other

_CLabAddrExpr :: Prism' (CExpression NodeInfo) (Ident, NodeInfo)
_CLabAddrExpr = prism (uncurryN CLabAddrExpr) $ \case
  CLabAddrExpr a b -> Right (a, b)
  other            -> Left other

_CBuiltinExpr :: Prism' (CExpression NodeInfo) (CBuiltinThing NodeInfo)
_CBuiltinExpr = prism CBuiltinExpr $ \case
  CBuiltinExpr a -> Right a
  other          -> Left other

_CAssignOp :: Prism' CAssignOp ()
_CAssignOp = prism (const CAssignOp) $ \case
  CAssignOp -> Right ()
  other     -> Left other

_CMulAssOp :: Prism' CAssignOp ()
_CMulAssOp = prism (const CMulAssOp) $ \case
  CMulAssOp -> Right ()
  other     -> Left other

_CDivAssOp :: Prism' CAssignOp ()
_CDivAssOp = prism (const CDivAssOp) $ \case
  CDivAssOp -> Right ()
  other     -> Left other

_CRmdAssOp :: Prism' CAssignOp ()
_CRmdAssOp = prism (const CRmdAssOp) $ \case
  CRmdAssOp -> Right ()
  other     -> Left other

_CAddAssOp :: Prism' CAssignOp ()
_CAddAssOp = prism (const CAddAssOp) $ \case
  CAddAssOp -> Right ()
  other     -> Left other

_CSubAssOp :: Prism' CAssignOp ()
_CSubAssOp = prism (const CSubAssOp) $ \case
  CSubAssOp -> Right ()
  other     -> Left other

_CShlAssOp :: Prism' CAssignOp ()
_CShlAssOp = prism (const CShlAssOp) $ \case
  CShlAssOp -> Right ()
  other     -> Left other

_CShrAssOp :: Prism' CAssignOp ()
_CShrAssOp = prism (const CShrAssOp) $ \case
  CShrAssOp -> Right ()
  other     -> Left other

_CAndAssOp :: Prism' CAssignOp ()
_CAndAssOp = prism (const CAndAssOp) $ \case
  CAndAssOp -> Right ()
  other     -> Left other

_CXorAssOp :: Prism' CAssignOp ()
_CXorAssOp = prism (const CXorAssOp) $ \case
  CXorAssOp -> Right ()
  other     -> Left other

_COrAssOp  :: Prism' CAssignOp ()
_COrAssOp = prism (const COrAssOp) $ \case
  COrAssOp -> Right ()
  other    -> Left other

_CMulOp ::  Prism' CBinaryOp ()
_CMulOp = prism (const CMulOp) $ \case
  CMulOp -> Right ()
  other  -> Left other

_CDivOp :: Prism' CBinaryOp ()
_CDivOp = prism (const CDivOp) $ \case
  CDivOp -> Right ()
  other  -> Left other

_CRmdOp :: Prism' CBinaryOp ()
_CRmdOp = prism (const CRmdOp) $ \case
  CRmdOp -> Right ()
  other  -> Left other

_CAddOp :: Prism' CBinaryOp ()
_CAddOp = prism (const CAddOp) $ \case
  CAddOp -> Right ()
  other  -> Left other

_CSubOp :: Prism' CBinaryOp ()
_CSubOp = prism (const CSubOp) $ \case
  CSubOp -> Right ()
  other  -> Left other

_CShlOp :: Prism' CBinaryOp ()
_CShlOp = prism (const CShlOp) $ \case
  CShlOp -> Right ()
  other  -> Left other

_CShrOp :: Prism' CBinaryOp ()
_CShrOp = prism (const CShrOp) $ \case
  CShrOp -> Right ()
  other  -> Left other

_CLeOp :: Prism' CBinaryOp ()
_CLeOp = prism (const CLeOp) $ \case
  CLeOp -> Right ()
  other -> Left other

_CGrOp :: Prism' CBinaryOp ()
_CGrOp = prism (const CGrOp) $ \case
  CGrOp -> Right ()
  other -> Left other

_CLeqOp :: Prism' CBinaryOp ()
_CLeqOp = prism (const CLeqOp) $ \case
  CLeqOp -> Right ()
  other  -> Left other

_CGeqOp :: Prism' CBinaryOp ()
_CGeqOp = prism (const CGeqOp) $ \case
  CGeqOp -> Right ()
  other  -> Left other

_CEqOp :: Prism' CBinaryOp ()
_CEqOp = prism (const CEqOp) $ \case
  CEqOp -> Right ()
  other -> Left other

_CNeqOp :: Prism' CBinaryOp ()
_CNeqOp = prism (const CNeqOp) $ \case
  CNeqOp -> Right ()
  other  -> Left other

_CAndOp :: Prism' CBinaryOp ()
_CAndOp = prism (const CAndOp) $ \case
  CAndOp -> Right ()
  other  -> Left other

_CXorOp :: Prism' CBinaryOp ()
_CXorOp = prism (const CXorOp) $ \case
  CXorOp -> Right ()
  other  -> Left other

_COrOp :: Prism' CBinaryOp ()
_COrOp = prism (const COrOp) $ \case
  COrOp -> Right ()
  other -> Left other

_CLndOp :: Prism' CBinaryOp ()
_CLndOp = prism (const CLndOp) $ \case
  CLndOp -> Right ()
  other  -> Left other

_CLorOp ::  Prism' CBinaryOp ()
_CLorOp = prism (const CLorOp) $ \case
  CLorOp -> Right ()
  other  -> Left other

_CPreIncOp :: Prism' CUnaryOp ()
_CPreIncOp = prism (const CPreIncOp) $ \case
  CPreIncOp -> Right ()
  other     -> Left other

_CPreDecOp :: Prism' CUnaryOp ()
_CPreDecOp = prism (const CPreDecOp) $ \case
  CPreDecOp -> Right ()
  other     -> Left other

_CPostIncOp :: Prism' CUnaryOp ()
_CPostIncOp = prism (const CPostIncOp) $ \case
  CPostIncOp -> Right ()
  other      -> Left other

_CPostDecOp :: Prism' CUnaryOp ()
_CPostDecOp = prism (const CPostDecOp) $ \case
  CPostDecOp -> Right ()
  other      -> Left other

_CAdrOp :: Prism' CUnaryOp ()
_CAdrOp = prism (const CAdrOp) $ \case
  CAdrOp -> Right ()
  other  -> Left other

_CIndOp :: Prism' CUnaryOp ()
_CIndOp = prism (const CIndOp) $ \case
  CIndOp -> Right ()
  other  -> Left other

_CPlusOp :: Prism' CUnaryOp ()
_CPlusOp = prism (const CPlusOp) $ \case
  CPlusOp -> Right ()
  other   -> Left other

_CMinOp :: Prism' CUnaryOp ()
_CMinOp = prism (const CMinOp) $ \case
  CMinOp -> Right ()
  other  -> Left other

_CCompOp :: Prism' CUnaryOp ()
_CCompOp = prism (const CCompOp) $ \case
  CCompOp -> Right ()
  other   -> Left other

_CNegOp :: Prism' CUnaryOp ()
_CNegOp = prism (const CNegOp) $ \case
  CNegOp -> Right ()
  other  -> Left other

_CBuiltinVaArg :: Prism' (CBuiltinThing NodeInfo) (CExpression NodeInfo, CDeclaration NodeInfo, NodeInfo)
_CBuiltinVaArg = prism (uncurryN CBuiltinVaArg) $ \case
  CBuiltinVaArg a b c -> Right (a, b, c)
  other               -> Left other

_CBuiltinOffsetOf :: Prism' (CBuiltinThing NodeInfo) (CDeclaration NodeInfo, [CPartDesignator NodeInfo], NodeInfo)
_CBuiltinOffsetOf = prism (uncurryN CBuiltinOffsetOf) $ \case
  CBuiltinOffsetOf a b c -> Right (a, b, c)
  other                  -> Left other

_CBuiltinTypesCompatible :: Prism' (CBuiltinThing NodeInfo) (CDeclaration NodeInfo, CDeclaration NodeInfo, NodeInfo)
_CBuiltinTypesCompatible = prism (uncurryN CBuiltinTypesCompatible) $ \case
  CBuiltinTypesCompatible a b c -> Right (a, b, c)
  other                         -> Left other

-- _CBuiltinConvertVector :: Prism' (CBuiltinThing NodeInfo) (CExpression NodeInfo, CDeclaration NodeInfo, NodeInfo)
-- _CBuiltinConvertVector = prism (uncurryN CBuiltinConvertVector) s
--   where
--     s = \case
--       CBuiltinConvertVector a b c -> Right (a, b, c)
--       other -> Left other

_CIntConst :: Prism' (CConstant NodeInfo) (CInteger, NodeInfo)
_CIntConst = prism (uncurryN CIntConst) $ \case
  CIntConst a b -> Right (a, b)
  other         -> Left other

_CCharConst :: Prism' (CConstant NodeInfo) (CChar, NodeInfo)
_CCharConst = prism (uncurryN CCharConst) $ \case
  CCharConst a b -> Right (a, b)
  other          -> Left other

_CFloatConst :: Prism' (CConstant NodeInfo) (CFloat, NodeInfo)
_CFloatConst = prism (uncurryN CFloatConst) $ \case
  CFloatConst a b -> Right (a, b)
  other           -> Left other

_CStrConst :: Prism' (CConstant NodeInfo) (CString, NodeInfo)
_CStrConst = prism (uncurryN CStrConst) $ \case
  CStrConst a b -> Right (a, b)
  other         -> Left other

_CStrLit :: Iso' (CStringLiteral NodeInfo) (CString, NodeInfo)
_CStrLit = iso g (uncurryN CStrLit)
  where
    g (CStrLit a b) = (a, b)
