{
{-# OPTIONS -Werror=missing-fields #-}

module Parser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import Lexer
import Location
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map )
import Data.Map ( fromList )

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- *************
-- *           *
-- * tokentype *
-- *           *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- *       *
-- * monad *
-- *       *
-- *********
%monad { Alex }

-- *********
-- *       *
-- * lexer *
-- *       *
-- *********
%lexer { lexwrap } { AlexTokenTag TokenEOF _ }

-- ***************************************************
-- * Call this function when an error is encountered *
-- ***************************************************
%error { parseError }

%token 

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

'('    { AlexTokenTag AlexRawToken_LPAREN _ }
')'    { AlexTokenTag AlexRawToken_RPAREN _ }
'['    { AlexTokenTag AlexRawToken_LBRACK _ }
']'    { AlexTokenTag AlexRawToken_RBRACK _ }
'{'    { AlexTokenTag AlexRawToken_LBRACE _ }
'}'    { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
','    { AlexTokenTag AlexRawToken_COMMA  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'id'                    { AlexTokenTag AlexRawToken_KWID            _ }
'end'                   { AlexTokenTag AlexRawToken_END             _ }
'raw'                   { AlexTokenTag AlexRawToken_RAW             _ }
'loc'                   { AlexTokenTag AlexRawToken_LOC             _ }
'Arg'                   { AlexTokenTag AlexRawToken_ARG             _ }
'var'                   { AlexTokenTag AlexRawToken_VAR             _ }
'tail'                  { AlexTokenTag AlexRawToken_TAIL            _ }
'kind'                  { AlexTokenTag AlexRawToken_KIND            _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'test'                  { AlexTokenTag AlexRawToken_TEST            _ }
'line'                  { AlexTokenTag AlexRawToken_LINE            _ }
'true'                  { AlexTokenTag AlexRawToken_TRUE            _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'body'                  { AlexTokenTag AlexRawToken_BODY            _ }
'quasis'                { AlexTokenTag AlexRawToken_QUASIS          _ }
'cooked'                { AlexTokenTag AlexRawToken_COOKED          _ }
'update'                { AlexTokenTag AlexRawToken_UPDATE          _ }
'false'                 { AlexTokenTag AlexRawToken_FALSE           _ }
'start'                 { AlexTokenTag AlexRawToken_START           _ }
'exprs'                 { AlexTokenTag AlexRawToken_EXPRS           _ }
'value'                 { AlexTokenTag AlexRawToken_VALUE           _ }
'right'                 { AlexTokenTag AlexRawToken_RIGHT           _ }
'stmts'                 { AlexTokenTag AlexRawToken_STMTS           _ }
'array'                 { AlexTokenTag AlexRawToken_ARRAY           _ }
'Param'                 { AlexTokenTag AlexRawToken_PARAM           _ }
'object'                { AlexTokenTag AlexRawToken_OBJECT          _ }
'prefix'                { AlexTokenTag AlexRawToken_PREFIX          _ }
'params'                { AlexTokenTag AlexRawToken_PARAMS          _ }
'column'                { AlexTokenTag AlexRawToken_COLUMN          _ }
'Literal'               { AlexTokenTag AlexRawToken_LITERAL         _ }
'Program'               { AlexTokenTag AlexRawToken_PROGRAM         _ }
'property'              { AlexTokenTag AlexRawToken_PROPERTY        _ }
'computed'              { AlexTokenTag AlexRawToken_COMPUTED        _ }
'operator'              { AlexTokenTag AlexRawToken_OPERATOR        _ }
'alternate'             { AlexTokenTag AlexRawToken_ALTERNATE       _ }
'consequent'            { AlexTokenTag AlexRawToken_CONSEQUENT      _ }
'argument'              { AlexTokenTag AlexRawToken_ARGUMENT        _ }
'arguments'             { AlexTokenTag AlexRawToken_ARGUMENTS       _ }
'generator'             { AlexTokenTag AlexRawToken_GENERATOR       _ }
'expression'            { AlexTokenTag AlexRawToken_EXPRESSION      _ }
'expressions'           { AlexTokenTag AlexRawToken_EXPRESSIONS     _ }
'declarations'          { AlexTokenTag AlexRawToken_DECLARATIONS    _ }
'async'                 { AlexTokenTag AlexRawToken_ASYNC           _ }
'callee'                { AlexTokenTag AlexRawToken_CALLEE          _ }
'sourceType'            { AlexTokenTag AlexRawToken_SRC_TYPE        _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'FunctionDeclaration'   { AlexTokenTag AlexRawToken_FUNCTION_DEC    _ }
'VariableDeclaration'   { AlexTokenTag AlexRawToken_VAR_DECLARATION _ }
'VariableDeclarator'    { AlexTokenTag AlexRawToken_VAR_DECLARATOR  _ }

-- *********
-- *       *
-- * other *
-- *       *
-- *********

QUOTED_INT  { AlexTokenTag AlexRawToken_QUOTED_INT  _ }
QUOTED_STR  { AlexTokenTag AlexRawToken_QUOTED_STR  _ }
QUOTED_BOOL { AlexTokenTag AlexRawToken_QUOTED_BOOL _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'NewExpression'    { AlexTokenTag AlexRawToken_EXPR_NEW    _ }
'CallExpression'   { AlexTokenTag AlexRawToken_EXPR_CALL   _ }
'MemberExpression' { AlexTokenTag AlexRawToken_EXPR_MEMBER _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_EXPR_BINOP  _ }
'UpdateExpression' { AlexTokenTag AlexRawToken_EXPR_UPDATE _ }
'AssignExpression' { AlexTokenTag AlexRawToken_EXPR_ASSIGN _ }
'LambdaExpression' { AlexTokenTag AlexRawToken_EXPR_LAMBDA _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'IfStatement'         { AlexTokenTag AlexRawToken_STMT_IF     _ }
'ForStatement'        { AlexTokenTag AlexRawToken_STMT_FOR    _ }
'BlockStatement'      { AlexTokenTag AlexRawToken_STMT_BLOCK  _ }
'ReturnStatement'     { AlexTokenTag AlexRawToken_STMT_RETURN _ }
'TemplateLiteral'     { AlexTokenTag AlexRawToken_TEMPLATE_LI _ }
'TemplateElement'     { AlexTokenTag AlexRawToken_TEMPLATE_EL _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_STMT_EXP    _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'Stmt_Echo'     { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Stmt_Expr'     { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Stmt_Function' { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'  { AlexTokenTag AlexRawToken_OP_LT       _ }
'==' { AlexTokenTag AlexRawToken_OP_EQ       _ }
'='  { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'*'  { AlexTokenTag AlexRawToken_OP_TIMES    _ }
'++' { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a):      a { [$1] } | a          listof(a) { $1:$2 }
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'{'
    'type' ':' 'Program' ','
    'body' ':' '[' commalistof(dec_or_stmt) ']' ','
    'sourceType' ':' ID ','
    'loc' ':' location
'}'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        decs = [],
        stmts = []
    }
}

-- ***************
-- *             *
-- * dec or stmt *
-- *             *
-- ***************
dec_or_stmt:
dec  { Left  $1 } |
stmt { Right $1 }

-- ***************
-- *             *
-- * dec_var_tag *
-- *             *
-- ***************
dec_var_tag:
'{'
    'type' ':' 'VariableDeclarator' ','
    'id' ':' identifier ','
    'init' ':' exp ','
    'loc' ':' location
'}'
{
    Left "DDDD"
}

-- ***********
-- *         *
-- * dec_var *
-- *         *
-- ***********
dec_var:
'{'
    'type' ':' 'VariableDeclaration' ','
    'declarations' ':' '[' commalistof(dec_var_tag) ']' ','
    'kind' ':' tokenID ','
    'loc' ':' location
'}'
{
    Left "GGGG"
}

-- *******
-- *     *
-- * dec *
-- *     *
-- *******
dec:
dec_function { $1 }

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location:
'{'
    'start' ':' '{' 'line' ':' INT ',' 'column' ':' INT '}' ','
    'end'   ':' '{' 'line' ':' INT ',' 'column' ':' INT '}'
'}'
{
    Location
    {
        Location.filename = "DDD",
        lineStart = tokIntValue $7,
        colStart = tokIntValue $11,
        lineEnd = tokIntValue $19,
        colEnd = tokIntValue $21
    }
}

-- ************
-- *          *
-- * token ID *
-- *          *
-- ************
tokenID:
ID      { tokIDValue $1 } |
'end'   { "end"         } |
'start' { "start"       }

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' tokenID ','
    'loc' ':' location
'}'
{
    Token.Named
    {
        Token.content = $8,
        Token.location = $12
    }
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' ID ','
    'loc' ':' location
'}'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named (tokIDValue $8) $12,
        Ast.paramNominalType = Token.NominalTy $ Token.Named "any" $12,
        Ast.paramSerialIdx = 156
    }
}

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var:
var
{
    ExpVar $ ExpVarContent
    {
        actualExpVar = $1
    }
}

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'{'
    'type' ':' 'BinaryExpression' ','
    'operator' ':' operator ','
    'left' ':' exp ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $12,
        Ast.expBinopRight = $16,
        Ast.expBinopOperator = $8,
        Ast.expBinopLocation = $20
    }
}

-- ******************
-- *                *
-- * field variable *
-- *                *
-- ******************
var_field:
'{'
    'type' ':' 'MemberExpression' ','
    'computed' ':' bool ','
    'object' ':' var ','
    'property' ':' identifier ','
    'loc' ':' location
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        varFieldLhs = Ast.ExpVarContent $12,
        varFieldName = Token.FieldName $16
    }
}

-- *******************
-- *                 *
-- * simple variable *
-- *                 *
-- *******************
var_simple:
identifier
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

-- ************
-- *          *
-- * variable *
-- *          *
-- ************
var:
var_simple { $1 } |
var_field  { $1 }

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign:
'{'
    'type' ':' 'AssignExpression' ','
    'operator' ':' operator ','
    'left' ':' var ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Nothing
}

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign_tag:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp_assign ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str:
'{'
    'type' ':' 'Literal' ','
    'value' ':' ID ','
    'raw' ':' QUOTED_STR ','
    'loc' ':' location
'}'
{
    Ast.ExpStr $ Ast.ExpStrContent
    {
        expStrValue = Token.ConstStr
        {
            Token.constStrValue = tokIDValue $8,
            Token.constStrLocation = $16
        }
    }
}


-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'{'
    'type' ':' 'Literal' ','
    'value' ':' INT ','
    'raw' ':' QUOTED_INT ','
    'loc' ':' location
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        expIntValue = Token.ConstInt
        {
            Token.constIntValue = tokIntValue $8,
            Token.constIntLocation = $16
        }
    }
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'{'
    'type' ':' 'Literal' ','
    'value' ':' bool ','
    'raw' ':' QUOTED_BOOL ','
    'loc' ':' location
'}'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = $8,
            Token.constBoolLocation = $16
        }
    }
}

-- ************
-- *          *
-- * exp_null *
-- *          *
-- ************
exp_null: 'null' { Nothing }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************
expressions: { [] } | commalistof(exp) { $1 }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'{'
    'type' ':' 'CallExpression' ','
    'callee' ':' exp ','
    'arguments' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $8,
        Ast.args = $13
    }
}

-- **************
-- *            *
-- * exp_lambda *
-- *            *
-- **************
exp_lambda:
'{'
    'type' ':' 'LambdaExpression' ','
    'id' ':' 'null' ','
    'params' ':' params ','
    'body' ':' stmts ','
    'generator' ':' bool ','
    'expression' ':' bool ','
    'async' ':' bool ','
    'loc' ':' location
'}'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        expLambdaParams = $12,
        expLambdaBody = $16
    }
}

-- *******************
-- *                 *
-- * fstring_element *
-- *                 *
-- *******************
fstring_element:
'{'
    'type' ':' 'TemplateElement' ','
    'value' ':' '{'
        'raw' ':' ID ','
        'cooked' ':' ID
    '}' ','
    'tail' ':' bool ','
    'loc' ':' location
'}'
{
    Nothing    
}

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'{'
    'type' ':' 'TemplateLiteral' ','
    'quasis' ':' '[' commalistof(fstring_element) ']' ','
    'expressions' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent
        {
            Ast.varName = Token.VarName $ Token.Named "fstring" $20
        },
        Ast.args = $15
    }
}

-- ***********
-- *         *
-- * exp_new *
-- *         *
-- ***********
exp_new:
'{'
    'type' ':' 'NewExpression' ','
    'callee' ':' exp ','
    'arguments' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $8,
        Ast.args = $13
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int     { $1 } |
exp_str     { $1 } |
exp_var     { $1 } |
exp_new     { $1 } |
exp_bool    { $1 } |
exp_call    { $1 } |
exp_binop   { $1 } |
exp_lambda  { $1 } |
exp_fstring { $1 }
-- exp_assign { Nothing }

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'{'
    'type' ':' 'ForStatement' ','
    'init' ':' exp ','
    'test' ':' exp ','
    'update' ':' stmt ','
    'body' ':' stmts ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
operator:
'++' { Ast.TIMES } |
'==' { Ast.TIMES } |
'*'  { Ast.TIMES } |
'<'  { Ast.TIMES } |
'='  { Ast.TIMES }

-- ********
-- *      *
-- * bool *
-- *      *
-- ********
bool:
'true'  { True  } |
'false' { False }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'{'
    'type' ':' 'ReturnStatement' ','
    'argument' ':' exp ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_update *
-- *             *
-- ***************
stmt_update:
'{'
    'type' ':' 'UpdateExpression' ','
    'operator' ':' operator ','
    'argument' ':' identifier ','
    'prefix' ':' bool ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
stmt_update    { $1 } |
exp_assign     { $1 } |
exp_assign_tag { $1 }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'{'
    'type' ':' 'IfStatement' ','
    'test' ':' exp ','
    'consequent' ':' stmts ','
    'alternate' ':' stmts ','
    'loc' ':' location
'}'
{
    Nothing
}

-- *************
-- *           *
-- * stmt_call *
-- *           *
-- *************
stmt_call:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp_call ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_decvar *
-- *             *
-- ***************
stmt_decvar: dec_var { Nothing }

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if     { $1 } |
stmt_for    { $1 } |
stmt_call   { $1 } |
stmt_assign { $1 } |
stmt_decvar { $1 } |
stmt_return { $1 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts:
'{'
    'type' ':' 'BlockStatement' ','
    'body' ':' '[' commalistof(stmt) ']' ','
    'loc' ':' location
'}'
{
    []
} |
'null' { [] }

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params: '[' ']' { [] } | '[' commalistof(param) ']' { $2 }

-- ****************
-- *              *
-- * dec_function *
-- *              *
-- ****************
dec_function: '{'
    'type' ':' 'FunctionDeclaration' ','
    'id' ':' identifier ','
    'params' ':' params ','
    'body' ':' stmts ','
    'generator' ':' bool ','
    'expression' ':' bool ','
    'async' ':' bool ','
    'loc' ':' location
'}'
{
    Left "MMM"
}

{

extractParamSingleName' :: [ Token.ParamName ] -> Maybe Token.ParamName
extractParamSingleName' ps = case ps of { [p] -> Just p; _ -> Nothing }
 
extractParamSingleName :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.ParamName
extractParamSingleName = extractParamSingleName' . lefts  

extractParamNominalType' :: [ Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType' ts = case ts of { [t] -> Just t; _ -> Nothing }
 
extractParamNominalType :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType = extractParamNominalType' . rights 

paramify :: [ Either Token.ParamName Token.NominalTy ] -> Location -> Maybe Ast.Param
paramify attrs l = let
    name = extractParamSingleName attrs
    nominalType = extractParamNominalType attrs
    in case (name, nominalType) of { (Just n, Just t) -> Just $ Ast.Param n t 0; _ -> Nothing }

getFuncNameAttr :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.FuncName
getFuncNameAttr = undefined

getFuncReturnType :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.NominalTy
getFuncReturnType = undefined

getFuncBody :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Stmt ]
getFuncBody = undefined

getFuncParams :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Param ]
getFuncParams = undefined

-- getFilename :: (Either Location Ast.Dec) -> FilePath
-- getFilename x = case x of { Left l -> Location.filename l; Right dec -> Location.filename $ Ast.locationDec dec }

-- add the /real/ serial index of the param
-- the parser just puts an arbitrary value
-- there because it lacks context
enumerateParams :: (Word,[Param]) -> [Param]
enumerateParams (_,[    ]) = []
enumerateParams (i,(p:ps)) =
    let
        n = (paramName        p)
        t = (paramNominalType p)
        head = Param { paramName = n, paramNominalType = t, paramSerialIdx = i }
        tail = (enumerateParams (i+1,ps))
    in
        head:tail

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

-- **************
-- *            *
-- * parseError *
-- *            *
-- **************
parseError :: AlexTokenTag -> Alex a
parseError t = alexError' (tokenLoc t)

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram :: FilePath -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}
