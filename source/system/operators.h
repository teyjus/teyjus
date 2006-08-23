#ifndef OPERATORS_H
#define OPERATORS_H

#include <limits.h>

/* Fixity types */
typedef  enum OP_FixityType
{ 
    OP_INFIX    = 0, 
    OP_INFIXL   = 1, 
    OP_INFIXR   = 2,            
    OP_NONE     = 3, 
    OP_PREFIX   = 4, 
    OP_PREFIXR  = 5, 
    OP_POSTFIX  = 6, 
    OP_POSTFIXL = 7 
}  OP_FixityType;



//usful ?
/*
#define  OP_CCOMMA_FIXITY  OP_infixr
#define  OP_CCOMMA_PREC    -2

#define  OP_APP_FIXITY     OP_infixl
#define  OP_APP_PREC       257 //?

#define  OP_LAM_FIXITY     OP_infixr
#define  OP_LAM_PREC       -1

#define  OP_LT_FIXITY      OP_infix
#define  OP_LT_PREC        130

#define  OP_LE_FIXITY      OP_infix
#define  OP_LE_PREC        130

#define  OP_GT_FIXITY      OP_infix
#define  OP_GT_PREC        130

#define  OP_GE_FIXITY      OP_infix
#define  OP_GE_PREC        130

#define  OP_UM_FIXITY      OP_prefix
#define  OP_UM_PREC        256 //?

#define  OP_PLUS_FIXITY    OP_infixl
#define  OP_PLUS_PREC      150

#define  OP_MINUS_FIXITY   OP_infixl
#define  OP_MINUS_PREC     150

#define  OP_TIMES_FIXITY   OP_infixl
#define  OP_TIMES_PREC     160

#define  OP_COMMA_FIXITY   OP_infixl
#define  OP_COMMA_PREC     110
*/

#endif // OPERATORS_H
