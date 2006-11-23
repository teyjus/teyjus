/*************************************************************************/
/* TEMP: all contents in this file should be moved to ../instructions.h  */
/*************************************************************************/

#ifndef MYINSTR_H
#define MYINSTR_H

#include "mctypes.h"
/**************************************************************************/
/*                  Types for instruction operants                        */
/**************************************************************************/
typedef Byte        INSTR_OpCode;
typedef Byte        INSTR_RegInd;     
typedef Byte        INSTR_EnvInd;
typedef Byte        INSTR_ClEnvInd;
typedef Byte        INSTR_OneByteInt;
typedef Byte        INSTR_NextClauseInd;
typedef TwoBytes    INSTR_CstIndex;
typedef TwoBytes    INSTR_KstIndex;

typedef CSpacePtr   INSTR_CodeLabel;

typedef long        INSTR_Int;
typedef float       INSTR_Float;
typedef char       *INSTR_Str;

typedef MemPtr      INSTR_ImplTab;
typedef MemPtr      INSTR_ModTab;
typedef MemPtr      INSTR_HashTab;
typedef MemPtr      INSTR_BrachTab;

/**************************************************************************/
/* Macros defines instruction lengths and distances between op code and   */
/* operands.                                                              */
/**************************************************************************/
#define MACH_64 //temp

#ifdef  MACH_64

#define INSTR_CALL_I1_LEN          15

//INSTR_CAT_X
#define INSTR_X_LEN                8
//INSTR_CAT_RX
#define INSTR_RX_R                 1
#define INSTR_RX_LEN               8
//INSTR_CAT_EX
#define INSTR_EX_E                 1
#define INSTR_EX_LEN               8
//INSTR_CAT_I1X
#define INSTR_I1X_I1               1
#define INSTR_I1X_LEN              8
//INSTR_CAT_CX
#define INSTR_CX_C                 2
#define INSTR_CX_LEN               8
//INSTR_CAT_KX
#define INSTR_KX_K                 2
#define INSTR_KX_LEN               8
//INSTR_CAT_IX
#define INSTR_IX_I                 8
#define INSTR_IX_LEN              16 
//INSTR_CAT_FX
#define INSTR_FX_F                 8
#define INSTR_FX_LEN              16 
//INSTR_CAT_SX
#define INSTR_SX_S                 8
#define INSTR_SX_LEN              16 
//INSTR_CAT_MTX
#define INSTR_MTX_MT               8
#define INSTR_MTX_LEN             16
//INSTR_CAT_LX
#define INSTR_LX_L                 8
#define INSTR_LX_LEN              16
//INSTR_CAT_RRX
#define INSTR_RRX_R1               1
#define INSTR_RRX_R2               2
#define INSTR_RRX_LEN              8
//INSTR_CAT_ERX
#define INSTR_ERX_E                1
#define INSTR_ERX_R                2
#define INSTR_ERX_LEN              8
//INSTR_CAT_RCX
#define INSTR_RCX_R                1
#define INSTR_RCX_C                2
#define INSTR_RCX_LEN              8
//INSTR_CAT_RIX
#define INSTR_RIX_R                1
#define INSTR_RIX_I                8
#define INSTR_RIX_LEN             16 
//INSTR_CAT_RFX
#define INSTR_RFX_R                1
#define INSTR_RFX_F                8
#define INSTR_RFX_LEN             16 
//INSTR_CAT_RSX
#define INSTR_RSX_R                1
#define INSTR_RSX_S                8
#define INSTR_RSX_LEN             16 
//INSTR_CAT_RI1X     
#define INSTR_RI1X_R               1
#define INSTR_RI1X_I1              2
#define INSTR_RI1X_LEN             8
//INSTR_CAT_RCEX               
#define INSTR_RCEX_R               1
#define INSTR_RCEX_CE              2
#define INSTR_RCEX_LEN             8
//INSTR_CAT_ECEX
#define INSTR_ECEX_E               1
#define INSTR_ECEX_CE              2
#define INSTR_ECEX_LEN             8
//INSTR_CAT_CLX
#define INSTR_CLX_C                2
#define INSTR_CLX_L                8
#define INSTR_CLX_LEN             16
//INSTR_CAT_RKX
#define INSTR_RKX_R                1
#define INSTR_RKX_K                2
#define INSTR_RKX_LEN              8
//INSTR_CAT_ECX
#define INSTR_ECX_E                1
#define INSTR_ECX_C                2
#define INSTR_ECX_LEN              8
//INSTR_CAT_I1ITX
#define INSTR_I1ITX_I1             1
#define INSTR_I1ITX_IT             8
#define INSTR_I1ITX_LEN           16
//INSTR_CAT_I1LX
#define INSTR_I1LX_I1              1
#define INSTR_I1LX_L               8
#define INSTR_I1LX_LEN            16  
//INSTR_CAT_I1NX
#define INSTR_I1NX_I1              1
#define INSTR_I1NX_N               2
#define INSTR_I1NX_LEN             8
//INSTR_CAT_I1HTX
#define INSTR_I1HTX_I1             1
#define INSTR_I1HTX_HT             8
#define INSTR_I1HTX_LEN           16
//INSTR_CAT_I1BVTX
#define INSTR_I1BVTX_I1            1
#define INSTR_I1BVTX_BVT           8
#define INSTR_I1BVTX_LEN          16 
//INSTR_CAT_CWPX
#define INSTR_CWPX_C               2
#define INSTR_CWPX_LEN            16
//INSTR_CAT_I1WPX
#define INSTR_I1WPX_I1             1
#define INSTR_I1WPX_LEN           16
//INSTR_CAT_RRI1X
#define INSTR_RRI1X_R1             1
#define INSTR_RRI1X_R2             2
#define INSTR_RRI1X_I1             3
#define INSTR_RRI1X_LEN            8
//INSTR_CAT_RCLX
#define INSTR_RCLX_R               1
#define INSTR_RCLX_C               2
#define INSTR_RCLX_L               8
#define INSTR_RCLX_LEN            16
//INSTR_CAT_RCI1X
#define INSTR_RCI1X_R              1
#define INSTR_RCI1X_C              2
#define INSTR_RCI1X_I1             4
#define INSTR_RCI1X_LEN            8
//INSTR_CAT_I1I1LX
#define INSTR_I1I1LX_I11           1
#define INSTR_I1I1LX_I12           2
#define INSTR_I1I1LX_L             8
#define INSTR_I1I1LX_LEN          16
//INSTR_CAT_I1LLX
#define INSTR_I1LLX_I1             1
#define INSTR_I1LLX_L1             8
#define INSTR_I1LLX_L2            16
#define INSTR_I1LLX_LEN           24
//INSTR_CAT_NLLX
#define INSTR_NLLX_N               1
#define INSTR_NLLX_L1              8
#define INSTR_NLLX_L2             16
#define INSTR_NLLX_LEN            24
//INSTR_CAT_LLLX
#define INSTR_LLLLX_L1              8
#define INSTR_LLLLX_L2             16
#define INSTR_LLLLX_L3             24
#define INSTR_LLLLX_L4             32
#define INSTR_LLLLX_LEN            40
//INSTR_CAT_I1CWPX
#define INSTR_I1CWPX_I1             1
#define INSTR_I1CWPX_C              2
#define INSTR_I1CWPX_LEN           16 

#else   //MACH_64

#define INSTR_CALL_I1_LEN          7

//INSTR_CAT_X
#define INSTR_X_LEN                4
//INSTR_CAT_RX
#define INSTR_RX_R                 1
#define INSTR_RX_LEN               4
//INSTR_CAT_EX
#define INSTR_EX_E                 1
#define INSTR_EX_LEN               4
//INSTR_CAT_I1X
#define INSTR_I1X_I1               1
#define INSTR_I1X_LEN              4
//INSTR_CAT_CX
#define INSTR_CX_C                 2
#define INSTR_CX_LEN               4
//INSTR_CAT_KX
#define INSTR_KX_K                 2
#define INSTR_KX_LEN               4
//INSTR_CAT_IX
#define INSTR_IX_I                 4
#define INSTR_IX_LEN               8 
//INSTR_CAT_FX
#define INSTR_FX_F                 4
#define INSTR_FX_LEN               8 
//INSTR_CAT_SX
#define INSTR_SX_S                 4
#define INSTR_SX_LEN               8 
//INSTR_CAT_MTX
#define INSTR_MTX_MT               4
#define INSTR_MTX_LEN              8
//INSTR_CAT_LX
#define INSTR_LX_L                 4
#define INSTR_LX_LEN               8
//INSTR_CAT_RRX
#define INSTR_RRX_R1               1
#define INSTR_RRX_R2               2
#define INSTR_RRX_LEN              4
//INSTR_CAT_ERX
#define INSTR_ERX_E                1
#define INSTR_ERX_R                2
#define INSTR_ERX_LEN              4
//INSTR_CAT_RCX
#define INSTR_RCX_R                1
#define INSTR_RCX_C                2
#define INSTR_RCX_LEN              4
//INSTR_CAT_RIX
#define INSTR_RIX_R                1
#define INSTR_RIX_I                4
#define INSTR_RIX_LEN              8 
//INSTR_CAT_RFX
#define INSTR_RFX_R                1
#define INSTR_RFX_F                4
#define INSTR_RFX_LEN              8 
//INSTR_CAT_RSX
#define INSTR_RSX_R                1
#define INSTR_RSX_S                4
#define INSTR_RSX_LEN              8 
//INSTR_CAT_RI1X     
#define INSTR_RI1X_R               1
#define INSTR_RI1X_I1              2
#define INSTR_RI1X_LEN             4
//INSTR_CAT_RCEX               
#define INSTR_RCEX_R               1
#define INSTR_RCEX_CE              2
#define INSTR_RCEX_LEN             4
//INSTR_CAT_ECEX
#define INSTR_ECEX_E               1
#define INSTR_ECEX_CE              2
#define INSTR_ECEX_LEN             4
//INSTR_CAT_CLX
#define INSTR_CLX_C                2
#define INSTR_CLX_L                4
#define INSTR_CLX_LEN              8
//INSTR_CAT_RKX
#define INSTR_RKX_R                1
#define INSTR_RKX_K                2
#define INSTR_RKX_LEN              4
//INSTR_CAT_ECX
#define INSTR_ECX_E                1
#define INSTR_ECX_C                2
#define INSTR_ECX_LEN              4
//INSTR_CAT_I1ITX
#define INSTR_I1ITX_I1             1
#define INSTR_I1ITX_IT             4
#define INSTR_I1ITX_LEN            8
//INSTR_CAT_I1LX
#define INSTR_I1LX_I1              1
#define INSTR_I1LX_L               4
#define INSTR_I1LX_LEN             8  
//INSTR_CAT_I1NX
#define INSTR_I1NX_I1              1
#define INSTR_I1NX_N               2
#define INSTR_I1NX_LEN             4
//INSTR_CAT_I1HTX
#define INSTR_I1HTX_I1             1
#define INSTR_I1HTX_HT             4
#define INSTR_I1HTX_LEN            8
//INSTR_CAT_I1BVTX
#define INSTR_I1BVTX_I1            1
#define INSTR_I1BVTX_BVT           4
#define INSTR_I1BVTX_LEN           8 
//INSTR_CAT_CWPX
#define INSTR_CWPX_C               2
#define INSTR_CWPX_LEN             8
//INSTR_CAT_I1WPX
#define INSTR_I1WPX_I1             1
#define INSTR_I1WPX_LEN            8
//INSTR_CAT_RRI1X
#define INSTR_RRI1X_R1             1
#define INSTR_RRI1X_R2             2
#define INSTR_RRI1X_I1             3
#define INSTR_RRI1X_LEN            4
//INSTR_CAT_RCLX
#define INSTR_RCLX_R               1
#define INSTR_RCLX_C               2
#define INSTR_RCLX_L               4
#define INSTR_RCLX_LEN             8
//INSTR_CAT_RCI1X
#define INSTR_RCI1X_R              1
#define INSTR_RCI1X_C              2
#define INSTR_RCI1X_I1             4
#define INSTR_RCI1X_LEN            8
//INSTR_CAT_I1I1LX
#define INSTR_I1I1LX_I11           1
#define INSTR_I1I1LX_I12           2
#define INSTR_I1I1LX_L             4
#define INSTR_I1I1LX_LEN           8
//INSTR_CAT_I1LLX
#define INSTR_I1LLX_I1             1
#define INSTR_I1LLX_L1             4
#define INSTR_I1LLX_L2             8
#define INSTR_I1LLX_LEN           12
//INSTR_CAT_NLLX
#define INSTR_NLLX_N               1
#define INSTR_NLLX_L1              4
#define INSTR_NLLX_L2              8
#define INSTR_NLLX_LEN            12
//INSTR_CAT_LLLX
#define INSTR_LLLLX_L1              4
#define INSTR_LLLLX_L2              8
#define INSTR_LLLLX_L3             12
#define INSTR_LLLLX_L4             16
#define INSTR_LLLLX_LEN            20 
//INSTR_CAT_I1CWPX
#define INSTR_I1CWPX_I1             1
#define INSTR_I1CWPX_C              2
#define INSTR_I1CWPX_LEN            8 

#endif  //MACH_64


#endif  //MYINSTR_H
