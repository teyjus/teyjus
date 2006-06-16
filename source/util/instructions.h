/****************************************************************************/
/* Teyjus, an implementation of Lambda Prolog                               */
/* Copyright (C) 1999 Gopalan Nadathur                                      */
/*                                                                          */
/* This program is free software; you can redistribute it and/or modify     */
/* it under the terms of the GNU General Public License as published by     */
/* the Free Software Foundation; either version 2 of the License, or        */
/* (at your option) any later version.                                      */
/*                                                                          */
/* This program is distributed in the hope that it will be useful,          */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/* GNU General Public License for more details.                             */
/*                                                                          */
/* You should have received a copy of the GNU General Public License        */
/* along with this program; if not, write to the Free Software              */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA */
/****************************************************************************/
/****************************************************************************/
/*                                                                          */
/*                                Teyjus, 1996                              */
/*                                                                          */
/*   File  instructions.h.  This header file defines the instruction        */
/*   categories according to argument types and op codes. It also defines   */
/*   a structure that is used to store information about each instruction.  */
/*   See the file instructions.c for information for each specific          */
/*   instruction.                                                           */
/*                                                                          */
/****************************************************************************/

#ifndef INSTRUCTIONS_H
#define INSTRUCTIONS_H

/* Structure for storing information about instructions; indexed by 
   op code.                                                          */

struct INSTR_info_ty {
   char  *name;
   int    type;
   int    size;
};

extern  struct INSTR_info_ty  INSTR_info[];

/* each operand is of one of the following types */
typedef enum INSTR_operand_type
{ 
   INSTR_P,			/* (1 byte) padding */
   INSTR_R,			/* argument register number */
   INSTR_E,			/* environment variable number */
   INSTR_N,			/* next clause number in impt/impl point */
   INSTR_I1,			/* 1 byte natural number  */
   INSTR_I2,			/* 2 byte natural number */
   INSTR_CE,			/* closure environment variable number */
   INSTR_C,			/* constant symbol table index */
   INSTR_K,			/* kind symbol table index */
   INSTR_T,			/* type skeleton table index */
   INSTR_MT,			/* module table address */
   INSTR_IT,			/* implication table address */
   INSTR_HT,			/* hash table address */
   INSTR_BVT,			/* branch table for bound variable indexing */
   INSTR_L,			/* code location */
   INSTR_I,			/* integer immediate value */
   INSTR_F,			/* floating-point immediate value*/
   INSTR_S,			/* pointer to string start address */
   INSTR_SL,			/* pointer to length word followed by
				   string */
   INSTR_X			/* (operand-list terminator) */
} INSTR_operand_type;


/*  The relevant instruction categories then are the following:  */

#define   P_P_P             0
#define   R_P_P             1
#define   E_P_P             2
#define   I1_P_P            3
#define   R_R_P             4
#define   E_R_P             5
#define   R_CE_P            6
#define   E_CE_P            7
#define   P_C               8
#define   P_K               9
#define   R_R_R            10
#define   E_R_R            11
#define   R_C              12
#define   E_C              13
#define   R_T              14
#define   E_T              15
#define   R_K              16
#define   R_C_I1_P_P_P     17
#define   R_T_R_P_P_P      18
#define   E_T_R_P_P_P      19
#define   R_K_R_P_P_P      20
#define   P_P_P_MT         21
#define   P_P_P_L          22
#define   R_P_P_L          23
#define   I1_P_P_L         24
#define   I1_P_P_IT        25
#define   I1_P_P_HT        26
#define   R_R_P_S          27
#define   P_C_L            28
#define   P_I1_I1_L        29
#define   R_C_L            30
#define   R_T_L            31
#define   E_T_L            32
#define   N_P_P_L_L        33
#define   P_P_P_L_L_L_L    34
#define   R_R_I1           35
#define   R_R_R_I1_P_P_P   36
#define   R_I1_P           37
#define   E_I1_P           38
#define   R_C_I1_P_P_P_L   39
#define   R_T_I1_P_P_P     40
#define   E_T_I1_P_P_P     41
#define   R_T_R_I1_P_P     42
#define   E_T_R_I1_P_P     43
#define   I1_C_P_P_P_P     44
#define   I1_N_P           45
#define   I1_P_P_BVT       46
#define   I1_I1_P_P_P_P_P  47
#define   I1_I1_P          48
#define   R_P_P_I          49
#define   R_P_P_F          50
#define   R_P_P_SL         51
#define   P_P_P_I          52
#define   P_P_P_F          53
#define   P_P_P_SL         54
#define   NUM_INSTRUCTION_CATEGORIES 55


/* Each instruction has a maximum of seven operands, plus one to
   terminate the list */
#define INSTR_MAX_OPERANDS 8

/* this array is indexed by instruction category.  For each category,
   INSTR_operand_types contains a string of values indicating the type
   of the operand at that position, terminated by INSTR_END.  This
   information is useful when parsing instruction streams. */
extern INSTR_operand_type
       INSTR_operand_types[NUM_INSTRUCTION_CATEGORIES][INSTR_MAX_OPERANDS];

/* Lengths of instructions in the different categories in bytes. 
   The assumption is that the op code occupies 1 byte. */

#define   P_P_P_Len             4
#define   R_P_P_Len             4
#define   E_P_P_Len             4
#define   I1_N_P_Len            4
#define   I1_P_P_Len            4
#define   R_R_P_Len             4
#define   E_R_P_Len             4
#define   R_CE_P_Len            4
#define   E_CE_P_Len            4
#define   P_C_Len               4
#define   P_K_Len               4
#define   R_R_R_Len             4
#define   E_R_R_Len             4
#define   R_C_Len               4
#define   E_C_Len               4
#define   R_T_Len               4
#define   E_T_Len               4
#define   R_K_Len               4
#define   I1_C_P_P_P_P_Len      8
#define   R_I2_R_P_P_P_Len      8
#define   R_C_I1_P_P_P_Len      8
#define   R_T_R_P_P_P_Len       8
#define   R_T_I1_P_P_P_Len      8
#define   E_T_R_P_P_P_Len       8
#define   E_T_I1_P_P_P_Len      8
#define   R_T_R_I1_P_P_Len      8
#define   E_T_R_I1_P_P_Len      8
#define   R_K_R_P_P_P_Len       8
#define   P_P_P_MT_Len          8
#define   P_P_P_L_Len           8
#define   R_P_P_L_Len           8
#define   I1_P_P_L_Len          8
#define   I1_P_P_IT_Len         8
#define   R_R_P_S_Len           8
#define   P_C_L_Len             8
#define   P_I1_I1_L_Len         8
#define   R_C_L_Len             8
#define   R_T_L_Len             8
#define   E_T_L_Len             8
#define   I1_P_P_HT_Len         8
#define   I1_P_P_BVT_Len        8
#define   N_P_P_L_L_Len        12
#define   P_P_P_L_L_L_L_Len    20
#define   R_R_I1_Len            4
#define   R_R_R_I1_P_P_P_Len    8
#define   R_I1_P_Len            4
#define   E_I1_P_Len            4
#define   R_C_I1_P_P_P_L_Len   12
#define   I1_I1_P_P_P_P_P_Len   8
#define   I1_I1_P_Len           4
#define   R_P_P_I_Len           8
#define   R_P_P_F_Len           8
#define   R_P_P_SL_Len          8
#define   P_P_P_I_Len           8
#define   P_P_P_F_Len           8
#define   P_P_P_SL_Len          8

/*  Op codes for instructions. Instructions are categorized by purpose
    here. A different categorization, such as by length and operand 
    types may also be meaningful. If such a characterization becomes 
    useful, it may be found in the implementation notes directory.   */

/*  Instructions for term unification/creation  */

#define   unify_nil                    0
#define   get_list                     1
#define   put_nil                      2
#define   unify_variable_t             3         /* temporary variable */
#define   unify_variable_tty           4         /* permanent variable */
#define   unify_value_t                5         /* temporary variable */      
#define   unify_value_p                6         /* permanent variable */
#define   unify_l_value_t              7         /* temporary variable */
#define   unify_l_value_p              8         /* permanent variable */
#define   globalize_t                  9         /* temporary variable */
#define   globalize_pt                 10        /* perm var and reg   */
#define   deref                        11
#define   set_value_t                  12        /* temporary variable */
#define   set_value_p                  13        /* permanent variable */
#define   copy_value                   14           
#define   head_normalize_t             15        /* temporary variable */
#define   head_normalize_p             16        /* permanent variable */
#define   get_variable_t               17        /* temporary variable */
#define   get_variable_p               18        /* permanent variable */
#define   put_value_t                  19        /* temporary variable */
#define   put_value_p                  20        /* permanent variable */
#define   put_clambda                  21        
#define   put_flambda                  22          
#define   put_unsafe_value             23
#define   init_variable_t              24        /* temporary variable */
#define   init_variable_p              25        /* permanent variable */
#define   put_capp                     26
#define   put_fapp                     27
#define   put_clist                    28
#define   put_flist                    29
#define   set_clambda                  30
#define   set_flambda                  31
#define   unify_m_constant             32
#define   get_m_constant               33
#define   put_p_const                  34
#define   put_m_const                  35
#define   set_p_constant               36
#define   set_m_constant               37
#define   set_nil                      38
#define   unify_m_variable_t           39        /* temporary variable */
#define   unify_m_variable_p           40        /* permanent variable */
#define   set_p_variable_t             41        /* temporary variable */
#define   set_p_variable_p             42        /* permanent variable */
#define   set_m_variable_t             43        /* temporary variable */
#define   set_m_variable_p             44        /* perm var and reg   */
#define   get_p_structure              45
#define   get_m_structure              46
#define   put_p_variable_t             47        /* temporary variable */
#define   put_p_variable_p             48        /* permanent variable */
#define   put_m_variable_t             49        /* temporary variable */
#define   put_m_variable_p             50        /* permanent variable */
#define   get_nil                      51
#define   unify_void                   52
#define   put_index                    53
#define   set_index                    54
#define   put_cheavylambda             55
#define   put_fheavylambda             56
#define   unify_p_constant             57
#define   get_p_constant               58
#define   unify_p_variable_t           59        /* temporary variable */
#define   unify_p_variable_p           60        /* permanent variable */
#define   set_p_variable_te            177
#define   set_m_variable_te            178



/* Instructions for type unification/creation */


#define   unify_type_variable_t        61        /* temporary variable */
#define   unify_type_variable_p        62        /* permanent variable */
#define   unify_type_value_t           63        /* temporary variable */
#define   unify_envty_value_t          64
#define   unify_type_value_p           65        /* permanent variable */
#define   unify_envty_value_p          66
#define   unified_type_value_t         67        /* temporary variable */
#define   unified_type_value_p         68        /* permanent variable */
#define   unify_type_l_value_t         69        /* temporary variable */
#define   unify_envty_l_value_t        70
#define   unify_type_l_value_p         71        /* permanent variable */
#define   unify_envty_l_value_p        72
#define   unified_type_l_value_t       73        /* temporary variable */
#define   unified_type_l_value_p       74        /* permanent variable */
#define   set_type_variable_t          75        /* temporary variable */
#define   set_type_variable_p          76        /* permanent variable */
#define   set_type_value_t             77        /* temporary variable */
#define   set_type_value_p             78        /* permanent variable */
#define   set_type_l_value_t           79        /* temporary variable */
#define   set_type_l_value_p           80        /* permanent variable */
#define   get_type_variable_t          81        /* temporary variable */
#define   get_type_variable_p          82        /* permanent variable */
#define   get_type_value_t             83        /* temporary variable */
#define   get_type_value_p             84        /* permanent variable */
#define   put_type_variable_t          85        /* temporary variable */
#define   put_type_variable_p          86        /* permanent variable */
#define   put_type_value_t             87        /* temporary variable */
#define   put_type_value_p             88        /* permanent variable */
#define   put_type_unsafe_value        89
#define   get_type_arrow               90
#define   put_type_arrow               91
#define   unify_argty_variable_t       92        /* temporary variable */
#define   unify_argty_variable_p       93        /* permanent variable */
#define   unify_argty_value_t          94        /* temporary variable */
#define   unify_argty_value_p          95        /* permanent variable */
#define   unify_argty_l_value_t        96        /* temporary variable */
#define   unify_argty_l_value_p        97        /* permanent variable */
#define   unify_targty_variable_t      98       /* temporary variable */
#define   unify_targty_variable_p      99       /* permanent variable */
#define   unify_targty_value_t         100       /* temporary variable */
#define   unify_targty_value_p         101       /* permanent variable */
#define   unify_targty_l_value_t       102       /* temporary variable */
#define   unify_targty_l_value_p       103       /* permanent variable */
#define   unify_type_constant          104
#define   set_type_constant            105
#define   get_type_constant            106
#define   get_type_structure           107
#define   put_type_const               108
#define   put_type_structure           109
#define   unify_argty_constant         110
#define   unify_targty_constant        111
#define   init_type_variable_t         175
#define   init_type_variable_p         176



/* Instructions for handling higher-order aspects  */

#define   call_finish_unify            112
#define   execute_finish_unify         113
#define   proceed_finish_unify         114
#define   simpl_t                      115       /* temporary variable */
#define   simpl_p                      116       /* permanent variable */



/* Instructions for handling logical aspects   */

#define   incr_universe                117
#define   decr_universe                118
#define   pop_impl_point               119
#define   pop_imports                  120
#define   set_univ_tag_p               121
#define   set_univ_tag_m               122
#define   tag_p_exists_t               123       /* temporary variable */
#define   tag_p_exists_p               124       /* permanent variable */
#define   tag_m_exists_t               125       /* temporary variable */
#define   tag_m_exists_p               126       /* permanent variable */
#define   tag_p_variable               127
#define   tag_m_variable               128
#define   push_import                  129
#define   push_impl_point              130
#define   remove_imports               131    
#define   add_imports                  132



/* Control Instructions  */

#define   allocate                     133
#define   deallocate                   134
#define   proceed                      135
#define   execute_name                 136
#define   call_name                    137
#define   execute                      138
#define   call                         139



/* Choice Instructions  */

#define   trust_me                     140
#define   unify_backtrack              141
#define   trust_ext                    142
#define   retry_me_else                143
#define   retry                        144
#define   trust                        145
#define   try_me_else                  146
#define   try                          147



/* Indexing Instructions */

#define   switch_on_constant           148
#define   switch_on_bvar               149
#define   switch_on_reg                150
#define   switch_on_term               151



/* Cut Instructions  */

#define   neck_cut                     152
#define   get_level                    153
#define   put_level                    154
#define   cut                          155
 
 
/* Miscellaneous Instructions */
 
#define   load_args_in_regs            156
#define   call_builtin                 157
#define   builtin                      158
#define   stop                         159
#define   halt                         160
#define   callp                        161     /* is this needed? GN 1/13/97 */
#define   fail                         162

/* insructions for manipulating immediate values */

#define   get_integer                  163
#define   get_float                    164
#define   get_string                   165

#define   unify_integer                166
#define   unify_float                  167
#define   unify_string                 168

#define   put_integer                  169
#define   put_float                    170
#define   put_string                   171

#define   set_integer                  172
#define   set_float                    173
#define   set_string                   174

#define   NUM_INSTRUCTIONS             179     /* this should be
						  incremented as
						  needed */

#endif
