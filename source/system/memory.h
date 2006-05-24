#ifndef MEMORY_H
#define MEMORY_H

/**********************************************************************/
/*      KIND SYMBOL TABLE                                             */
/**********************************************************************/
typedef unsigned short int MEM_KSTTABIND;      //symbol table index

typedef unsigned short int MEM_SKELIND;        //offset of tvar in skel


/**********************************************************************/
/*      CONSTANT SYMBOL TABLE                                         */
/**********************************************************************/

typedef unsigned short int MEM_CSTTABIND;      //symbol table index

typedef unsigned short int MEM_UNIVIND;        //universe index


/*********************************************************************/
/*      STREAM SYMBOL TABLE                                          */
/*********************************************************************/

typedef unsigned short int MEM_STREAMTABIND;

/**********************************************************************/
/*      CODE SPACE                                                    */
/**********************************************************************/

typedef unsigned short int MEM_EMBEDLEV;    //abstraction context (DB ind)
typedef unsigned short int MEM_ARITY;       //arity

typedef unsigned short int MEM_TY_ARITY;    //arity of type structures


/* types for pre universe counter, (symbol) table index, and abstraction 
   embedding level fields for handling overflow exceptions*/
typedef unsigned int MEM_PREUNIVIND;
typedef unsigned int MEM_PREEMBEDLEV;
typedef unsigned int MEM_PREARITY;

#endif  //MEMORY_H
