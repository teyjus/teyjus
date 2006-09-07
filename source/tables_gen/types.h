/****************************************************************************/
/* File types.h. This file contains "abstract syntax" representation of     */
/* type skeletons that is used for parsing those in pervasives.in.          */
/****************************************************************************/
#ifndef TYPES_H
#define TYPES_H

typedef struct Type_       *Type;
typedef struct TypeList_   *TypeList;

//type arrow information
typedef struct ArrowInfo
{
    Type  lop;
    Type  rop;
} ArrowInfo;

//structure functor information
typedef struct FuncInfo
{
    char *name;
    char *arity;
} FuncInfo;

//type structure information
typedef struct StrInfo
{
    Type     functor;
    int      arity;
    TypeList args;
} StrInfo;

//type skeleton category
typedef enum {
    SORT, SKVAR, ARROW, STR, FUNC
} TypeCats;

//type representation
typedef struct Type_
{
    TypeCats tag;
    union
    {
        char*      sort;
        char*      skvar;
        FuncInfo   func;
        ArrowInfo  arrow;
        StrInfo    str;
    } data;
} Type_;

//type list representation
typedef struct TypeList_
{
    Type        oneType;
    TypeList    next;
} TypeList_;


Type mkSortType(char* name);
Type mkSkVarType(char* index);
Type mkStrFuncType(char* name, char* arity);
Type mkStrType(Type name, int arity, TypeList args);
Type mkArrowType(Type lop, Type rop);
void freeType(Type ty);


TypeList addItem(Type data, TypeList typeList);
TypeList addItemToEnd(TypeList typeList, Type data);
TypeList append(TypeList typeList1, TypeList typeList2);

#endif //TYPES_H




