/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef whirl2c_common_INCLUDED
#define whirl2c_common_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: whirl2c_common.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.whirl2c_common.h $
 *
 * Revision history:
 *  07-Nov-94 - Original Version
 *
 * Description:
 *
 * This file is to be included in all major translating modules
 * belonging to whirl2c.  It includes all files that these modules
 * will depend on and defines some utilities that are generally
 * useful.
 *
 *    Output files
 *    ------------
 *
 *      1) The Whirl2c_File is the file to which we would write the
 *         program units (subroutines/functions) in the high-level 
 *         output language.
 *
 *      2) The Whirl2c_Header is the file to which we would write 
 *         file-level declarations in C, and this file is to be
 *         #included at the top of the Whirl2c_File.
 *
 *      3) The Whirl2c_Srcpos_Map_File is the file into which we 
 *         will write the mapping between source positions in the
 *         original source and source positions in the whirl2c
 *         generated source.
 *
 *    Context
 *    -------
 *    We provides facilities for maintaining a CONTEXT, which
 *    flag various aspects of the context in which a WHIRL to C 
 *    translation takes place.  The flags typically say something 
 *    about surrounding expressions, statements, and scopes which 
 *    may influence the translation of types, variables, expressions
 *    and statements
 *
 *
 *    Identifier Naming
 *    -----------------
 *
 *       WHIRL2C_number_as_c_name: Converts the given number into
 *          a C identifier by converting it into a sequence of decimal
 *          characters prepending an underscore ('_').
 *
 *       WHIRL2C_ptr_as_c_name: Converts the given pointer value into
 *           a C identifier by converting it into a sequence of decimal
 *           characters prepending an underscore ('_').  Note that the
 *           number may be a 32 or 64 bits value, depending on the size
 *           of a pointer representation.
 *
 *       WHIRL2C_make_valid_c_name: If the given name is already a
 *          valid C name, then it is simply returned.  If the name
 *          is NULL, then return NULL.  Otherwise, construct a valid
 *          C identifier by removing invalid characters, thus
 *          returning a non-NULL name.

 *    Other General Utilities
 *    -----------------------
 *       See the comments preceding their definitions for a brief
 *       description of the other utilities.
 *
 * ====================================================================
 * ====================================================================
 */

#include "common_include.h"
#include "w2c_driver.h"


typedef struct Context
{
   mUINT32 flags;
   SRCPOS  srcpos;
   TY_IDX  given_ty;
   TY_IDX  elem_ty;
   UINT32  field_id;
   TY_IDX  struct_ty;
} CONTEXT;

/* Initialization */
#define INIT_CONTEXT {0, 0LLU, TY_IDX_ZERO}
#define CONTEXT_reset(c) \
   (void)(CONTEXT_reset_flags(c), \
	  CONTEXT_reset_srcpos(c), \
	  CONTEXT_reset_given_base_ty(c))

/* Base-type suggestions when CONTEXT_array_basetype(c) == TRUE */
#define CONTEXT_given_base_ty(c) (c).given_ty
#define CONTEXT_set_given_base_ty(c, ty) ((c).given_ty = (ty))
#define CONTEXT_reset_given_base_ty(c) ((c).given_ty = TY_IDX_ZERO)

/* lvalue-type suggestions when CONTEXT_lvalue_type(c) == TRUE */
#define CONTEXT_given_lvalue_ty(c) (c).given_ty
#define CONTEXT_set_given_lvalue_ty(c, ty) ((c).given_ty = (ty))
#define CONTEXT_reset_given_lvalue_ty(c) ((c).given_ty = TY_IDX_ZERO)

/* array elem ty */
#define CONTEXT_array_elem_ty(c) (c).elem_ty
#define CONTEXT_set_array_elem_ty(c, ty) ((c).elem_ty = (ty))
#define CONTEXT_reset_array_elem_ty(c) ((c).elem_ty = TY_IDX_ZERO)

/* Srcpos values */
#define CONTEXT_srcpos(c) (c).srcpos
#define CONTEXT_set_srcpos(c, pos) ((c).srcpos = pos)
#define CONTEXT_reset_srcpos(c) (void)((c).srcpos = 0LLU)

/* Field id */
#define CONTEXT_field_id(c) (c).field_id
#define CONTEXT_set_field_id(c, field) ((c).field_id = field)
#define CONTEXT_reset_field_id(c) (void)((c).field_id = 0)

#define CONTEXT_struct_ty(c) (c).struct_ty
#define CONTEXT_set_struct_ty(c, ty) ((c).struct_ty = (ty))
#define CONTEXT_reset_struct_ty(c) ((c).struct_ty = TY_IDX_ZERO)


/* Flag values */
#define CONTEXT_NEEDS_LVALUE 0x000000001     /* Context could use an lvalue */
#define CONTEXT_NEW_FUNC_SCOPE 0x000000002   /* New function scope */
#define CONTEXT_UNQUALIFIED_TY2C 0x000000004 /* Emit unqualified C type */
#define CONTEXT_INCOMPLETE_TY2C 0x000000008  /* Emit incomplete C type  */
#define CONTEXT_TOP_LEVEL_EXPR 0x000000010   /* Top-level of an expression */
#define CONTEXT_ARRAY_BASETYPE 0x000000020   /* Context suggests array type */
#define CONTEXT_LVALUE_TYPE 0x000000040      /* Context suggests lvalue type */
#define CONTEXT_OMP_PRAGMA 0x000000080       /* Processing an Open MP pragma */
#define CONTEXT_CONST_TY2C 0x000000100       /* Whether const is output for this TY */
#define CONTEXT_ARRAY_FOLD     0x00000200       /* Whether the array has been fold base */
#define CONTEXT_ARRAY_ELEM     0x00000400       /* Whether the array elem ty has been transfered down */
// add by shihui for two struct, each of other's pointer or instance. 
#define CONTEXT_TY_IS_POINTED     0x00000800       /* Whether the array elem ty has been transfered down */
#define CONTEXT_TY_IS_BIT_Field   0x00001000       /* Whether the array elem ty has been transfered down */
// add by mw, insert _GLOBAL__D_main and _GLOBAL__I_main when cxx to c 
#define CONTEXT_TY_Insert_Main     0x00002000
// add by mw, indicate translating func header

#define CONTEXT_DIRECT_CAST        0x00004000


#define CONTEXT_ARRAY_BASE         0x00008000

#define CONTEXT_PTR_ARITH 0x000010000       /* pointer arithmetic expressions


// m9. WN2C_based_lvalue from WN2C_lvalue_st from WN2C_LDA should be 
//     processed different from
// #define CONTEXT_FROM_LDA     0x00002000

// convert param from KIND_STRUCT to ptr to KIND_STRUCT
// #define CONTEXT_FUNC_HEADER       0x00004000

/* Accessor macros */
#define CONTEXT_reset_flags(c)  ((c).flags = 0U)

#define CONTEXT_needs_lvalue(c) ((c).flags & CONTEXT_NEEDS_LVALUE)
#define CONTEXT_set_needs_lvalue(c) \
   ((c).flags = (c).flags | CONTEXT_NEEDS_LVALUE)
#define CONTEXT_reset_needs_lvalue(c) \
   ((c).flags = (c).flags & ~CONTEXT_NEEDS_LVALUE)

#define CONTEXT_new_func_scope(c) ((c).flags & CONTEXT_NEW_FUNC_SCOPE)
#define CONTEXT_set_new_func_scope(c) \
   ((c).flags = (c).flags | CONTEXT_NEW_FUNC_SCOPE)
#define CONTEXT_reset_new_func_scope(c) \
   ((c).flags = (c).flags & ~CONTEXT_NEW_FUNC_SCOPE)

#define CONTEXT_unqualified_ty2c(c) ((c).flags & CONTEXT_UNQUALIFIED_TY2C)
#define CONTEXT_set_unqualified_ty2c(c) \
   ((c).flags = (c).flags | CONTEXT_UNQUALIFIED_TY2C)
#define CONTEXT_reset_unqualified_ty2c(c) \
   ((c).flags = (c).flags & ~CONTEXT_UNQUALIFIED_TY2C)

#define CONTEXT_incomplete_ty2c(c) ((c).flags & CONTEXT_INCOMPLETE_TY2C)
#define CONTEXT_set_incomplete_ty2c(c) \
   ((c).flags = (c).flags | CONTEXT_INCOMPLETE_TY2C)
#define CONTEXT_reset_incomplete_ty2c(c) \
   ((c).flags = (c).flags & ~CONTEXT_INCOMPLETE_TY2C)

#define CONTEXT_top_level_expr(c) ((c).flags & CONTEXT_TOP_LEVEL_EXPR)
#define CONTEXT_set_top_level_expr(c) \
   ((c).flags = (c).flags | CONTEXT_TOP_LEVEL_EXPR)
#define CONTEXT_reset_top_level_expr(c) \
   ((c).flags = (c).flags & ~CONTEXT_TOP_LEVEL_EXPR)

#define CONTEXT_array_basetype(c) ((c).flags & CONTEXT_ARRAY_BASETYPE)
#define CONTEXT_set_array_basetype(c) \
   ((c).flags = (c).flags | CONTEXT_ARRAY_BASETYPE)
#define CONTEXT_reset_array_basetype(c) \
   ((c).flags = (c).flags  & ~CONTEXT_ARRAY_BASETYPE)

#define CONTEXT_lvalue_type(c) ((c).flags & CONTEXT_LVALUE_TYPE)
#define CONTEXT_set_lvalue_type(c) \
   ((c).flags = (c).flags | CONTEXT_LVALUE_TYPE)
#define CONTEXT_reset_lvalue_type(c) \
   ((c).flags = (c).flags  & ~CONTEXT_LVALUE_TYPE)

#define CONTEXT_omp(c) ((c).flags & CONTEXT_OMP_PRAGMA)
#define CONTEXT_set_omp(c) \
   ((c).flags = (c).flags | CONTEXT_OMP_PRAGMA)
#define CONTEXT_reset_omp(c) \
   ((c).flags = (c).flags  & ~CONTEXT_OMP_PRAGMA)

#define CONTEXT_const(c) ((c).flags & CONTEXT_CONST_TY2C)
#define CONTEXT_set_const(c) \
   ((c).flags = (c).flags | CONTEXT_CONST_TY2C)
#define CONTEXT_reset_const(c) \
   ((c).flags = (c).flags  & ~CONTEXT_CONST_TY2C)

#define CONTEXT_ptr_arith(c) ((c).flags & CONTEXT_PTR_ARITH)

#define CONTEXT_array_fold(c) ((c).flags & CONTEXT_ARRAY_FOLD)
#define CONTEXT_set_array_fold(c) \
   ((c).flags = (c).flags | CONTEXT_ARRAY_FOLD)
#define CONTEXT_reset_array_fold(c) \
   ((c).flags = (c).flags  & ~CONTEXT_ARRAY_FOLD)

#define CONTEXT_array_elem(c) ((c).flags & CONTEXT_ARRAY_ELEM)
#define CONTEXT_set_array_elem(c) \
   ((c).flags = (c).flags | CONTEXT_ARRAY_ELEM)
#define CONTEXT_reset_array_elem(c) \
   ((c).flags = (c).flags  & ~CONTEXT_ARRAY_ELEM)

#define CONTEXT_ty_is_pointed(c) \
   ((c).flags  & CONTEXT_TY_IS_POINTED)
#define CONTEXT_set_ty_is_pointed(c) \
   ((c).flags = (c).flags  | CONTEXT_TY_IS_POINTED)
#define CONTEXT_reset_ty_is_pointed(c) \
   ((c).flags = (c).flags  & ~CONTEXT_TY_IS_POINTED)

#define CONTEXT_ty_is_bit_field(c) \
   ((c).flags  & CONTEXT_TY_IS_BIT_Field)
#define CONTEXT_set_ty_is_bit_field(c) \
   ((c).flags = (c).flags  | CONTEXT_TY_IS_BIT_Field)
#define CONTEXT_reset_ty_is_bit_field(c) \
   ((c).flags = (c).flags  & ~CONTEXT_TY_IS_BIT_Field)

#define CONTEXT_ty_insert_main(c) \
   ((c).flags & CONTEXT_TY_Insert_Main)
#define CONTEXT_set_ty_insert_main(c) \
   ((c).flags = (c).flags  | CONTEXT_TY_Insert_Main)
#define CONTEXT_reset_ty_insert_main(c) \
   ((c).flags = (c).flags  & ~CONTEXT_TY_Insert_Main)

#define CONTEXT_direct_cast(c) \
   ((c).flags & CONTEXT_DIRECT_CAST)
#define CONTEXT_set_direct_cast(c) \
   ((c).flags = (c).flags  | CONTEXT_DIRECT_CAST)
#define CONTEXT_reset_direct_cast(c) \
   ((c).flags = (c).flags  & ~CONTEXT_DIRECT_CAST)

#define CONTEXT_array_base(c) \
   ((c).flags & CONTEXT_ARRAY_BASE)
#define CONTEXT_set_array_base(c) \
   ((c).flags = (c).flags  | CONTEXT_ARRAY_BASE)
#define CONTEXT_reset_array_base(c) \
   ((c).flags = (c).flags  & ~CONTEXT_ARRAY_BASE)
#define CONTEXT_set_ptr_arith(c) \
   ((c).flags = (c).flags | CONTEXT_PTR_ARITH)
#define CONTEXT_reset_ptr_arith(c) \
   ((c).flags = (c).flags  & ~CONTEXT_PTR_ARITH)


                     /* Identifier naming */
                     /*-------------------*/

/* Some general utility routines for creating C identifier names.
 */
#define WHIRL2C_number_as_c_name(number) Number_as_String(number, "_%lld")
#define WHIRL2C_ptr_as_c_name(ptr) Concat2_Strings("_", Ptr_as_String(ptr))
extern const char * WHIRL2C_make_valid_c_name(const char *name);


            /* Other generally useful macros and functions */
            /*---------------------------------------------*/

/* Put parenthesis around the contents of the tokens.
 */
extern void WHIRL2C_parenthesize(TOKEN_BUFFER tokens);

extern int compiling_upc_flag;

extern CONTEXT W2C_Bakup_Context(CONTEXT context);
extern void W2C_Restore_Context(CONTEXT context, CONTEXT bakup);
#ifndef W2C
struct eqstr {
  bool operator()(const char* s1, const char* s2) const
  {
    return strncmp(s1, s2, 256) == 0;
  }
};
#endif

#endif /* whirl2c_common_INCLUDED */




