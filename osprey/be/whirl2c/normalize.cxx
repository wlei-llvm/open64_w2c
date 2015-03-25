//-----------------------------------------------------------------------------
// MODULE:     Whirl/Symtab Normalization module
// File Name:  normalize.cxx
// Author:     miwei
// Abstract:   normalize whirl and symtab to make whirl2c easier            
// Date:       2007-6-14
// Copy Right: 2004-2007, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#include "wn_util.h"
#include "w2cf_parentize.h"
#include "normalize.h"
#include "stab_attr.h"
#include "erglob.h"
#include "tracing.h"

WN_MAP W2CF_Treety_Map = WN_MAP_UNDEFINED;

extern void
Normalize_Array(WN *wn, TY_IDX objty, BOOL rec);

//--------------------------------------------------------------------
// FUNCTION NAME: WN2C_Set_Treety
// Input:
//   arguments: 
//     WN *wn
//     TY_IDX ty
// Output:
//     void
// Description:
//     stick treety on wn using W2CF_Treety_Map 
//--------------------------------------------------------------------
void WN2C_Set_Treety(const WN *wn, TY_IDX ty)
{
  WN_MAP32_Set(W2CF_Treety_Map, (WN *)wn, ty);
}

//--------------------------------------------------------------------
// FUNCTION NAME: WN2C_Get_Treety
// Input:
//   arguments: 
//     WN *wn
// Output:
//     TY_IDX ty
// Description:
//     return stick treety on wn using W2CF_Treety_Map 
//--------------------------------------------------------------------
TY_IDX WN2C_Get_Treety(const WN *wn)
{
  return (TY_IDX)WN_MAP32_Get(W2CF_Treety_Map, (WN *)wn);
}

//--------------------------------------------------------------------
// FUNCTION NAME: WN2C_unparser_compatible_types
// Input:
//   arguments: 
//     TY_IDX t1 
//     TY_IDX t2
// Output:
//     void
// Description:
//     if wnty is incompatible with treety, then wnty should be 
//     changed to treety
//--------------------------------------------------------------------
BOOL
WN2C_unparser_compatible_types(TY_IDX wnty,
		   TY_IDX treety,
                   INT depth)
{
  INT i; /* Array dimensions */

  /* FIXME: UPC */
  if (wnty == treety)
    return TRUE;
  else if (TY_kind(wnty) == KIND_INVALID || 
             TY_kind(treety) == KIND_INVALID) {
    return FALSE;  
  } else {
    switch (TY_kind(wnty))
    {
      case KIND_VOID:
	return TY_kind(treety) == KIND_VOID; /* Must be identical kinds */

      case KIND_SCALAR:
	if (TY_Is_String(wnty) && TY_Is_Array_Of_Chars(treety))
	  return TRUE;
	else
	  return (TY_Is_Scalar(treety) &&
                   (TY_mtype(wnty) == TY_mtype(treety)));

      case KIND_POINTER:
	/* Should we also consider MTYPE_STRING identical to a (char*)? */
	if (TY_Is_Pointer(treety)) {
          return WN2C_unparser_compatible_types(TY_pointed(wnty), 
                                                  TY_pointed(treety),
                                                  depth + 1); 
	} else if (TY_Is_Array(treety)) {
          // int *p and int p[] is compatible but 
          // int **p and int (*p)[] is incompatible
          if (depth == 0) {
            return WN2C_unparser_compatible_types(TY_pointed(wnty), 
                                                    TY_AR_etype(treety),
                                                    depth + 1); 
          } else {
            FALSE;
          }
	} else {
          return FALSE;
        }
      case KIND_FUNCTION:
	/* We do a very quick check to see if two function types are
	 * identical.  A more elaborate, but slower, method will check
	 * each individual parameter type (TY_parms(wnty) and TY_parms(treety))
	 * for identity.
	 */
	return (TY_Is_Function(treety)                           &&
                  TY_has_prototype(wnty) == TY_has_prototype(treety) &&
                  TY_is_varargs(wnty) == TY_is_varargs(treety)       &&
                  TY_parms(wnty) == TY_parms(treety)                 &&
                  WN2C_unparser_compatible_types(Func_Return_Type(wnty), 
                                                 Func_Return_Type(treety)));

      case KIND_ARRAY:
	if (TY_Is_String(treety) && TY_Is_Array_Of_Chars(wnty)) {
	  return TRUE;
	} else if (TY_Is_Pointer(treety)) {
          // int *p and int p[] is compatible but 
          // int **p and int (*p)[] is incompatible
          if (depth == 0) {
            return WN2C_unparser_compatible_types(TY_AR_etype(wnty),   
                                                    TY_pointed(treety),
                                                    depth + 1); 
          } else {
            return 0;
          }
	} else if (!TY_Is_Array(treety) ||
                    TY_AR_ndims(wnty) != TY_AR_ndims(treety)) {
	  if (!TY_Is_Array(treety)) {
	    return FALSE;
	  } else {
	    Is_True(0, ("Ty incompatible because of "
                        "array dim num not equal, take a look!"));
	    return FALSE;
	  }
	} else {
	  for (i=0; i<TY_AR_ndims(wnty); i++) {
	    /* First check if one constant and the other not;
	     * then check if constants don't match; we assume
	     * dynamic bounds/strides always match since we
	     * implement them in terms of pointers in C.
	     */
	    if (TY_AR_const_lbnd(wnty,i) != TY_AR_const_lbnd(treety,i) ||
		TY_AR_const_ubnd(wnty,i) != TY_AR_const_ubnd(treety,i)) {
	      Is_True(0, ("Ty incompatible because of "
		    "array dim lb/ub not equal, take a look!"));
	      return FALSE;

	    } else if (TY_AR_const_lbnd(wnty,i) &&
		(TY_AR_lbnd_val(wnty,i) != TY_AR_lbnd_val(treety,i))) {
	      return FALSE;

	    } else if (TY_AR_const_ubnd(wnty,i) &&
		(TY_AR_ubnd_val(wnty,i) != TY_AR_ubnd_val(treety,i))) {
	      return FALSE;

	    } else {
              return WN2C_unparser_compatible_types(TY_AR_etype(wnty), 
	                                              TY_AR_etype(treety),
                                                      depth + 1);
            }
          }
	}

      case KIND_STRUCT:
	return (TY_Is_Structured(treety) &&
                  TY_flist(Ty_Table[wnty]) == TY_flist(Ty_Table[treety]));

      default:
	ErrMsg ( EC_Invalid_Case, 
                   "WN2C_unparser_compatible_types", __LINE__ );
	return FALSE;
    }
  }
} 

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Ldid
// Input:
//   arguments: 
//     WN * wn
//     TY_IDX treety
// Output:
//     void
// Description:
//     Normalize Ldid
//--------------------------------------------------------------------
void
Normalize_Ldid(WN *wn, TY_IDX treety)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX ty_pointed;
  INT objty_sz;
  INT ld_sz;
  TY_IDX wnty = WN_ty(wn);

  if (WN2C_Get_Treety(wn) != 0) {
    return;
  }

  objty = wnty;
  if (TY_kind(objty) == KIND_STRUCT) {
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(objty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
  }
  objty_sz = TY_size(objty);

  // if MTYPE_M, st_sz == 0
  if (WN_desc(wn) != MTYPE_M && WN_desc(wn) != MTYPE_BS) {
    // take care to bitwise support
    ld_sz = MTYPE_byte_size(WN_desc(wn));
    if (ld_sz != objty_sz) {
      if (MTYPE_is_integral(WN_desc(wn)) &&
          MTYPE_is_integral(TY_mtype(objty))) {
        DevWarn("object size not equal to WN_desc of ldid!");
        WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
        if (WN_field_id(wn) != 0) {
          WN_set_field_id(wn, 0);
          DevWarn("type uncompatible, Set field_id to 0!");
        }
      } else {
        Is_True(0, ("Size of WN_desc(wn) type != size of WN_ty(wn)"));
      }
    } 
  }

  /* Is_True(0, ("Imcompatible type in Normalize Ldid,"
              " may be nothing serious, but to grab the case")); */
  WN2C_Set_Treety(wn, treety);
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Stid
// Input:
//   arguments: 
//     WN * wn
// Output:
//     void
// Description:
//     Normalize Stid
//--------------------------------------------------------------------
void
Normalize_Stid(WN *wn)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX ty_pointed;
  INT objty_sz;
  INT st_sz;
  TY_IDX wnty = WN_ty(wn);

  objty = wnty;
  if (TY_kind(objty) == KIND_STRUCT) {
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(objty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
  }
  objty_sz = TY_size(objty);

  // if MTYPE_M, st_sz == 0
  if (WN_desc(wn) != MTYPE_M && WN_desc(wn) != MTYPE_BS) {
    // take care to bitwise support
    st_sz = MTYPE_byte_size(WN_desc(wn));
    if (st_sz != objty_sz) {
      if (MTYPE_is_integral(WN_desc(wn)) &&
          MTYPE_is_integral(TY_mtype(objty))) {
        DevWarn("object size not equal to WN_desc of stid!");
        WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
        if (WN_field_id(wn) != 0) {
          WN_set_field_id(wn, 0);
          DevWarn("type uncompatible, Set field_id to 0!");
        }
      } else {
        DevWarn("Not integral type: object size not equal to WN_desc of stid!");
        WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
        if (WN_field_id(wn) != 0) {
          WN_set_field_id(wn, 0);
          DevWarn("Not integral type: type uncompatible, Set field_id to 0!");
        }
      }
    } 
  }
 
  if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
    Normalize_Array(WN_kid0(wn), 0, FALSE);
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Lda
// Input:
//   arguments: 
//     WN * wn
//     TY_IDX treety
// Output:
//     void
// Description:
//     Normalize Lda
//--------------------------------------------------------------------
void
Normalize_Lda(WN *wn, TY_IDX treety)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  INT objty_sz;
  TY_IDX wnty = WN_ty(wn);

  if (WN2C_Get_Treety(wn) != 0) {
    return;
  }  

  // Is_True(TY_Is_Pointer(wnty), ("WN_ty of LDA is not a pointer type"));
  if (!TY_Is_Pointer(wnty)) {
    DevWarn("WN_ty(lda) is not a pointer type, change it to void *");
    WN_set_ty(wn, Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));    
  }

  #if 0
  objty = TY_pointed(wnty);
  if (TY_kind(wnty) == KIND_STRUCT) {
    STAB_OFFSET offt = WN_lda_offset(wn);
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(structty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
    else if (offt != 0) {
      Is_True(0, ("Can this case be removed?"
                  " check whether the whirl is canonical"));
    }
    // objty_sz = TY_size(objty);
  }
  #endif

  WN2C_Set_Treety(wn, treety);
}

////--------------------------------------------------------------------
// FUNCTION NAME: Make_Array_Type
// Input: 
//   TY_IDX elemty : array element ty
//   INT32 ndim : array dim
//   INT64 len : array length, assume equal length in every dim
// Output:
//   TY_IDX : array ty
// Description:
//   Make array type using array elemty, dim num, dim length 
//--------------------------------------------------------------------
extern TY_IDX
W2C_Make_Array_Type (TY_IDX elemty, INT32 ndim, INT64 len)
{
    FmtAssert(TY_size(elemty) > 0 && TY_align(elemty) > 0,
              ("Cannot make an array of %s", TY_name(elemty))); 
    ARB_HANDLE arb,arb_first;
    for (UINT i = 0; i < ndim; ++i) {
       arb = New_ARB ();
       if (i==0) {
	 arb_first = arb;
       }
       ARB_Init (arb, 0, len - 1, TY_size(elemty));
       Set_ARB_dimension (arb, ndim-i);
    }
    
    Set_ARB_last_dimen (arb);
    Set_ARB_first_dimen (arb_first);

    TY_IDX ty_idx;
    TY& ty = New_TY (ty_idx);
    TY_Init (ty, TY_size(elemty) * ndim * len, KIND_ARRAY,
	     MTYPE_M, 0);
    Set_TY_align (ty_idx, TY_align(elemty));
    Set_TY_etype (ty, elemty);
    Set_TY_arb (ty, arb_first);

    return ty_idx;

} 

//--------------------------------------------------------------------
// FUNCTION NAME: Get_Array_Tree_Ty
// Input:
//   arguments: 
//     WN * wn
//     TY_IDX elemty
//     BOOL needptr: need ptr or not in the highest dim
// Output:
//     TY_IDX treety
// Description:
//     return the array ty
//--------------------------------------------------------------------
TY_IDX
Get_Array_Tree_Ty(WN *wn, TY_IDX elemty, BOOL needptr)
{
  INT i;
  TY_IDX returnty = elemty;
  for (i = WN_num_dim(wn) - 1; i >= 1; i--) {
    WN *dim = WN_array_dim(wn, i);
    Is_True((WN_operator(dim) == OPR_INTCONST), 
              ("array dim is not a intconst"));
    returnty = W2C_Make_Array_Type(returnty, 1, WN_const_val(dim));
  }
  // the highest dim should be converted to a pointer
  // because array ty is hard to be used for typecast
  // wnty of ldid/lda should be a pointer
  if (needptr) {
    returnty = Make_Pointer_Type(returnty);
  } else {
    WN *dim = WN_array_dim(wn, 0);
    Is_True((WN_operator(dim) == OPR_INTCONST),
              ("array dim is not a intconst"));
    returnty = W2C_Make_Array_Type(returnty, 1, WN_const_val(dim));
  }
  return returnty;
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Base
// Input:
//   arguments: 
//     WN * wn
//     TY_IDX treety
// Output:
//     WN * wn
// Description:
//     Normalize Array Base
//--------------------------------------------------------------------
WN *
Normalize_Array_Base(WN *wn, WN *array, TY_IDX elemty)
{
  TY_IDX treety = 0;

  if (WN_operator(wn) == OPR_LDA) {
    treety = Get_Array_Tree_Ty(array, elemty, TRUE);
    Normalize_Lda(wn, treety);
  } else if (WN_operator(wn) == OPR_LDID) {
    treety = Get_Array_Tree_Ty(array, elemty, TRUE);
    Normalize_Ldid(wn, treety);
  } else if (WN_operator(wn) == OPR_ILOAD) {
    // Is_True(FALSE, ("Array Base is OPR_ILOAD"));
    treety = Get_Array_Tree_Ty(array, elemty, TRUE);
    Normalize_Iload(wn, treety);
  } else if (WN_operator(wn) == OPR_ARRAY) {
    treety = Get_Array_Tree_Ty(array, elemty, FALSE);
    Normalize_Array(wn, treety, TRUE); 
  } else if (WN_operator(wn) == OPR_ADD) {
    treety = Get_Array_Tree_Ty(array, elemty, TRUE);
    // bind tree type on wn
    WN2C_Set_Treety(wn, treety);
  } else {
    Is_True(FALSE, ("Normalize base of array"));
  }

}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Array
// Input:
//   arguments: 
//     WN * wn
//     TY_IDX objty : got from field of ILOAD/ISTORE WN_ty
//     BOOL rec : called when processing base of ARRAY 
// Output:
//     WN : wn
// Description:
//     Normalize Array
//--------------------------------------------------------------------
void
Normalize_Array(WN *wn, TY_IDX objty, BOOL rec)
{
  TY_IDX elemty;
  
  // not to process a node for more than once
  if (WN2C_Get_Treety(wn) != 0) {
    return;
  }

  if (!rec) {
    const WN *parent = W2CF_Get_Parent(wn);
/*	
    Is_True((WN_operator(parent) == OPR_ILOAD ||
             WN_operator(parent) == OPR_STID  ||
	     WN_operator(parent) == OPR_ISTORE), 
	("ARRAY parent should be ILOAD/ISTORE/STID"));  */
  }

  if (objty != 0) {

    INT64 elemsz = WN_element_size(wn);
    INT64 objsz = TY_size(objty);
    INT64 structsz = 0;

    if (elemsz != objsz) {
      // The context:
      // STRUCT *p; int i; 
      //   for (i = 0, p = &st; i < 10; i++, p+=3)
      //   after induction var recognition, p is removed 
      //   and p->a ==> arr[i].a (arr is 3 times the struct size)
      TY_IDX ty_idx =
          W2C_Make_Array_Type(Stab_Mtype_To_Ty(MTYPE_I1), 1, elemsz);
      elemty = ty_idx; 
    } else {
      elemty = objty;
    }
  } else {
    // when parent of array is STID, the objty == 0
    INT64 elemsz = WN_element_size(wn);
    elemty = W2C_Make_Array_Type(Stab_Mtype_To_Ty(MTYPE_I1), 1, elemsz);
    objty = elemty;
  }

  // bind tree type on array
  WN2C_Set_Treety(wn, Make_Pointer_Type(objty));

  Normalize_Array_Base(WN_kid0(wn), wn, elemty); 
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Iload
// Input:
//   arguments: 
//     WN * wn
// Output:
//     WN : wn
//     TY_IDX : treety, a empty interface
// Description:
//     Nomalize iload
//--------------------------------------------------------------------
void
Normalize_Iload(WN *wn, TY_IDX treety = 0)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX addrty;
  INT objty_sz;
  INT addrty_pt_sz;
  INT ld_sz;

  // not to process a node for more than once
  if (WN2C_Get_Treety(wn) != 0) {
    return;
  }

  objty = WN_ty(wn);
  addrty = WN_load_addr_ty(wn);
  if (TY_kind(WN_ty(wn)) == KIND_STRUCT) {
    structty = WN_ty(wn);
    STAB_OFFSET offt = WN_load_offset(wn);
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(structty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
    // if field id != 0, and ofst != 0, then
    // ofst cannot be the ofst of field in struct
    // so the objty is keep as WN_ty(wn)
  }
  objty_sz = TY_size(objty);
  addrty_pt_sz = TY_size(TY_pointed(addrty));

  // if MTYPE_M, st_sz == 0
  if (WN_desc(wn) != MTYPE_M && WN_desc(wn) != MTYPE_BS) {
    // take care to bitwise support
    ld_sz = MTYPE_byte_size(WN_desc(wn));
    if (ld_sz != objty_sz) {
      if (MTYPE_is_integral(WN_desc(wn)) &&
          MTYPE_is_integral(TY_mtype(objty))) {
        DevWarn("object size not equal to WN_desc of iload!");
        WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
        WN_set_load_addr_ty(wn, Make_Pointer_Type(MTYPE_To_TY(WN_desc(wn))));
        if (WN_field_id(wn) != 0) {
          WN_set_field_id(wn, 0);
          DevWarn("type uncompatible, Set field_id to 0!");
        }
      } else {
        Is_True(0, ("Size of WN_desc(wn) type != size of WN_ty(wn)"));
      }
    }
  }

  // if treety is not compatible with objty, use treety as the passing 
  // objty
  #if 0 
  if (treety != 0) {
    #if 0
    // set unconditionally, maybe objty and treety are 
    // WN2C_unparser_compatible_types(objty, treety) and need no cast
    // it is left to WN2C_based_lvalue_st / WN2C_based_lvalue_wn
    #endif

    if (TY_size(objty) != TY_size(treety)) {
      Is_True(WN_field_id(wn) == 0, 
               ("if WN_field_id(wn) != 0, we should set it to 0"));
      objty = treety;
      WN_set_ty(wn, treety);
      WN_set_load_addr_ty(wn, Make_Pointer_Type(treety)); 
    }
  }
  #endif
  WN2C_Set_Treety(wn, treety);

  addrty = WN_load_addr_ty(wn);
  if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
    Normalize_Array(WN_kid0(wn), TY_pointed(addrty), FALSE);  
  } else if (WN_operator(WN_kid0(wn)) == OPR_LDID) {
    Normalize_Ldid(WN_kid0(wn), addrty);  
  } else if (WN_operator(WN_kid0(wn)) == OPR_ILOAD) {
    Normalize_Iload(WN_kid0(wn), addrty);
  } else if (WN_operator(WN_kid0(wn)) == OPR_ADD) {
    WN2C_Set_Treety(WN_kid0(wn), addrty);
  } else if (WN_operator(WN_kid0(wn)) == OPR_SUB) {
    WN2C_Set_Treety(WN_kid0(wn), addrty);
  } else {
    WN2C_Set_Treety(WN_kid0(wn), addrty);
  }


  //Normalize_Wn_kid(WN_kid0(wn), addrty);
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Mload
// Input:
//   arguments: 
//     WN * wn
// Output:
//     WN : wn
//     TY_IDX : treety, a empty interface
// Description:
//     Nomalize mload is the same with Normalize iload
//--------------------------------------------------------------------
void
Normalize_Mload(WN *wn, TY_IDX treety = 0)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX addrty;
  INT objty_sz;
  INT ld_sz;

  // not to process a node for more than once
  if (WN2C_Get_Treety(wn) != 0) {
    return;
  }

  objty = TY_pointed(WN_ty(wn));

  if (TY_kind(WN_ty(wn)) == KIND_STRUCT) {
    structty = WN_ty(wn);
    STAB_OFFSET offt = WN_load_offset(wn);
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(structty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
    // if field id != 0, and ofst != 0, then
    // ofst cannot be the ofst of field in struct
    // so the objty is keep as WN_ty(wn)
  }
  objty_sz = TY_size(objty);

  // Mload has no desc/rtype

  #if 0
  // if treety is not compatible with objty, use treety as the passing 
  // objty 
  if (treety != 0) {
    #if 0
    // set unconditionally, maybe objty and treety are 
    // WN2C_unparser_compatible_types(objty, treety) and need no cast
    // it is left to WN2C_based_lvalue_st / WN2C_based_lvalue_wn
    #endif

    if (TY_size(objty) != TY_size(treety)) {
      Is_True(WN_field_id(wn) == 0, 
               ("if WN_field_id(wn) != 0, we should set it to 0"));
      WN_set_ty(wn, treety);
    }
  }
  #endif

  if ((treety != 0) && !WN2C_unparser_compatible_types(WN_ty(wn), treety)) {
    WN2C_Set_Treety(wn, treety);
  }

  if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
    Normalize_Array(WN_kid0(wn), objty, FALSE);  
  } else if (WN_operator(WN_kid0(wn)) == OPR_LDID) {
    Normalize_Ldid(WN_kid0(wn), Make_Pointer_Type(objty));  
  } else if (WN_operator(WN_kid0(wn)) == OPR_ILOAD) {
    Normalize_Iload(WN_kid0(wn), Make_Pointer_Type(objty));  
  } else if (WN_operator(WN_kid0(wn)) == OPR_ADD) {
    WN2C_Set_Treety(WN_kid0(wn), Make_Pointer_Type(objty));
  } else if (WN_operator(WN_kid0(wn)) == OPR_SUB) {
    WN2C_Set_Treety(WN_kid0(wn), Make_Pointer_Type(objty));
  } else {
    WN2C_Set_Treety(WN_kid0(wn), Make_Pointer_Type(objty));
  }

  //Normalize_Wn_kid(WN_kid0(wn), WN_ty(wn));
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Istore
// Input:
//   arguments: 
//     WN * wn
// Output:
//     WN : wn
//     TY_IDX : treety, a empty interface
// Description:
//     Nomalize iload
//--------------------------------------------------------------------
void
Normalize_Istore(WN *wn, TY_IDX treety = 0)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX wn_ty;
  TY_IDX ty_pointed;
  INT objty_sz;
  INT st_sz;

  wn_ty = WN_ty(wn);
  ty_pointed = TY_pointed(wn_ty);
  objty = ty_pointed;  

  Is_True(TY_kind(wn_ty) ==  KIND_POINTER, 
           ("WN_ty of ISTORE should be KIND_POINTER")); 
  if (TY_kind(ty_pointed) == KIND_STRUCT) {
    structty = ty_pointed;
    STAB_OFFSET offt = WN_store_offset(wn);
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(structty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
    // if field id != 0, and ofst != 0, then
    // ofst cannot be the ofst of field in struct
    // so the objty is keep as WN_ty(wn)
  }
  objty_sz = TY_size(objty);
  // if MTYPE_M, st_sz == 0
  if (WN_desc(wn) != MTYPE_M && WN_desc(wn) != MTYPE_BS) {
    st_sz = MTYPE_byte_size(WN_desc(wn));
    //Is_True(objty_sz == st_sz, ("object size not equal to WN_desc of istore!"));
  } 

  if (WN_operator(WN_kid1(wn)) == OPR_ARRAY) {
    Normalize_Array(WN_kid1(wn), ty_pointed, FALSE);  
  } else if (WN_operator(WN_kid1(wn)) == OPR_LDID) {
    Normalize_Ldid(WN_kid1(wn), Make_Pointer_Type(ty_pointed)); 
  } else if (WN_operator(WN_kid1(wn)) == OPR_ILOAD) {
    Normalize_Iload(WN_kid1(wn), Make_Pointer_Type(ty_pointed));  
  } else if (WN_operator(WN_kid1(wn)) == OPR_ADD) {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  } else if (WN_operator(WN_kid1(wn)) == OPR_SUB) {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  } else {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  }

  // Normalize_Rec(WN_kid0(wn));
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Mstore
// Input:
//   arguments: 
//     WN * wn
// Output:
//     WN : wn
//     TY_IDX : treety, a empty interface
// Description:
//     Normalize mstore is almost the same with normalize istore 
//--------------------------------------------------------------------
void
Normalize_Mstore(WN *wn, TY_IDX treety = 0)
{
  TY_IDX structty = 0;
  TY_IDX objty;
  TY_IDX wn_ty;
  TY_IDX ty_pointed;
  INT objty_sz;
  INT st_sz;

  wn_ty = WN_ty(wn);
  ty_pointed = TY_pointed(wn_ty);
  objty = ty_pointed;  

  Is_True(TY_kind(wn_ty) ==  KIND_POINTER, 
           ("WN_ty of ISTORE should be KIND_POINTER")); 
  if (TY_kind(ty_pointed) == KIND_STRUCT) {
    structty = ty_pointed;
    STAB_OFFSET offt = WN_store_offset(wn);
    if (WN_field_id(wn) != 0) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(structty, 
                                          WN_field_id(wn), 
                                          cur_field_id);
      objty = FLD_type(fld);
    }
    // if field id != 0, and ofst != 0, then
    // ofst cannot be the ofst of field in struct
    // so the objty is keep as WN_ty(wn)
  }
  objty_sz = TY_size(objty);

  // Mstore has no desc

  if (WN_operator(WN_kid1(wn)) == OPR_ARRAY) {
    Normalize_Array(WN_kid1(wn), ty_pointed, FALSE);  
  } else if (WN_operator(WN_kid1(wn)) == OPR_LDID) {
    Normalize_Ldid(WN_kid1(wn), Make_Pointer_Type(ty_pointed)); 
  } else if (WN_operator(WN_kid1(wn)) == OPR_ILOAD) {
    Normalize_Iload(WN_kid1(wn), Make_Pointer_Type(ty_pointed)); 
  } else if (WN_operator(WN_kid1(wn)) == OPR_ADD) {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  } else if (WN_operator(WN_kid1(wn)) == OPR_SUB) {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  } else {
    WN2C_Set_Treety(WN_kid1(wn), Make_Pointer_Type(ty_pointed));
  }

}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Parm
// Input:
//   arguments: 
//     WN * wn
// Output:
// Description:
//     Normalize Parm kids 
//--------------------------------------------------------------------
void
Normalize_Parm(WN *wn) 
{
  if (WN_operator(WN_kid0(wn)) == OPR_LDA) {
    Normalize_Lda(WN_kid0(wn), 0);
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Check_If_Va_Start 
// Input:
//   arguments: 
//     WN * wn
//     WN * func 
// Output:
//     BOOL : find va_start or not
// Description:
//     If call va_start appear in pu, change pu attr to varargs
//     which may be reset in IPA if IPA finds actually the num of
//     arg is invariant.
//--------------------------------------------------------------------
BOOL
Check_If_Va_Start(WN *wn, WN *func) 
{
  if (strcmp(ST_name(WN_st(wn)), "va_start") == 0) {
    Set_TY_is_varargs(ST_type(WN_st(func)));
    return TRUE;
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Normalize_Rec
// Input:
//   arguments: 
//     WN * wn
//     WN * func
// Output:
//     void 
// Description:
//     Nomalize pu recursively
//--------------------------------------------------------------------
void 
Normalize_Rec(WN *wn, WN *func) 
{
  if (WN_operator(wn) == OPR_LDID) {
    Normalize_Ldid(wn, 0);
  
  } else if (WN_operator(wn) == OPR_STID) {
    Normalize_Stid(wn);

  } else if (WN_operator(wn) == OPR_ILOAD) {
    Normalize_Iload(wn);

  } else if (WN_operator(wn) == OPR_MLOAD) {
    Normalize_Mload(wn);
    
  } else if (WN_operator(wn) == OPR_ISTORE) {
    Normalize_Istore(wn);

  } else if (WN_operator(wn) == OPR_MSTORE) {
    Normalize_Mstore(wn);

  } else if (WN_operator(wn) == OPR_LDA) {
    Normalize_Lda(wn, 0);

  }else if (WN_operator(wn) == OPR_ARRAY) {
    Normalize_Array(wn, 0, FALSE);

  }else if (WN_operator(wn) == OPR_CALL) {
    Check_If_Va_Start(wn, func);

  } else if (WN_operator(wn) == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
      Normalize_Rec(stmt, func);
    }
  } 

  for (INT i = 0; i < WN_kid_count(wn); i++) {
    Normalize_Rec(WN_kid(wn, i), func);
  }
}

