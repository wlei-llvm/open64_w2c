//-----------------------------------------------------------------------------
// MODULE:    IPL Alignment analysis module
// File Name:  ipl_align.cxx
// Author:     miwei
// Abstract:   collect align info of actual parm in IPL align phase 
// Date:       2005-9-16
// Copy Right: 2004-2006, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#include "ipl_align.h"
#include "ipa_align.h"
#include "ipl_summary.h"

#include "loop_info.h"
#include "symtab_defs.h"
#include "symtab_compatible.h"
#include "wn_core.h"
#include "access_vector.h"
#include "wutil.h"

// W is the width of a vector register
#define W 16

ARRAY_ALIGN_SUMMARY Array_Align_Summary;
extern MEM_POOL IPL_local_pool;
extern WN_MAP Parent_Map;
extern WN_MAP IPL_info_map;

//--------------------------------------------------------------------
// FUNCTION NAME: Is_Fortran
// Description:
//   the pu is fortran or not
//--------------------------------------------------------------------
static BOOL Is_Fortran()
{
  const PU& pu = Get_Current_PU ();
  return (PU_f77_lang (pu) || PU_f90_lang (pu));
}

//--------------------------------------------------------------------
// FUNCTION NAME: Get_Array_Header_Addr
// Description:
//   get array header addr according to its storage type, auto
// global, static array is assumed to align to 16 bytes boundary
// common block is assumed to align to 16 bytes boundary  
//--------------------------------------------------------------------
mINT32 Get_Array_Header_Addr(ST *st)
{
  TY_IDX ty_idx = ST_type(st);
  Is_True((TY_kind(ty_idx) == KIND_ARRAY 
                  || TY_kind(ty_idx) == KIND_STRUCT
  	           || (strcmp(TY_name(ty_idx), ".dope.") == 0))
                  , ("st is not a array sym"));
  
  if ((ST_sclass(st) == SCLASS_AUTO) || (ST_sclass(st) == SCLASS_FORMAL)
       || (ST_sclass(st) == SCLASS_UGLOBAL) 
       || (ST_sclass(st) == SCLASS_DGLOBAL)
       || (ST_sclass(st) == SCLASS_PSTATIC) 
       || (ST_sclass(st) == SCLASS_FSTATIC)
       || (strcmp(TY_name(ty_idx), ".dope.") == 0)) {
    // we assume auto formal global static arrays are all 16 bytes aligned
    return 0;
  }
  else if (ST_sclass(st) == SCLASS_COMMON) {
    // how to take allocate arrays into account???
    UINT64 ofst = ST_ofst(st);
    mINT32 header_addr = ofst % W;
    return header_addr;
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Evaluate_Align_Inipl
// Description:
//   evaluate actual array align info in ipl phase, it is similar 
// with the align part in lno phase. because the whirl structure
// is much simpler than that in lno phase, so the elem size is still
// fetched from symtab. another main difference lies in that if
// the coeff of a loop index var mod W != 0, think the align info
// of the array ref point is unknown
//--------------------------------------------------------------------
void Evaluate_Align_Inipl(BOOL cvt_from_pointer,
                                               WN *wn,
                                               ACCESS_ARRAY *array, 
                                               ACTUAL_ALIGN_INFO *actual_align_info, 
                                               mINT32 header_addr)
{
  UINT32 span;    //the span of certain array dim 
  INT64 offset;   //offset relative to array header addr

  if (array->Too_Messy) {
    actual_align_info->Set_Lattice_btm();
    return;
  }

  //initial val of span is the array element size
  if (cvt_from_pointer)
    span = 1;
  else
    span = WN_element_size(wn);
  offset = 0;

  INT i;
  for (i = array->Num_Vec() - 1; i >= 0; i--) {
    // check if the array bound is const and span of relative dim mod W is 0
    if (i != array->Num_Vec() - 1) {
      WN *dim = WN_array_dim(wn, i + 1);
      if (WN_operator(dim) == OPR_INTCONST)
        span *= WN_const_val(dim);
      else if (WN_operator(dim) == OPR_LDID) 
        return;
      // stride mod w == 0, don't need to process the dims in succession
      // and the align info must have been fixed.
      if (span % W == 0) {
        break;
      }
    }// end of if (i != array->Num_Vec() - 1)

    ACCESS_VECTOR *vector = array->Dim(i);
    if (vector == NULL || vector->Too_Messy) {
      actual_align_info->Set_Lattice_btm();
      return;
    }

    // check the coeff of nonlinear symbol if coeff mod W == 0
    if (vector->Non_Lin_Symb != NULL) {
      SUMPROD_ITER iter(vector->Non_Lin_Symb);
      for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); node = iter.Next())
        if (((node->Coeff * span) % W) != 0) { 
          actual_align_info->Set_Lattice_btm();
          return;
        }
    }

    // check the coeff of linear symbol if coeff mod W == 0
    if (vector->Lin_Symb != NULL) {
      INTSYMB_ITER iter(vector->Lin_Symb);
      for (INTSYMB_NODE* node = iter.First(); !iter.Is_Empty(); node = iter.Next())
        if (((node->Coeff * span) % W) != 0) {
          actual_align_info->Set_Lattice_btm();
          return;
        }
    }
     
    // check the coeff of loop index symbol if coeff mod W == 0
    INT j;
    for (j = 0; j < vector->Nest_Depth(); j++)
      if (((vector->Loop_Coeff(j) * span) % W) != 0) {
        actual_align_info->Set_Lattice_btm();
        return;
      }
    // accumulate array ref point's offset relative to array header
    offset = vector->Const_Offset * span;
    if (vector->Const_Offset < 0) {
      // a(2i-1) vector->Const_Offset == -2, we change -2 ==> -2 + slot = 2
      offset = (-offset / W + 1) * W + offset;
    }
  }//end of for (i = array->Num_Vec()-1; i >= 0; i++)

  // now align info is fixed, compute the align info
  mINT32 align = (header_addr + offset) % W;
  actual_align_info->Set_stride(W);       //wait to be delete
  actual_align_info->Set_offset(align);
  actual_align_info->Set_Lattice_mid();
}

//--------------------------------------------------------------------
// FUNCTION NAME: Generate_Actual_Access_Array
// Description:
//   we don't have access_array structure in ipa phase, output 
// actual array ref point's ACCESS_ARRAY to ACTUAL_ACCESS_ARRAY 
// for ipa use. write every part in ACCESS_ARRAY into 
// ACTUAL_ACCESS_ARRAY. change every pointer into array index
//--------------------------------------------------------------------
void Generate_Actual_Access_Array(ACCESS_ARRAY *a, 
                                  ACTUAL_ACCESS_ARRAY *actual_access_array)
{
  INT i;
  ACTUAL_ACCESS_VECTOR_ARRAY *actual_access_vector_array = 
                      Array_Align_Summary.Get_actual_access_vector_array();
  ACTUAL_ACCESS_VECTOR *actual_access_vector;

  // process from the lowest dim to the highest, Dim(0) is always the highest.
  for (i = a->Num_Vec() - 1; i >= 0; i--) {
    ACCESS_VECTOR *vector = a->Dim(i);
    if (vector == NULL) 
      continue;
    else if (vector->Too_Messy == TRUE) {
      actual_access_array->Too_Messy = TRUE;
      return;
    }    
      
    mINT32 idx = actual_access_vector_array->Newidx();
    actual_access_vector = &(*actual_access_vector_array)[idx];
    actual_access_vector->Init();
    if (i == a->Num_Vec() - 1) 
      actual_access_array->Set_access_vector_index(idx);

    // write Const_Offset, Nest_Depth, Non_const_loops in 
    //    access_vector to actual_access_vector
    actual_access_vector->Set_Const_Offset(vector->Const_Offset);
    actual_access_vector->Set_nest_depth(vector->Nest_Depth());
    actual_access_vector->Set_non_const_loops(vector->Non_Const_Loops()); 

    // write Loop_Coeff in access_vector to actual_access_vector
    if (vector->Nest_Depth() > 0) {
      INT k;
      for (k = 0; k < vector->Nest_Depth(); k++) {
        actual_access_vector->Set_Loop_Coeff(k, vector->Loop_Coeff(k));
      }// end of for (k = 0; k < Nest_Depth() ...)
    }// end of if (vector->Nest_Depth() > 0)   

    // write Lin_Symb in access_vector to actual_access_vector
    BOOL first = TRUE;
    if (vector->Lin_Symb != NULL) {
      INTSYMB_ITER iter(vector->Lin_Symb);
      for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
        ALIGN_INTSYMB_NODE_ARRAY *align_intsymb_node_array = 
                                     Array_Align_Summary.Get_align_intsymb_node_array();
        idx = align_intsymb_node_array->Newidx();
        ALIGN_INTSYMB_NODE *align_intsymb_node = &(*align_intsymb_node_array)[idx];
        align_intsymb_node->Init();
        align_intsymb_node->Set_Coeff(node->Coeff);
        if (first == TRUE) {
          actual_access_vector->Set_lin_symb_index(idx);
          first = FALSE;
        }
        // here we assume we don't use INTSYMB_NODE::_symbol_index for the moment
        /*        node->Symbol.
        Summary->Get_symbol_index(st)
        align_intsymb_node->Set_symbol_index(); */
        actual_access_vector->Set_lin_symb_count(actual_access_vector->Get_lin_symb_count() + 1);
      }//end of for (INTSYMB_NODE *node = iter.First(), ... )
    }//end of if (vector->Lin_Symb ...) 
    
    // write Non_Lin_Symb in access_vector to actual_access_vector
    first = TRUE;
    if (vector->Non_Lin_Symb != NULL) {
      SUMPROD_ITER iter(vector->Non_Lin_Symb);
      for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
        ALIGN_SUMPROD_NODE_ARRAY *align_sumprod_node_array = 
                                     Array_Align_Summary.Get_align_sumprod_node_array();
        idx = align_sumprod_node_array->Newidx();
        ALIGN_SUMPROD_NODE *align_sumprod_node = &(*align_sumprod_node_array)[idx];
        align_sumprod_node->Init();
        align_sumprod_node->Set_Coeff(node->Coeff);
        if (first == TRUE) {
          actual_access_vector->Set_non_lin_symb_index(idx);
          first = FALSE;
        }
        // here we assume we don't use SUMPROD_NODE::_symbol_index[] for the moment
        actual_access_vector->Set_non_lin_symb_count(actual_access_vector->Get_non_lin_symb_count() + 1);
      }//end of for (SUMPROD_NODE *node = ...)
    }//end of if (vector->Non_Lin_Symb ...)
  }//end of for(i = Num_Vec()-1; i >=0; i--)   
  
  actual_access_array->Set_access_vector_count(a->Num_Vec());

}

//--------------------------------------------------------------------
// FUNCTION NAME: Pointer_Processing
// INPUT: 
//   INT span: means the size of the pointer ref object
// Description:
//   used in pointer align analysis.
//   if pointer ref is converted into array ref in whirl, 
// its processing is the same with array ref. Here we process the 
// addr a pointer points to plus offset case, for example the whirl
// of pointer ref p[i] is 
//    LDID p
//     LDID i
//     INTCONST sizeof(struct p points to)
//    MPY
//   ADD  
//  ILOAD
//  the func build access array for the pointer ref offset expr
//--------------------------------------------------------------------
static ACCESS_ARRAY *Pointer_Processing(WN *wn, 
                                 DOLOOP_STACK *stack, 
                                 MEM_POOL *pool)
{
  ACCESS_ARRAY *array = CXX_NEW(ACCESS_ARRAY(1,stack->Elements(),
				pool),pool);
  array->Dim(0)->Set(wn, stack, 1, 0, 1);
  return array;
}

//--------------------------------------------------------------------
// FUNCTION NAME: Build_Doloop_Stack_Rec 
// Description:
//   build loop stack for wn recursively
//--------------------------------------------------------------------
static void Build_Doloop_Stack_Rec(WN* wn, WN *parent,DOLOOP_STACK* stack)
{
  if (parent) {
    Build_Doloop_Stack_Rec(parent,(WN *)WN_MAP_Get(Parent_Map, parent),stack);
    if (WN_opcode(parent) == OPC_DO_LOOP && 
	WN_do_body(parent) == wn) {
      stack->Push(parent);
    }
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Build_Doloop_Stack 
// Description:
//   build loop stack for wn
//--------------------------------------------------------------------
void Build_Doloop_Stack(WN* wn, DOLOOP_STACK* stack)
{
  if (wn) {
    WN *parent = (WN *)WN_MAP_Get(Parent_Map, wn);
    Build_Doloop_Stack_Rec(wn,parent,stack);
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      stack->Push(wn);
    }
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: WN_Is_Formal
// Description:
//   Find the st in the wn is a formal or not
//--------------------------------------------------------------------
BOOL WN_Is_Formal(WN *wn) {
  if (WN_operator(wn) != OPR_LDID && WN_operator(wn) != OPR_LDA)
    return FALSE;
  WN *pu = wn;
  while (WN_operator(pu) != OPR_FUNC_ENTRY) {
    pu = (WN *)WN_MAP_Get(Parent_Map, pu);
  }

  for (INT i = 0; i < WN_num_formals(pu); i++) {
    ST *st = WN_st(WN_formal(pu, i));
    if (WN_st(wn) == st)
      return TRUE;
  }
  return FALSE;
}

//--------------------------------------------------------------------
// FUNCTION NAME: Process_Actual_Align_Inipl
// Description:
//   process actual array ref points, analyse the subscription
// expression and get align info, meanwhile, save the subscription
// for ipa use 
//--------------------------------------------------------------------
void Process_Actual_Align_Inipl (SUMMARY_ACTUAL *summary_actual, WN *wn) 
{       
  // init must be placed before ipl Preorder_Process_PUs,it is 
  // placed in Ipl_init() at last
  // Array_Align_Summary.Init(); 
  ACTUAL_ALIGN_INFO_ARRAY *actual_align_info_array 
      = Array_Align_Summary.Get_actual_align_info_array();
  ACTUAL_ACCESS_ARRAY_ARRAY *actual_access_array_array
      = Array_Align_Summary.Get_actual_access_array_array();

  // mpool = Array_Align_Summary.Get_mem_pool();
  // generate a new ACTUAL_ALIGN_INFO
  INT32 alg_idx = actual_align_info_array->Newidx();
  ACTUAL_ALIGN_INFO *actual_align_info = &(*actual_align_info_array)[alg_idx];
  actual_align_info->Init();
  summary_actual->Set_align_index(alg_idx);        

  if (Get_Trace(TKIND_IR, TP_IPL)) {
    WN *parm = (WN *)WN_MAP_Get(Parent_Map, wn);
    WN *call = (WN *)WN_MAP_Get(Parent_Map, parm);
    INT i;
    for (i = 0; i < WN_kid_count(call); i++) {
      if (WN_kid(call, i) == parm)
        break;
    }
    if (WN_operator(call) == OPR_INTRINSIC_CALL)
      fprintf(TFile, "Actual param %d in Callsite %s <7:%d>\n",
              i, get_intrinsic_name(WN_intrinsic(call)), WN_map_id(call));
    else
      fprintf(TFile, "Actual param %d in Callsite %s <7:%d>\n",
              i, ST_name(WN_st(call)), WN_map_id(call));
    fprintf(TFile, "\tIts align idx is: %d\n", alg_idx);
  }

  BOOL is_formal = FALSE;
  ACCESS_ARRAY *array = NULL;
  INT off = 0;
  INT header_addr = 0;
  // we assume the problem that multidim array ref in C is linearized 
  // is solved.
  // the kid of iload may be ADD, LDID or LDA
  if (WN_operator(wn) == OPR_ADD) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY
        && WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
      off += WN_const_val(WN_kid1(wn));
      wn = WN_kid0(wn);
    }   
    else if (WN_operator(WN_kid1(wn)) == OPR_ARRAY
             && WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
      off += WN_const_val(WN_kid0(wn));
      wn = WN_kid1(wn);
    }   
    else if ((WN_operator(WN_kid0(wn)) == OPR_LDID
		      && TY_kind(WN_ty(WN_kid0(wn))) == KIND_POINTER)
             && (WN_operator(WN_kid1(wn)) != OPR_LDID
                 || TY_kind(WN_ty(WN_kid1(wn))) != KIND_POINTER)) {
      wn = WN_kid1(wn);
    }       
    else 
      return;
  }
  else if (WN_operator(wn) == OPR_LDID && WN_Is_Formal(wn)) {
    ST_IDX st_idx = WN_st_idx(wn);
    TY_IDX ty_idx = ST_type(st_idx);
    if (TY_kind(ty_idx) == KIND_POINTER) {
      actual_align_info->Set_is_formal_array_ref();
      actual_align_info->Set_offset(WN_load_offset(wn));
    }
    return;
  }
  else if (WN_operator(wn) == OPR_LDA) {
    ST_IDX st_idx = WN_st_idx(wn);
    TY_IDX ty_idx = ST_type(&St_Table[st_idx]);
    if (TY_kind(ty_idx) == KIND_ARRAY) {
      header_addr = Get_Array_Header_Addr(&St_Table[st_idx]); 
      actual_align_info->Set_Lattice_mid();
      actual_align_info->Set_offset(header_addr + WN_lda_offset(wn));
    }
    return;
  }
  else if (WN_operator(wn) == OPR_ARRAY)
    ;
  else
    return;
  
  ST *st = NULL;
  BOOL cvt_from_pointer = FALSE;
  // after preprocessing of the kid of iload, the wn must be ARRAY or a pointer
  if (WN_operator(wn) == OPR_ARRAY) {
    // nested array appear in f90 program
    if (WN_operator(WN_array_base(wn)) == OPR_LDA)
      off += WN_lda_offset(WN_array_base(wn));
    else if (WN_operator(WN_array_base(wn)) == OPR_LDID)
      off += WN_load_offset(WN_array_base(wn));
    else 
      return;
    is_formal = WN_Is_Formal(WN_array_base(wn));
    st = WN_st(WN_array_base(wn));
    array = (ACCESS_ARRAY *)WN_MAP_Get(IPL_info_map, (WN*)wn);
    if (!is_formal && WN_operator(WN_array_base(wn)) == OPR_LDID)
      return;
  }
  else {
    WN *parent = (WN*)WN_MAP_Get(Parent_Map, (WN*)wn);
    // we add a processing to pointer for ILOAD/ISTORE header addr + offset 
    // fashion pointer ref here
    if (WN_operator(WN_kid0(parent)) == OPR_LDID)
      off += WN_load_offset(WN_kid0(parent));
    else if (WN_operator(WN_kid0(parent)) == OPR_LDA) 
      off += WN_lda_offset(WN_kid0(parent));
    else
      // (*atomlist)[i].who in ammp is an example, its whirl is 
      //    ARRAY
      //   ILOAD
      //  ILOAD
      return;
    is_formal = WN_Is_Formal(WN_kid0(parent));
    if (is_formal) 
      st = WN_st(WN_kid0(parent));
    else 
      // now we have no dataflow support in a pu, so we dismiss such case
      // call malloc
      // ldid rt1
      return;
    DOLOOP_STACK *stack =
          CXX_NEW(DOLOOP_STACK(&IPL_local_pool), &IPL_local_pool);
    // outer loop at the bottom of the stack
    Build_Doloop_Stack(wn, stack);
    array = Pointer_Processing(wn, stack, &IPL_local_pool);    
    cvt_from_pointer = TRUE;
  }

  INT arr_idx;
  if (is_formal)
  {     
    // we can get the formal index from summary_symbol findex
    actual_align_info->Set_is_formal_array_ref();

    // when actual array ref point is a formal array,
    // output ACCESS_ARRAY to ACTUAL_ACCESS_ARRAY
    arr_idx = actual_access_array_array->Newidx();
    ACTUAL_ACCESS_ARRAY *actual_access_array = &(*actual_access_array_array)[arr_idx];
    actual_access_array->Init();
    actual_access_array->Set_cvt_from_pointer(cvt_from_pointer);
    summary_actual->Set_array_index(arr_idx);
    // ACTUAL_ACCESS_ARRAY generation
    Generate_Actual_Access_Array(array, actual_access_array); 
    if (Get_Trace(TKIND_IR, TP_IPL)) 
      fprintf(TFile, "\tIts array idx is: %d\n", arr_idx);
    return;
  }

  header_addr = Get_Array_Header_Addr(st); 
  // wn is used only when wn opr is array, when cvt_from_pointer true, wn is of no use
  // and can be anything
  Evaluate_Align_Inipl(cvt_from_pointer, wn, array, actual_align_info, header_addr+off);
}

