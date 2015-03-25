//-----------------------------------------------------------------------------
// MODULE:     IPA Alignment analysis module
// File Name:  ipa_align.cxx 
// Author:     miwei
// Abstract:   dataflow analysis part in IPA align phase 
// Date:       2005-9-16
// Copy Right: 2004-2007, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#define __STDC_LIMIT_MACROS
#include <stdint.h>
#include <alloca.h>
#include <elf.h>

#include "mempool.h"
#include "srcpos.h"
#include "ipl_summary.h"		// summary info
#include "ipa_option.h"			// IPA_TRACE flags
#include "ipa_summary.h"                // IPA_get_ functions
#include "ipl_align.h"
#include "ipa_align.h"
#include "ipa_cg.h"
#include "ipa_align_analysis.h"
#include "ipo_defs.h"

static SUMMARY_PROCEDURE *ipa_proc;
static SUMMARY_CALLSITE *ipa_callsite;
static SUMMARY_FORMAL *ipa_formal;
static SUMMARY_ACTUAL *ipa_actual;
static SUMMARY_SYMBOL *ipa_symbol;
static ACTUAL_ALIGN_INFO *ipa_align_info;
static ACTUAL_ACCESS_ARRAY *ipa_access_array;
static ACTUAL_ACCESS_VECTOR *ipa_access_vector;
static ALIGN_INTSYMB_NODE *ipa_intsymb_node;
static ALIGN_SUMPROD_NODE *ipa_sumprod_node;

//--------------------------------------------------------------------
// FUNCTION NAME: Set_summary_info 
// Description:
//   set summary info of the ipa node, these various kinds of summary
// array contains corresponding info collected in the pu in ipl phase.
//--------------------------------------------------------------------
static void Set_summary_info(const IPA_NODE *node)
{
  ipa_proc = node->Summary_Proc();
 
  ipa_callsite     = IPA_get_callsite_array (node);
  ipa_actual       = IPA_get_actual_array (node);
  ipa_formal       = IPA_get_formal_array (node);
  ipa_symbol       = IPA_get_symbol_array (node);
  ipa_align_info   = IPA_get_align_info (node);
  ipa_access_array = IPA_get_access_array (node);
  ipa_access_vector = IPA_get_access_vector (node);
  ipa_intsymb_node = IPA_get_intsymb_node (node);
  ipa_sumprod_node = IPA_get_sumprod_node (node); 
}

#define W 16

//--------------------------------------------------------------------
// FUNCTION NAME: Ipa_align_trace 
// Description:
//   output the align info of every formal param of a ipa node
//----------------------------------------------------------------------
static void
Ipa_align_trace (IPA_NODE* node)
{
  char buffer[64];

  ALIGN_DYN_ARRAY *formal_annot = node->Align_Annot();
  if (formal_annot && formal_annot != (ALIGN_DYN_ARRAY*) -1) {

    for (INT i = 0; i < formal_annot->Elements(); i++) {

      if ((*formal_annot)[i].Is_Lattice_mid()) {
        sprintf(buffer, "Position %d: lattice mid, stride %d, offset %d", 
                 i, (*formal_annot)[i].Get_stride(), (*formal_annot)[i].Get_offset());
        fprintf(TFile, "\n%s %s node: %s\n",
                 "IPA", "Align Analysis", DEMANGLE(node->Name()));
        fprintf(TFile, "{ %s }\n", buffer);
      }// end if ( (*formal_annot ...)
      else if ((*formal_annot)[i].Is_Lattice_top()) {
        sprintf(buffer, "Position %d: lattice top, stride %d", 
                 i, (*formal_annot)[i].Get_stride());
        fprintf(TFile, "\n%s %s node: %s\n",
                 "IPA", "Align Analysis", DEMANGLE(node->Name()));
        fprintf(TFile, "{ %s }\n", buffer);
      }
      else {
        sprintf(buffer, "Position %d: lattice btm, stride %d", 
                 i, (*formal_annot)[i].Get_stride());
        fprintf(TFile, "\n%s %s node: %s\n",
                 "IPA", "Align Analysis", DEMANGLE(node->Name()));
        fprintf(TFile, "{ %s }\n", buffer);
      }

    }// end for (INT i = 0 ...)
  }// end if (formal_annot && ...)
}

//--------------------------------------------------------------------
// FUNCTION NAME: Init_Align_Annotations
// Description:
//   initialize node annotations of formal params. if a call is externally callable, 
// simply set the annotation to be Lattice_Btm
//----------------------------------------------------------------------
void
Init_Align_Annotations (IPA_NODE* node)
{
  //Set_summary_info(node);

  INT count = node->Num_Formals(); 
  if (count > 0) {
    ALIGN_DYN_ARRAY* annot = CXX_NEW(ALIGN_DYN_ARRAY(Malloc_Mem_Pool), 
                                     Malloc_Mem_Pool);
    annot->Force_Alloc_array (count);
    annot->Setidx (count-1);
    for (INT j = 0; j < count; ++j) {
      (*annot)[j].Init();
    }
    node->Set_Align_Annot(annot);
    // process indirectly incoming call edge
    if (node->Is_Externally_Callable()) {
      for (INT j = 0; j < count; ++j) {
        (*annot)[j].Set_Lattice_btm();
      }
      return;
    }//end if (node->Is_Externally...)
    
    if (node->Has_Varargs()) {
      (*annot)[count-1].Set_Lattice_btm();
    }
 
  }// end if (count > 0)
  else {
    node->Set_Align_Annot (NULL);
  }
}

//--------------------------------------------------------------------
// FUNCTION NAME: Set_summary_info 
// Description:
//   dataflow iteration initialize func
//----------------------------------------------------------------------

void
IPA_ALIGN_DF_FLOW::InitializeNode(void* node)
{
  Init_Align_Annotations ((IPA_NODE*)node);
} //IPA_ALIGN_DF_FLOW::InitializeNode

//--------------------------------------------------------------------
// FUNCTION NAME: Union_Align_Info 
// Description:
//   Union every actual's align info with formal align info. union operation is
// based on align lattice.
//----------------------------------------------------------------------
BOOL 
Union_Align_Info(ACTUAL_ALIGN_INFO *formal, ACTUAL_ALIGN_INFO *actual)
{
  // need to define a construct func, use actual as the param
  // Is there anything wrony about different type data 's stride
  // in Union op ???
  if (actual == NULL) return FALSE;

  if ((formal == NULL) && (actual != NULL)) {
    formal = CXX_NEW(ACTUAL_ALIGN_INFO(), Malloc_Mem_Pool);
    formal->Set_Lattice(actual->Get_Lattice());
    formal->Set_stride(actual->Get_stride());
    formal->Set_offset(actual->Get_offset());
    return TRUE;
  }

  if (formal->Is_Lattice_top() && !actual->Is_Lattice_top()) {
    formal->Set_Lattice(actual->Get_Lattice());
    formal->Set_stride(actual->Get_stride());
    formal->Set_offset(actual->Get_offset());
    return TRUE;
  }

  // actual align info should not be top, wait to be work out.
  /*if (formal->Is_Lattice_mid() && actual->Is_Lattice_top()) {
    return TRUE;
  }*/

  if (formal->Is_Lattice_mid() && actual->Is_Lattice_btm()) {
    formal->Set_Lattice_btm();
    return TRUE;
  }
  // formal and actual both have fix align info, check if they 
  // are equal
  if (formal->Is_Lattice_mid() && actual->Is_Lattice_mid()) {

    if (formal->Get_stride() == actual->Get_stride()) {

      if (formal->Get_offset() == actual->Get_offset()) {
        return FALSE;
      }
      else {
        formal->Set_Lattice_btm();
        return TRUE;
      }

    }
    else {
      DevWarn("stride not equal, to be processed");       
    }

  }
  
  return FALSE;
}

//--------------------------------------------------------------------
// FUNCTION NAME: Union_Align_Annot 
// Description:
//   Union operation, union every edge annot to node annot
//----------------------------------------------------------------------
BOOL 
Union_Align_Annot(IPA_NODE *callee, IPA_EDGE *edge)
{

  // Also if there is a recursive in-edge, we cannot propagate the
  // formal parameter value down. Because the align info of the
  // parameters may not have been determined yet. So, don't consider
  // the formal parameter values in flow analysis below

  // suppose we don't process recursive case, and see what will happen
  /*if (edge && edge->Is_Recursive()) {
    formal_align = 0;  //how to process recursion ???
    break;
  }*/


  BOOL change = FALSE;
  ALIGN_DYN_ARRAY* formals = callee->Align_Annot();
  ALIGN_DYN_ARRAY* actuals = edge->Align_Annot();

  if (formals == NULL || actuals == NULL) {
    return FALSE;
  }

  // check to see if there are more actuals than formals
  UINT32 count = (formals->Elements() < actuals->Elements()) ? 
                  formals->Elements() : actuals->Elements();

  // Set_summary_info (callee); need or not???
  for (UINT32 pos = 0; pos < count; ++pos) {
     ACTUAL_ALIGN_INFO& actual_align = (*actuals)[pos];
     ACTUAL_ALIGN_INFO& formal_align = (*formals)[pos];
    if (Union_Align_Info(&formal_align, &actual_align))
      change = TRUE; 
  }
  return change;
}

//--------------------------------------------------------------------
// FUNCTION NAME: IPA_ALIGN_DF_FLOW::Meet
// Description:
//   Meet operation, union every in edge of a ipa node in the call 
// graph to get the dataflow info of the node 
//----------------------------------------------------------------------

void*
IPA_ALIGN_DF_FLOW::Meet(void*, void* vertex, INT* change)
{
  IPA_NODE *n = (IPA_NODE *) vertex;

  // Iterate over all incoming edges
  IPA_PRED_ITER edge_iter(n);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e) {
      if (Union_Align_Annot(n, e))
        *change = TRUE;
    }// end if (e)
  }// end for (edge_iter...)

  return 0;
}

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
// FUNCTION NAME: Evaluate_Align_Inipa
// Description:
//   Evaluate actual align info using formal array header addr if 
// the actual ref point is a formal array ref point we must be careful
// about the case that formal array is adjustable array
//----------------------------------------------------------------------
BOOL
Evaluate_Align_Inipa(ST* st, ACTUAL_ACCESS_ARRAY *array, 
                     ACTUAL_ALIGN_INFO *align_info, 
                     mINT32 header_addr)
{
  UINT32 span;    //the span of certain array dim 
  INT64 offset;  //offset relative to array header addr

  // if array subscripts too messy, align cannot be analyzed.
  if (array->Too_Messy) {
    align_info->Set_Lattice_btm();
    return TRUE;
  }

  TY_IDX ty_idx = ST_type(st);
  TY *base_ty = &(Ty_Table[ty_idx]);
  if (base_ty->kind == KIND_POINTER) {
    ty_idx = base_ty->Pointed();
    base_ty = &(Ty_Table[ty_idx]);
  }

  ARB_HANDLE arb_base;
  if (base_ty->kind == KIND_ARRAY)
    arb_base = TY_arb(*base_ty);
  ARB_HANDLE arb;

  // use a stack to reverse C array dims, we need to process 
  // the array dims from low to high
  STACK<ARB_HANDLE> arb_stack(&Ipa_align_pool);
  if (!Is_Fortran()) {
    TY *elemty = base_ty;
    while (elemty->kind == KIND_ARRAY) {
        arb_base = TY_arb(*elemty);
        arb = arb_base[0];
        arb_stack.Push(arb);
        elemty = &Ty_Table[elemty->Etype()];
    }
  }
  
  // get the array element type and its size
  TY *elemty = base_ty;
  if (Is_Fortran()) {
    if (elemty->kind == KIND_ARRAY) {
      elemty = &Ty_Table[elemty->Etype()];
    }
  }
  else {
    // Although C array is of multidim form in the whirl, 
    // it is still linearized in the Ty_Table. 
    while (elemty->kind == KIND_ARRAY) {
        elemty = &Ty_Table[elemty->Etype()];
    }
  }

  //initial val of span is the array element size
  span = elemty->size;
  offset = 0;

  INT i;
  for (i = 0; i < array->Get_access_vector_count(); i++) {
    // used to print arb table entry
    
    if (i != 0) {
      // if process PU written in C, get the array dim from 
      // arb stack built above, else get the array dim from 
      // arb_base array. 
      if (!Is_Fortran()) {
        arb = arb_stack.Pop();
      }
      else {
        arb = arb_base[array->Get_access_vector_count() - i];
      }
    }
 
    // check if the array bound is const and span of relative dim mod W is 0
    if (i != 0) {
     if(ARB_const_ubnd(arb) && ARB_const_lbnd(arb) && 
	 ARB_const_stride(arb)) 
	span = span * (abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb)) + 1);
      else {
        align_info->Set_Lattice_btm();
        DevWarn("%s array is a adjustable array which dim stride is unknown", 
                 &Str_Table[st->u1.name_idx]);
	return TRUE;
      }
      // means the align info is fixed
      if (span % W == 0)
        break;
    }// end of if (i != array->Num_Vec() - 1)

    ACTUAL_ACCESS_VECTOR *vector = &ipa_access_vector[array->Get_access_vector_index() + i];
    if (vector == NULL || vector->Too_Messy) {
      align_info->Set_Lattice_btm();
      return TRUE;
    }

    // check the coeff of nonlinear symbol if coeff mod W == 0  
    INT j;
    for (j = 0; j < vector->Get_lin_symb_count(); j++) {
      ALIGN_INTSYMB_NODE *intsymb = &ipa_intsymb_node[vector->Get_lin_symb_index() + j];
      if (((intsymb->Get_Coeff() * span) % W) != 0) {
        align_info->Set_Lattice_btm();
        return TRUE;
      }
    }

    // check the coeff of linear symbol if coeff mod W == 0
    for (j = 0; j < vector->Get_non_lin_symb_count(); j++) {
      ALIGN_SUMPROD_NODE *sumprod = &ipa_sumprod_node[vector->Get_non_lin_symb_index() + j];
      if (((sumprod->Get_Coeff() * span) % W) != 0) {
        align_info->Set_Lattice_btm();
        return TRUE;
      }
    }

    // check the coeff of loop index symbol if coeff mod W == 0     
    for (j = 0; j < vector->Get_nest_depth(); j++) {
      if (((vector->Loop_Coeff(j) * span) % W) != 0) {
        align_info->Set_Lattice_btm();
        return TRUE;
      }       
    }
    offset = vector->Get_Const_Offset() * span;
    if (offset < 0) {
      // a(2i-1) vector->Const_Offset == -2, we change -2 ==> -2 + slot = 2
      offset = (offset / W + 1) * W + offset;
    }
  }// end of for (i = array->Get_access_vector_count()...)
  
  mINT32 align = (header_addr + offset) % W;
  align_info->Set_stride(W);          //wait to be deleted
  align_info->Set_offset(align); 
  align_info->Set_Lattice_mid();
  return TRUE;
}
               
//--------------------------------------------------------------------
// FUNCTION NAME: Evaluate_Actual_Align
// Description:
//   Evaluate IPA_EDGE 's ALIGN_ANNOT corresponding to a callsite
// propagate the header addr info in the caller to the actual params in
// the callsites in it
//----------------------------------------------------------------------
BOOL 
Evaluate_Actual_Align(IPA_NODE *caller, IPA_NODE *callee, IPA_EDGE *edge)
{
  BOOL change = FALSE;
  ALIGN_DYN_ARRAY *actual_annot;

  INT count = edge->Summary_Callsite()->Get_param_count ();
  if (count > callee->Num_Formals())
    count = callee->Num_Formals();

  // edge annotation initial is always put in IPA trans function
  // while node annotation initial put in Initialize_Node func
  actual_annot = edge->Align_Annot();
  if (actual_annot == NULL) {
    actual_annot = CXX_NEW(ALIGN_DYN_ARRAY(Malloc_Mem_Pool), Malloc_Mem_Pool); 
    edge->Set_Align_Annot(actual_annot);
    
    INT actual_idx = edge->Summary_Callsite()->Get_actual_index ();
    for (INT i = 0; i < count; i++, actual_idx++) {
      SUMMARY_ACTUAL *sum_actual = &(ipa_actual[actual_idx]);
      INT alg_idx = sum_actual->Get_align_index();
      if (alg_idx == -1) {
        actual_annot->Newidx();
        continue;
      }
      ACTUAL_ALIGN_INFO *align_info = &(ipa_align_info[alg_idx]);
      actual_annot->AddElement(*align_info);
    }
    change = TRUE;
  }

  ALIGN_DYN_ARRAY *caller_annot = caller->Align_Annot();
  ALIGN_DYN_ARRAY *callee_annot = callee->Align_Annot();
  INT actual_idx = edge->Summary_Callsite()->Get_actual_index ();
  // utilizing the align info in a formal param in the caller
  // to the align analysis of an actual param in a callsite if
  // the actual param is a ref point of the formal param
  for (INT i = 0; i < count; i++, actual_idx++) {
    SUMMARY_ACTUAL *sum_actual = &(ipa_actual[actual_idx]);
    INT alg_idx = sum_actual->Get_align_index();
    INT arr_idx = sum_actual->Get_array_index();
    if (alg_idx == -1) 
      continue;
    
    ACTUAL_ALIGN_INFO *align_info = &(*actual_annot)[i]; 

    // if array ref is a formal array ref point, we check if we have known 
    // the formal array header addr. If TRUE, use Evaluate_Align_Inipa to
    // evaluate it.
    if (align_info->Is_formal_array_ref() && align_info->Is_Lattice_top()) {
      INT symbol_idx = ipa_actual[actual_idx].Get_symbol_index(); 
      if ((symbol_idx != -1) && (ipa_symbol[symbol_idx].Is_formal())) {
	INT position = ipa_formal[ipa_symbol[symbol_idx].Get_findex()].
							 Get_position();
	ACTUAL_ALIGN_INFO *caller_align_info = &(*caller_annot)[position];
	ACTUAL_ALIGN_INFO *callee_align_info = &(*callee_annot)[i];
	// arr_idx != -1, the actual param is a array or pointer plus 
	// offset ref point 
	if (caller_align_info->Is_Lattice_mid() && arr_idx != -1) {
	  ACTUAL_ACCESS_ARRAY *actual_access_array = 
			      &(ipa_access_array[arr_idx]);
	  Is_True(align_info->Get_stride() == callee_align_info->Get_stride(), 
		   ("different vector register stride waited to be processed"));
	  /* caller->Set_Node_Scope();
	  SCOPE *scope = caller->Scope_Table();
	  ST_TAB *st_tab = scope->st_tab;
	  INT idx = Extract_index24(ipa_symbol[symbol_idx].St_idx()); 
	  ST *st = &(*st_tab)[idx]; */
	  // we cannot use the func Set_Node_Scope written by ourselves, it always
	  // overlay the last pu's Scope.
	  Scope_tab = caller->Scope();
	  ST *st = &St_Table[ipa_symbol[symbol_idx].St_idx()];
	  IPA_NODE_CONTEXT context_switch(caller);
	  Evaluate_Align_Inipa(st, actual_access_array, align_info, 
			       caller_align_info->Get_offset());
	  context_switch.~IPA_NODE_CONTEXT(); 
	  change = TRUE;
	}
	// arr_idx == -1, the actual param is the formal pointer plus no offset
	else if (caller_align_info->Is_Lattice_mid() && arr_idx == -1) {
	  align_info->Set_Lattice_mid();
	   // align_info->Get_offset is the offset of ldid 
	  align_info->Set_offset(caller_align_info->Get_offset() 
				 + align_info->Get_offset());
	  change = TRUE;
	}
	else if (caller_align_info->Is_Lattice_btm()) {
	  align_info->Set_Lattice_btm();
	  change = TRUE;
	}
      }
      else {
        align_info->Set_Lattice_btm();
        change = TRUE;
      }     
    }// end if (align_info...)  
  }// end for (i = 0; i < ...) 
  return change;
}

//--------------------------------------------------------------------
// FUNCTION NAME: IPA_ALIGN_DF_FLOW::Trans
// Description:
//   Transfer operation, merge sections from callee to caller
// If change in annotation, set change to TRUE else set to FALSE
//----------------------------------------------------------------------
void*
IPA_ALIGN_DF_FLOW::Trans(void*, void*, void* vertex, INT* change)
{
  IPA_NODE *node = (IPA_NODE *)vertex;

  // set up summary info arrays
  Set_summary_info (node);
  
  SUMMARY_CALLSITE* callsite = &ipa_callsite[ipa_proc->Get_callsite_index()];
  IPA_SUCC_ITER edge_iter (node);

  for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE *e = edge_iter.Current_Edge();
    if (e == NULL)
      continue;
    IPA_NODE *callee = IPA_Call_Graph->Callee (e);
    if (Evaluate_Actual_Align(node, callee, e)) {
      *change = TRUE;
    }
  }

}

//--------------------------------------------------------------------
// FUNCTION NAME: IPA_ALIGN_DF_FLOW::IPA_ALIGN_DF_FLOW
// Description:
//   construction function 
//----------------------------------------------------------------------

IPA_ALIGN_DF_FLOW::IPA_ALIGN_DF_FLOW (DF_DIRECTION ddf, MEM_POOL* m) 
  : IPA_DATA_FLOW (ddf, m) 
{
  
}

//--------------------------------------------------------------------
// FUNCTION NAME: IPA_ALIGN_DF_FLOW::PostProcessIO 
// Description:
//   at the end of the data flow problem, call the post process routine
// which is responsible for setting up the TCON array that needs to be
// written out and the constant propagation annotation
//-----------------------------------------------------------------------
void
IPA_ALIGN_DF_FLOW::PostProcessIO (void *n)
{
  if (!n) return;
  IPA_NODE* node = (IPA_NODE*) n;

  if (Get_Trace(TP_IPA, IPA_TRACE_ALIGN)) {
    Ipa_align_trace(node);
  }
}








