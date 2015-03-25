//-----------------------------------------------------------------------------
// MODULE:     IPA Alignment analysis module
// File Name:  ipa_align.h
// Author:     miwei
// Abstract:   Alignment summary data structure in IPA phase 
// Date:       2005-9-16
// Copy Right: 2004-2007, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef ipa_align_INCLUDED
#define ipa_align_INCLUDED

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#include <stdint.h>
//#undef  __STDC_LIMIT_MACROS
#else
#include <stdint.h>
#endif
#include "ipl_summary.h"
 
#define MAX_SYM_NUM 50
#define MAX_LOOP_NEST 20
// W is the width of a vector register
#define W 16

// simply dismiss the symbol items of sumprod node
// because they have no use in ipa align analysis
//-------------------------------------------------------------------
// Class Name: ALIGN_SUMPROD_NODE 
// Description:
//   correspond to SUMPROD_NODE
//   simply dismiss the symbol items of sumprod node
// because they have no use in ipa align analysis
//-------------------------------------------------------------------
class ALIGN_SUMPROD_NODE 
{
private:
  mINT32 _symbol_index[MAX_SYM_NUM];  //summary_symbol index array
  mINT16 _symbol_count;               //how many symbols in Prod_List
  INT32 Coeff;  // An integer that multiplies the symbols in Prod_List
 
public:
  ALIGN_SUMPROD_NODE() {
    Init();
  }

  void Init() {
    INT i;
    for (i = 0; i < MAX_SYM_NUM; i++) {
      _symbol_index[i] = -1;
    }
    _symbol_count = 0;
    Coeff = 0;
  }

  mINT32 Get_symbol_index(mINT32 index) { 
    Is_True(index < MAX_SYM_NUM, ("out of bounds of symbol num <ipa_align.h>"));
    return _symbol_index[index]; 
  }

  mINT32 Set_symbol_index(mINT32 index, mINT32 val) {
    Is_True(index < MAX_SYM_NUM, ("out of bounds of symbol num <ipa_align.h>"));
    _symbol_index[index] = val;
  }
  
  INT32 Get_Coeff() { return Coeff; }
  void Set_Coeff(INT32 coeff) { Coeff = coeff; }
};

//-------------------------------------------------------------------
// Class Name: ALIGN_INTSYMB_NODE
// Description:
//   correspond to INTSYMB_NODE
//   simply dismiss the symbol items of intsymb node
// because they have no use in ipa align analysis
//-------------------------------------------------------------------
class ALIGN_INTSYMB_NODE {
private:
  INT32 _symbol_index;          // summary_symbol index
  INT32 Coeff;                    // symbol coefficient

public:
  ALIGN_INTSYMB_NODE() {
    Init();
  }

  void Init() {
    _symbol_index = -1;
    Coeff = 0;
  }

  INT32 Get_symbol_index() { return _symbol_index; }
  INT32 Get_Coeff() { return Coeff; }

  void Set_symbol_index(INT32 symbol_index) { _symbol_index = symbol_index; }
  void Set_Coeff(INT32 coeff) { Coeff = coeff; }
};

//-------------------------------------------------------------------
// Class Name: ACTUAL_ACCESS_VECTOR
// Description:
//   corresponding to ACCESS_VECTOR
//-------------------------------------------------------------------
class ACTUAL_ACCESS_VECTOR {
private:
  mINT32 _lcoeff[MAX_LOOP_NEST];  // linear coefficients of loop variables
  mINT16 _nest_depth;
  mINT16 _non_const_loops;
  mINT32 _lin_symb_index;        // linear symbolic terms index
  mINT16 _lin_symb_count;
  mINT32 _non_lin_symb_index;    // non-linear symbolic terms index
  mINT16 _non_lin_symb_count;
  INT32 Const_Offset;

public:
  BOOL Too_Messy;                 // is this vector too messy
  
  ACTUAL_ACCESS_VECTOR() {
    Init();
  }
  
  void Init() {
    INT i;
    for (i = 0; i < MAX_LOOP_NEST; i++) {
      _lcoeff[i] = -1;
    }
    _nest_depth = -1;
    _non_const_loops = -1;
    _lin_symb_index = -1;
    _lin_symb_count = 0;
    _non_lin_symb_index = -1;
    _non_lin_symb_count = 0;
    Const_Offset = 0;
    Too_Messy = FALSE;
  }

  INT32 Loop_Coeff(INT32 i) const {
    Is_True(i < MAX_LOOP_NEST, ("out of bounds of loop nest <ipa_align.h>"));
    if (_lcoeff && (i < _nest_depth)) {
      return(_lcoeff[i]);
    } else {
      return(0);
    }
  }
  
  void Set_Loop_Coeff(INT32 i, mINT32 val) {
    Is_True(i < MAX_LOOP_NEST, ("out of bounds of loop nest <ipa_align.h>"));
    if (_lcoeff && (i < _nest_depth)) {
      _lcoeff[i] = val;
    }
  }

  mINT16 Get_nest_depth() { return _nest_depth; }
  void Set_nest_depth(mINT16 nest_depth) { _nest_depth = nest_depth; }

  mINT16 Get_non_const_loops() { return _non_const_loops; }
  void Set_non_const_loops(mINT16 non_const_loops) { _non_const_loops = non_const_loops; }

  mINT32 Get_lin_symb_index() { return _lin_symb_index; }
  void Set_lin_symb_index(mINT32 lin_index) { _lin_symb_index = lin_index; }

  mINT32 Get_lin_symb_count() { return _lin_symb_count; }
  void Set_lin_symb_count(mINT32 lin_count) { _lin_symb_count = lin_count; }

  mINT32 Get_non_lin_symb_index() { return _non_lin_symb_index; }
  void Set_non_lin_symb_index(mINT32 non_lin_index) { _non_lin_symb_index = non_lin_index; }

  mINT32 Get_non_lin_symb_count() { return _non_lin_symb_count; }
  void Set_non_lin_symb_count(mINT32 non_lin_count) { _non_lin_symb_count = non_lin_count; }

  INT32 Get_Const_Offset() { return Const_Offset; } 
  void Set_Const_Offset(INT32 const_offset) { Const_Offset = const_offset; }  
};

//-------------------------------------------------------------------
// Class Name: ACTUAL_ACCESS_ARRAY
// Description:
//   corresponding to ACCESS_ARRAY
//-------------------------------------------------------------------
class ACTUAL_ACCESS_ARRAY 
{
private:
  mINT32 _access_vector_index;
  mINT16 _access_vector_count;   // here count = size 
  BOOL    _cvt_from_pointer;         // if an access array is converted from a 
                                                      // offset of a pointer

public:
  BOOL Too_Messy;

  ACTUAL_ACCESS_ARRAY() {
    Init(); 
  }

  void Init() {
    _access_vector_index = -1;
    _access_vector_count = 0;
    _cvt_from_pointer = FALSE;
    Too_Messy = FALSE;
  }
 
  mINT32 Get_access_vector_index() {
    return _access_vector_index;
  }  
  mINT32 Get_access_vector_count() {
    return _access_vector_count;
  }
  BOOL Get_cvt_from_pointer() {
    return _cvt_from_pointer;
  }
  
  void Set_access_vector_index(mINT32 access_vector_index) {
    _access_vector_index = access_vector_index;
  }  
  void Set_access_vector_count(mINT32 access_vector_count) {
    _access_vector_count = access_vector_count;
  }  
  void Set_cvt_from_pointer(BOOL cvt_from_pointer) {
    _cvt_from_pointer = cvt_from_pointer;
  }
  
  void Print_file(FILE *f);
};

//-------------------------------------------------------------------
// Class Name: ACTUAL_ALIGN_INFO
// Description:
//   record the align info of a actual param
//-------------------------------------------------------------------
enum {
  IS_FORMAL_ARRAY_REF = 0x00000001,
  IS_FORMAL_REMOVED = 0x00000002              // the formal is removed in constprop
};

class ACTUAL_ALIGN_INFO
{
private:
  typedef enum {
    LATTICE_TOP,                  // Align info can be expressed in a Lattice ,
    LATTICE_MID,                  // Lattice top means align info is certained,        
    LATTICE_BTM                   // mid means have more than one choice, btm 
  } LATTICE_VAL;                  // means it is unknown.
  LATTICE_VAL _lattice_pos;                  
  
  mINT32 _stride;                 // stride is the width of vector register
  mINT32 _offset;                 // offset = addr mod stride
  UINT32 _flags;                    // flags

public:
  void Init ()
  {
    _lattice_pos = LATTICE_TOP;
    _stride = W;                  // we can add a Get_stride_according_type 
                                  // function and use it here
    _offset = -1;
    _flags = 0;
  }
  mINT32 Get_stride() { return _stride; }
  mINT32 Get_offset() { return _offset; }
  LATTICE_VAL Get_Lattice() { return _lattice_pos; }
  BOOL Is_Lattice_top() { return _lattice_pos == LATTICE_TOP; }
  BOOL Is_Lattice_mid() { return _lattice_pos == LATTICE_MID; }
  BOOL Is_Lattice_btm() { return _lattice_pos == LATTICE_BTM; }
  BOOL Is_formal_array_ref() { return _flags & IS_FORMAL_ARRAY_REF; }
  BOOL Is_formal_removed() { return _flags & IS_FORMAL_REMOVED; }

  void Set_stride(mINT32 stride) { _stride = stride; }
  void Set_offset(mINT32 offset) { _offset = offset; }
  void Set_Lattice(LATTICE_VAL val) { _lattice_pos = val; }
  void Set_Lattice_top() { _lattice_pos = LATTICE_TOP; }
  void Set_Lattice_mid() { _lattice_pos = LATTICE_MID; }
  void Set_Lattice_btm() { _lattice_pos = LATTICE_BTM; }
  void Set_is_formal_array_ref() { _flags |= IS_FORMAL_ARRAY_REF; }
  void Reset_is_formal_array_ref() { _flags &= ~IS_FORMAL_ARRAY_REF; }
  void Set_is_formal_removed() { _flags |= IS_FORMAL_REMOVED; }
  void Reset_is_formal_removed() { _flags &= ~IS_FORMAL_REMOVED; }
  	
  void Print_file(FILE *f); 
};  

typedef DYN_ARRAY<ACTUAL_ALIGN_INFO> ACTUAL_ALIGN_INFO_ARRAY;
typedef DYN_ARRAY<ACTUAL_ACCESS_ARRAY> ACTUAL_ACCESS_ARRAY_ARRAY;
typedef DYN_ARRAY<ACTUAL_ACCESS_VECTOR> ACTUAL_ACCESS_VECTOR_ARRAY;
typedef DYN_ARRAY<ALIGN_INTSYMB_NODE> ALIGN_INTSYMB_NODE_ARRAY;
typedef DYN_ARRAY<ALIGN_SUMPROD_NODE> ALIGN_SUMPROD_NODE_ARRAY;

//-------------------------------------------------------------------
// Class Name: ARRAY_ALIGN_SUMMARY
// Description:
//   array ref point align info summary in the whole pu. similar
// with ARRAY_SUMMARY in the IPA_ARRAY_DF_FLOW analysis
//-------------------------------------------------------------------
class ARRAY_ALIGN_SUMMARY
{
private:
  MEM_POOL _align_pool;
  // actual array ref point align info array
  ACTUAL_ALIGN_INFO_ARRAY     *actual_align_info_array;
  // actual array ref point subscription expression
  ACTUAL_ACCESS_ARRAY_ARRAY   *actual_access_array_array;
  ACTUAL_ACCESS_VECTOR_ARRAY  *actual_access_vector_array;
  ALIGN_INTSYMB_NODE_ARRAY    *align_intsymb_node_array;
  ALIGN_SUMPROD_NODE_ARRAY    *align_sumprod_node_array;
  INT offset_align_info, offset_access_array, offset_access_vector;
  INT offset_intsymb_node, offset_sumprod_node;

public:
  void Init()
  {
    MEM_POOL_Initialize(&_align_pool, "align summary pool", 0);
    MEM_POOL_Push(&_align_pool);
    actual_align_info_array = CXX_NEW(ACTUAL_ALIGN_INFO_ARRAY(&_align_pool), &_align_pool);
    actual_access_array_array = CXX_NEW(ACTUAL_ACCESS_ARRAY_ARRAY(&_align_pool), &_align_pool);
    actual_access_vector_array = CXX_NEW(ACTUAL_ACCESS_VECTOR_ARRAY(&_align_pool), &_align_pool);
    align_intsymb_node_array = CXX_NEW(ALIGN_INTSYMB_NODE_ARRAY(&_align_pool), &_align_pool);
    align_sumprod_node_array = CXX_NEW(ALIGN_SUMPROD_NODE_ARRAY(&_align_pool), &_align_pool);
  }

  void Finalize()
  {
    MEM_POOL_Pop(&_align_pool);
    MEM_POOL_Delete(&_align_pool);
  }

  ACTUAL_ALIGN_INFO_ARRAY * Get_actual_align_info_array()
  {
    return actual_align_info_array;
  }
  
  ACTUAL_ACCESS_ARRAY_ARRAY * Get_actual_access_array_array()
  {
    return actual_access_array_array;
  }

  ACTUAL_ACCESS_VECTOR_ARRAY * Get_actual_access_vector_array()
  {
    return actual_access_vector_array;
  }

  ALIGN_INTSYMB_NODE_ARRAY * Get_align_intsymb_node_array()
  {
    return align_intsymb_node_array;
  }

  ALIGN_SUMPROD_NODE_ARRAY * Get_align_sumprod_node_array()
  {
    return align_sumprod_node_array;
  }

  INT Get_align_info_count() { return actual_align_info_array->Lastidx() + 1; }
  INT Get_access_array_count() { return actual_access_array_array->Lastidx() + 1; }
  INT Get_access_vector_count() { return actual_access_vector_array->Lastidx() + 1; }
  INT Get_intsymb_node_count() { return align_intsymb_node_array->Lastidx() + 1; }
  INT Get_sumprod_node_count() { return align_sumprod_node_array->Lastidx() + 1; }

  void Write_summary(struct output_file *f, INT cur_sec_disp);
  void Update_array_align_header(SUMMARY_FILE_HEADER *header_addr); 

  void Print_align_info_array(FILE *f, INT size, ACTUAL_ALIGN_INFO *align_info);
  void Print_access_array_array(FILE *f, INT size, ACTUAL_ACCESS_ARRAY *access_array);
  
  void Trace(FILE *f);
 
};//end of class ARRAY_ALIGN_SUMMARY

#endif








