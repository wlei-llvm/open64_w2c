//-------------------------------------------------------------------
// MODULE:     IPA Alignment analysis module
// File Name:  ipa_align.cxx 
// Author:     miwei
// Abstract:   Alignment analysis of ref points in IPA phase 
// Date:       2005-9-16
// Copy Right: 2004-2006, Institute of Computing Technology, Chinese 
//             Academy of Sciences. All rights reserved.
//-------------------------------------------------------------------

#include "ipa_align.h"
#include "ipl_align.h"
#include "ipl_summary.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"

//-------------------------------------------------------------------
// FUNCTION NAME: ACTUAL_ALIGN_INFO::Print_file 
// Description:
//   write ARRAY_ALIGN_SUMMARY info into .o file for ipa use
//-------------------------------------------------------------------
void ACTUAL_ALIGN_INFO::Print_file(FILE *f)
{
  switch (_lattice_pos) {
    case LATTICE_BTM: 
      fprintf(f, " lattice pos: LATTICE_BTM \n");
      break;

    case LATTICE_MID:
      fprintf(f, " lattice pos: LATTICE_MID ");
      fprintf(f, " stride: %d offset: %d \n", _stride, _offset);
      break;

    case LATTICE_TOP:
      fprintf(f, " lattice pos: LATTICE_TOP \n");
      break; 
  }//end of switch 
}

//-------------------------------------------------------------------
// FUNCTION NAME: ACTUAL_ACCESS_ARRAY::Print_file
// Description:
//   write ARRAY_ALIGN_SUMMARY info into .o file for ipa use
//-------------------------------------------------------------------
void ACTUAL_ACCESS_ARRAY::Print_file(FILE *fp)
{
  INT i;
  ACTUAL_ACCESS_VECTOR_ARRAY *vect_array = 
            Array_Align_Summary.Get_actual_access_vector_array();
  ALIGN_INTSYMB_NODE_ARRAY *int_array =
            Array_Align_Summary.Get_align_intsymb_node_array();
  ALIGN_SUMPROD_NODE_ARRAY *sum_array = 
            Array_Align_Summary.Get_align_sumprod_node_array();

  //output every access vector
  for (i = 0; i < Get_access_vector_count(); i++) {
    INT index = Get_access_vector_index();
    ACTUAL_ACCESS_VECTOR *vector = &(*vect_array)[i + index];
    fprintf(fp, "\n ACTUAL_ACCESS_VECTOR[%d] ", i);
    
    // Const offset
    fprintf(fp, "\n Const Offset:  %d ", vector->Get_Const_Offset());    

    // every loop coeff in a access vector
    INT j;
    fprintf(fp, "\n Loop Coeff: ");
    for (j=0; j < vector->Get_nest_depth(); j++) {
      fprintf(fp, " %d--i%d ", vector->Loop_Coeff(j), j);
    }

    // every intsymb node in a access vector
    index = vector->Get_lin_symb_index();
    fprintf(fp, "\n Intsymb_Node: ");    
    for (j=0; j < vector->Get_lin_symb_count(); j++) {     
      fprintf(fp, " %d--v%d ", (*int_array)[j + index].Get_Coeff(), j);
    }

    // every sumprod node in a access vector
    index = vector->Get_non_lin_symb_index();
    fprintf(fp, "\n Sumprod_Node: ");
    for (j=0; j < vector->Get_non_lin_symb_count(); j++) {
      fprintf(fp, " %d--(v*v)%d ", (*sum_array)[j + index].Get_Coeff(), j);
    }

  }//end of for (i = 0; i...) 
}

//-------------------------------------------------------------------
// FUNCTION NAME: ARRAY_ALIGN_SUMMARY::Write_summary
// Description:
//   write ARRAY_ALIGN_SUMMARY info into .o file for ipa use, 
// f is the handle of the output file
//-------------------------------------------------------------------
void ARRAY_ALIGN_SUMMARY::Write_summary(struct output_file *f, 
                          INT cur_sec_disp)
{
  INT size;
  Output_File *fl = (Output_File *)f;

  ACTUAL_ALIGN_INFO_ARRAY *align_info_array = Get_actual_align_info_array();
  ACTUAL_ACCESS_ARRAY_ARRAY *access_array_array = Get_actual_access_array_array();
  ACTUAL_ACCESS_VECTOR_ARRAY *access_vector_array = Get_actual_access_vector_array();
  ALIGN_INTSYMB_NODE_ARRAY *intsymb_node_array = Get_align_intsymb_node_array();
  ALIGN_SUMPROD_NODE_ARRAY *sumprod_node_array = Get_align_sumprod_node_array();

  if (Get_align_info_count() != 0) {
    size = (Get_align_info_count()) * sizeof(ACTUAL_ALIGN_INFO);
    offset_align_info = (INT)ir_b_save_buf(&(*align_info_array)[0], size,
                             sizeof(INT64), 0, fl);
    offset_align_info -= cur_sec_disp;
  }

  if (Get_access_array_count() != 0) {
    size = (Get_access_array_count()) * sizeof(ACTUAL_ACCESS_ARRAY);
    offset_access_array = (INT)ir_b_save_buf(&(*access_array_array)[0], size,
                             sizeof(INT64), 0, fl);
    offset_access_array -= cur_sec_disp;
  }
  if (Get_access_vector_count() != 0) {
    size = (Get_access_vector_count()) * sizeof(ACTUAL_ACCESS_VECTOR);
    offset_access_vector = (INT)ir_b_save_buf(&(*access_vector_array)[0], size,
                             sizeof(INT64), 0, fl);
    offset_access_vector -= cur_sec_disp;
  }
  if (Get_intsymb_node_count() != 0) {
    size = (Get_intsymb_node_count()) * sizeof(ALIGN_INTSYMB_NODE);
    offset_intsymb_node = (INT)ir_b_save_buf(&(*intsymb_node_array)[0], size,
                             sizeof(INT64), 0, fl);
    offset_intsymb_node -= cur_sec_disp;
  }
  if (Get_sumprod_node_count() != 0) {
    size = (Get_sumprod_node_count()) * sizeof(ALIGN_SUMPROD_NODE);
    offset_sumprod_node = (INT)ir_b_save_buf(&(*sumprod_node_array)[0], size,
                             sizeof(INT64), 0, fl);
    offset_sumprod_node -= cur_sec_disp;
  }
}

//-------------------------------------------------------------------
// FUNCTION NAME:ARRAY_ALIGN_SUMMARY::Update_array_align_header
// Description:
//   Update array_align_summary part in SUMMARY_FILE_HEADER
//-------------------------------------------------------------------
void ARRAY_ALIGN_SUMMARY::Update_array_align_header
                                                    (SUMMARY_FILE_HEADER *header_addr) 
{
  header_addr->Set_align_info_offset(offset_align_info);
  header_addr->Set_access_array_offset(offset_access_array);
  header_addr->Set_access_vector_offset(offset_access_vector);
  header_addr->Set_intsymb_node_offset(offset_intsymb_node);
  header_addr->Set_sumprod_node_offset(offset_sumprod_node);

  header_addr->Set_align_info_entry_size(sizeof(ACTUAL_ALIGN_INFO));
  header_addr->Set_access_array_entry_size(sizeof(ACTUAL_ACCESS_ARRAY));
  header_addr->Set_access_vector_entry_size(sizeof(ACTUAL_ACCESS_VECTOR));
  header_addr->Set_intsymb_node_entry_size(sizeof(ALIGN_INTSYMB_NODE));
  header_addr->Set_sumprod_node_entry_size(sizeof(ALIGN_SUMPROD_NODE));

  header_addr->Set_align_info_size(Get_align_info_count());
  header_addr->Set_access_array_size(Get_access_array_count());
  header_addr->Set_access_vector_size(Get_access_vector_count());
  header_addr->Set_intsymb_node_size(Get_intsymb_node_count());
  header_addr->Set_sumprod_node_size(Get_sumprod_node_count());

}

//-------------------------------------------------------------------
// FUNCTION NAME: ARRAY_ALIGN_SUMMARY::Print_align_info_array
// Description:
//   print actual_align_info array 
//-------------------------------------------------------------------
void ARRAY_ALIGN_SUMMARY::Print_align_info_array
                                          (FILE *f, INT size, ACTUAL_ALIGN_INFO *align_info)
{
  fprintf ( f, "%sStart actual_align_info array\n%s", SBar, SBar );
  for (INT i = 0; i < size; ++i)
  {
    fprintf(f, "ACTUAL_ALIGN_INFO[%d]: ", i);
    align_info[i].Print_file (f);
  }
  fprintf ( f, "%sEnd actual_align_info array\n%s", SBar, SBar );
}

//-------------------------------------------------------------------
// FUNCTION NAME: ARRAY_ALIGN_SUMMARY::Print_access_array_array
// Description:
//   Print actual_access_array array
//-------------------------------------------------------------------
void ARRAY_ALIGN_SUMMARY::Print_access_array_array
                (FILE *f, INT size, ACTUAL_ACCESS_ARRAY *access_array)
{
  fprintf ( f, "%sStart actual_access_array array\n%s", SBar, SBar );
  for (INT i = 0; i < size; ++i)
  {
    fprintf(f, "ACTUAL_ACCESS_ARRAY[%d]: ", i);
    access_array[i].Print_file (f);
  }
  fprintf ( f, "\n%sEnd actual_access_array array\n%s", SBar, SBar );
}

//-------------------------------------------------------------------
// FUNCTION NAME: ARRAY_ALIGN_SUMMARY::Trace
// Description:
//   trace func, output align info and access array info passed 
// from ipl to ipa
//-------------------------------------------------------------------
void ARRAY_ALIGN_SUMMARY::Trace(FILE *f)
{
  ACTUAL_ALIGN_INFO_ARRAY *align_info_array = 
                          Get_actual_align_info_array();
  ACTUAL_ACCESS_ARRAY_ARRAY *access_array_array = 
                            Get_actual_access_array_array();
  if (Get_align_info_count() > 0)
    Print_align_info_array(f, Get_align_info_count(), 
                           &(*align_info_array)[0]);
  if (Get_access_array_count() > 0)
    Print_access_array_array(f, Get_access_array_count(), 
                             &(*access_array_array)[0]);
}
