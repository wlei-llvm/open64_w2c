//-----------------------------------------------------------------------------
// MODULE:     IPA Alignment analysis module
// File Name:  ipa_align.h 
// Author:     miwei
// Abstract:   dataflow analysis part in IPA align phase 
// Date:       2005-9-16
// Copy Right: 2004-2007, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#define __STDC_LIMIT_MACROS
#include <stdint.h>
#include <elf.h>
#include <alloca.h>

/*#include "defs.h"
#include "mempool.h"
#include "tracing.h"
#include "cxx_template.h"

#include "ipc_symtab_merge.h"*/		// IPC_GLOBAL_IDX_MAP
#include "ipl_summary.h"		// summary info data structures
//#include "ipa_option.h"                 // IPA trace flags
#include "ipa_cg.h"			// call graph
#include "ipa_summary.h"		// summary info access routines


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


MEM_POOL Ipa_align_pool;		// mem pool for ip align analysis

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
