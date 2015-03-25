//-----------------------------------------------------------------------------
// MODULE:     IPA Alignment analysis module
// File Name:  ipa_align.h 
// Author:     miwei
// Abstract:   dataflow analysis part in IPA align phase 
// Date:       2005-9-16
// Copy Right: 2004-2007, Institute of Computing Technology, Chinese Academy of 
//             Sciences. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef ipa_align_analysis_INCLUDED
#define ipa_align_analysis_INCLUDED

#include "ipa_df.h"
#include "ipa_align.h"


typedef DYN_ARRAY<ACTUAL_ALIGN_INFO> ALIGN_DYN_ARRAY;
extern MEM_POOL Ipa_align_pool;

//-------------------------------------------------------------------
// Class Name: IPA_ALIGN_DF_FLOW 
// Description:
//   dataflow class of ipa align analysis
//-------------------------------------------------------------------
class IPA_ALIGN_DF_FLOW : public IPA_DATA_FLOW
{
protected:
  virtual void* Meet(void* in, void* vertex, INT *change);
  virtual void* Trans(void* in, void* out, void* vertex, INT *change);

public:
  IPA_ALIGN_DF_FLOW (DF_DIRECTION ddf, MEM_POOL* m);
  
  virtual void InitializeNode(void *n);
//virtual void Print_entry(FILE *fp, void* out, void* n);
  virtual void PostProcessIO(void *node);

};

#endif
