#ifndef normalize_INCLUDED
#define normalize_INCLUDED

typedef mINT64    STAB_OFFSET;
#define TT_NORMALIZE 0x1

extern WN_MAP W2CF_Treety_Map; 

extern void 
WN2C_Set_Treety(const WN *wn, TY_IDX ty);

extern TY_IDX 
WN2C_Get_Treety(const WN *wn);

BOOL
WN2C_unparser_compatible_types(TY_IDX wnty, TY_IDX treety, INT depth = 0);

extern void 
Normalize_Rec(WN *wn, WN *func);

extern void 
Normalize_Iload(WN *wn, TY_IDX treety);

extern TY_IDX
W2C_Make_Array_Type (TY_IDX elemty, INT32 ndim, INT64 len);

TY_IDX
Get_Array_Tree_Ty(WN *wn, TY_IDX elemty, BOOL needptr);

#endif
