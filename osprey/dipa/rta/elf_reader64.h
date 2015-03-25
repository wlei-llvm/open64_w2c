
/*

  Copyright (C) 2008 .  All Rights Reserved.

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

*/

//
// elf_reader.h
// Class and interface defintions for ELF object reader
// Author: Savio Tang
//
//

#ifndef _ELF_READER64_
#define _ELF_READER64_

#include <elf.h>
#include <vector>
#include "workaround.h"
#include "messg.h"

  //
  // General ELF definitions
  //
#define _BSS_SEC ".bss"
#define _COMMENT_SEC ".comment"
#define _DATA_SEC ".data"
#define _DATA1_SEC ".data1"
#define _DEBUG_SEC ".debug"
#define _STRTAB_SEC ".strtab"
#define _SYMTAB_SEC ".symtab"
#define _TEXT_SEC ".text"
#define _RODATA_SEC ".rodata"

  // There are other sections like .dynamic but I don't think we need them now
 
  //
  // Fixing Endianess
  // These functions are "filters" that fix the endianess of the data
  // Preferably, these functionsa re called at the lowest level possible
  // that is, right at the data access level, so that the code is more
  // readable, and the endianess-fixing is hidden away in the low-level access
  // routine of each particular sub-field
  //
  
#if 1 // endian handling
#define NDN(A) A
#endif /* if defined TEMP */
  
  /*
    Elf32_Addr  NDN(Elf32_Addr x);
    Elf32_Off   NDN(Elf32_Off x);
    Elf32_Half  NDN(Elf32_Half x);
    Elf32_Word  NDN(Elf32_Word x);
    Elf32_Sword NDN(Elf32_Sword x);
  */

  //
  // class declarations
  //
  class ELFHdr64;
  class ProgHdr64; 
  class ProgHdrTable64;
  class SecHdr64;
  class SecHdrTable64;
  class ELF_object64;
  

//
// Class definitions
//

class ELFHdr64
{

  friend class SecHdrTable64;
  friend class ProgHdrTable64;

 public:
  Elf64_Ehdr* _startAddr;
  unsigned char _eimag[EI_NIDENT];

 public:
  ELFHdr64(Elf64_Ehdr* address);

  // access funtions
  unsigned char* e_ident_EI_MAG(void) { return NDN(_eimag); };
  unsigned char e_ident_EI_CLASS(void) { return NDN(_startAddr->e_ident[EI_CLASS]); };
  unsigned char e_ident_EI_DATA(void) { return NDN(_startAddr->e_ident[EI_DATA]); };
  unsigned char e_ident_EI_VERSION(void) { return NDN(_startAddr->e_ident[EI_VERSION]); };
  Elf32_Half e_type(void) { return NDN(_startAddr->e_type); };
  Elf32_Half e_machine(void) { return NDN(_startAddr->e_machine); };
  Elf32_Word e_version(void) { return NDN(_startAddr->e_version); };
  Elf32_Addr e_entry(void) { return NDN(_startAddr->e_entry); };
  Elf32_Off e_phoff(void) { return NDN(_startAddr->e_phoff); };
  Elf32_Off e_shoff(void) { return NDN(_startAddr->e_shoff); };
  Elf32_Word e_flags(void) { return NDN(_startAddr->e_flags); };
  Elf32_Half e_ehsize(void) { return NDN(_startAddr->e_ehsize); };
  Elf32_Half e_phentsize(void) { return NDN(_startAddr->e_phentsize); };
  Elf32_Half e_phnum(void) { return NDN(_startAddr->e_phnum); };
  Elf32_Half e_shentsize(void) { return NDN(_startAddr->e_shentsize); };
  Elf32_Half e_shnum(void) { return NDN(_startAddr->e_shnum); };
  Elf32_Half e_shstrndx(void) { return NDN(_startAddr->e_shstrndx); };

  // print function
  void Print(FILE* stream = stdout);


}; // class ELFHdr

class ProgHdr64
{

  friend class ProgHdrTable64;

 public:
  Elf64_Phdr* _startAddr;
  ProgHdrTable64* _progHdrTable;

 public:
  ProgHdr64(unsigned char* address,ProgHdrTable64* pht);

  // access funtions
  Elf64_Word p_type(void) { return NDN(_startAddr->p_type); };
  Elf64_Off p_offset(void) { return NDN(_startAddr->p_offset); };
  Elf64_Addr p_vaddr(void) { return NDN(_startAddr->p_vaddr); };
  Elf64_Addr p_paddr(void) { return NDN(_startAddr->p_paddr); };
  Elf64_Word p_filesz(void) { return NDN(_startAddr->p_filesz); };
  Elf64_Word p_memsz(void) { return NDN(_startAddr->p_memsz); };
  Elf64_Word p_flags(void) { return NDN(_startAddr->p_flags); };
  Elf64_Word p_align(void) { return NDN(_startAddr->p_align); };

 public: // general utility functions
  const unsigned char* decode_p_type(void);
  const unsigned char* decode_p_flags(void);

  BOOL is_loadable(void) { return (p_type() == PT_LOAD); };
  BOOL is_ldExec(void) { 
    // is loadable and has exec permission
    return (is_loadable() &&
	    (p_flags() % 2));
  };
  BOOL is_ldReadWrite(void) {
    // is loadable and has read-write permission
    return (is_loadable() &&
	    ((PF_R | PF_W) == (p_flags() & (PF_R | PF_W)))
	    );
  };

  BOOL is_ldReadOnly(void) {
    // is loadable and has readonly permission
    return (is_loadable() &&
	  ((p_flags()&PF_R) && ((p_flags()&PF_W)==0) && ((p_flags()&PF_X)==0))
	    	);
  };

  void Print(FILE* stream = stdout);

}; // class ProgHdr


class ProgHdrTable64
{
  
  friend class ProgHdr64;
 public: // private data members
  unsigned char* _fileStartAddr;  // the starting address of the file
  ProgHdr64* _progHdrs; // array of program headers
  Elf64_Half _numEntries;
  unsigned int _numExecSeg; // keep number of loadable segments with exec permission
  std::vector<int> _execSegNdx;  // index of loadable exec segment (for fast retrieval)
  std::vector<int> _BssSegNdx;   // index of loadable bss segment (for fast retrieval)
  unsigned int _numBssSeg; // number of loadable Read only segments
  unsigned int _numRWSeg; // number of loadable Read-Write segments
  std::vector<int> _RWSegNdx;  // index of loadable read-write segment (for fast retrieval)
  std::vector<int> _RSegNdx;   // index of loadable read only segment (for fast retrieval)
  unsigned int _numRSeg; // number of loadable Read only segments
  int _ReginfoNdx;

 private: // private utility functions
  void _print_one(FILE* stream,ProgHdr64* phdr,unsigned int phNum);
  
 public: // Constructors
  ProgHdrTable64(unsigned char* address,ELFHdr64* eh);

 public:
  ProgHdr64* getPHdrByIndex(unsigned int phNum);
#if 0
  Elf64_Off textSegPAddr(int);
  Elf64_Off textSegVAddr(int);
  Elf64_Off textSegFAddr(int);
  Elf64_Word textSegFSize(int);
  Elf64_Word textSegMSize(int);
  Elf64_Off dataSegPAddr(int);
  Elf64_Off dataSegVAddr(int);
  Elf64_Off dataSegFAddr(int);
  Elf64_Word dataSegFSize(int);
  Elf64_Word dataSegMSize(int);
  Elf64_Off rodataSegPAddr(int);
  Elf64_Off rodataSegVAddr(int);
  Elf64_Off rodataSegFAddr(int);
  Elf64_Word rodataSegFSize(int);
  Elf64_Word rodataSegMSize(int);
  Elf64_Off bssSegVAddr(int);
  Elf64_Off bssSegFAddr(int);
  Elf64_Word bssSegMSize(int);
#endif
  ProgHdr64* begin(void) { return _progHdrs; };
  ProgHdr64* end(void) { return &_progHdrs[_numEntries]; };
  INT32 Reginfo_bias(void);
  INT32 numExecSeg(void) { return _execSegNdx.size(); }
  INT32 numRSeg(void) { return _RSegNdx.size(); }
  INT32 numRWSeg(void) { return _RWSegNdx.size(); }
  INT32 numBssSeg(void) { return _BssSegNdx.size(); }

  void Print(FILE* stream = stdout,int phNum = -1);

}; // class ProgHdrTable


class SecHdr64
{

 private:
  Elf64_Shdr* _startAddr;
  SecHdrTable64* _secHdrTable;

 public: // Constructor
  SecHdr64(unsigned char* address,SecHdrTable64* sht);

 public: // general access functions
  Elf64_Word sh_name(void)  { return NDN(_startAddr->sh_name); };
  Elf64_Word sh_type(void)  { return NDN(_startAddr->sh_type); };
  Elf64_Word sh_flags(void) { return NDN(_startAddr->sh_flags); };
  Elf64_Addr sh_addr(void)  { return NDN(_startAddr->sh_addr); };
  Elf64_Off sh_offset(void) { return NDN(_startAddr->sh_offset); };
  Elf64_Word sh_size(void)  { return NDN(_startAddr->sh_size); };
  Elf64_Word sh_link(void)  { return NDN(_startAddr->sh_link); };
  Elf64_Word sh_info(void)  { return NDN(_startAddr->sh_info); };
  Elf64_Word sh_addralign(void) { return NDN(_startAddr->sh_addralign); };
  Elf64_Word sh_entsize(void) { return NDN(_startAddr->sh_entsize); };

 public: // general utility functions
  const unsigned char* decode_sh_type(void);
  unsigned char* secName(void);

}; // class SecHdr

class SecHdrTable64
{
  
  friend class SecHdr64;

 public: // private data members
  unsigned char* _fileStartAddr;  // the starting address of the file
  SecHdr64* _secHdrs; // array of section headers
  Elf64_Half _numEntries;
  Elf64_Half _shstrndx;

 private: // private utility functions
  void _print_one(FILE* stream,SecHdr64* shdr,unsigned int secNum);
  
 public: // Constructors
  SecHdrTable64(unsigned char* address,ELFHdr64* eh);

 public:
  SecHdr64* SecTab(void) { return _secHdrs; }
  int     SecNum(void)      { return _numEntries; }
  SecHdr64* getSHdrByName(char* secName);
  SecHdr64* getSHdrByIndex(unsigned int secNum);

  SecHdr64* begin(void) { return _secHdrs; };
  SecHdr64* end(void){ return &_secHdrs[_numEntries]; };

  void Print(FILE* stream = stdout,int secNum = -1);

}; // class SecHdrTable

#if 0
class ElfSymbol
{

  friend class ElfSymtab;

 public:
  Elf32_Sym* _startAddr;
  ElfSymtab* _symtab;

 public: // Constructor
  ElfSymbol(unsigned char* address, ElfSymtab* st);

 public: // general access functions
  Elf32_Word st_name(void) { return NDN(_startAddr->st_name); };
  Elf32_Addr st_value(void) { return NDN(_startAddr->st_value); };
  Elf32_Half st_shndx(void) { return NDN(_startAddr->st_shndx); };
  Elf32_Word st_size(void) { return NDN(_startAddr->st_size); };
  unsigned char st_info(void) { return NDN(_startAddr->st_info); };
  unsigned char st_other(void) { return NDN(_startAddr->st_other); };
  bool st_is_code(void) { return (ELF32_ST_TYPE(NDN(_startAddr->st_info)) == STT_FUNC); }
  bool st_is_data(void) { return ((ELF32_ST_TYPE(NDN(_startAddr->st_info)) == STT_OBJECT) || (ELF32_ST_TYPE(NDN(_startAddr->st_info)) == STT_TLS)) ; }
  bool st_is_bss(void) { return (ELF32_ST_TYPE(NDN(_startAddr->st_info)) == STT_COMMON); }
  bool st_is_weak(void) { return (ELF32_ST_BIND(_startAddr->st_info)) == STB_WEAK; }

 public: // general utility functions
  char* symName(void);
  const unsigned char* decode_st_type(void);
  const unsigned char* decode_st_bind(void);
  const unsigned char* decode_sec_index(void);
  char* symNameIsSpecial(ElfSymtab*);
  bool  symNameIsSpecial(void) { return this->st_shndx() == 0; }

}; // class ElfSymbol

class ElfSymtab
{

  friend class ElfSymbol;

 public:
  unsigned char* _fileStartAddr;  // the starting address of the file
  SecHdrTable* _shdrs; // a handle of the section headers
  ElfSymbol* _symbols; // array of elf symbols
  Elf32_Half _numEntries;
  Elf32_Half _sizeOfEntry;

 private: // private utility functions
  void _print_one(FILE* stream, ElfSymbol* sym, unsigned int symNum);

 public:
  ElfSymtab(unsigned char* address, SecHdrTable* sh);

  ElfSymbol* getSymByName(char* symName);
  ElfSymbol* getSymByIndex(unsigned int symNum);
  ElfSymbol* begin(void) { return _symbols; };
  ElfSymbol* end(void) { return &_symbols[_numEntries]; }
  ElfSymbol* Entry(int i) { return &_symbols[i]; }
  int Size(void) { return (int)_numEntries; }
  void Print(FILE* stream = stdout,int symNum = -1);
  
#ifdef _SL1_VERSION_CHECK  
  void setSymVersionNumber(unsigned version);
#endif

  // print function

}; // class ElfSymtab
#endif // 0

#define FILE_NAME_MAX 256
class ELF_object64
{

 public:
  ELFHdr64*        _elfHeader;
  SecHdrTable64*   _secHdrs;
  //  ElfSymtab*     _symTable;
  ProgHdrTable64*  _progHdrs;
  char  _elfFileName[ FILE_NAME_MAX ];

 public:  // Constructors
  ELF_object64(char* elf_file);

 public:  // general print functions
  char *getFileName( void ) { return _elfFileName;}
  SecHdrTable64* SecHdrTab(void) { return _secHdrs; }
  void PrintEHdr(FILE* stream = stdout);
  void PrintSHdr(FILE* stream = stdout);
  void PrintPHdr(FILE* stream = stdout);
  void PrintSymtab(FILE* stream = stdout);

 public:
  Elf64_Word textSecFSize(void) {
    return (_secHdrs->getSHdrByName(_TEXT_SEC)->sh_size());
  };

  Elf64_Addr textSecVAddr(void) {
    return _secHdrs->getSHdrByName(_TEXT_SEC)->sh_addr();
  };

  Elf64_Addr EntryPt(void) { return _elfHeader->e_entry(); }
#if 0
  Elf64_Off textSecFAddr(void) {
    return (Elf64_Off)(_secHdrs->getSHdrByName(_TEXT_SEC)->sh_offset()
      + _elfHeader);
  };
  
  ElfSymtab* Table(void) { return _symTable; }
  Elf32_Off textSegPAddr(int);
  Elf32_Off textSegVAddr(int);
  Elf32_Off textSegFAddr(int);
  Elf32_Word textSegFSize(int);
  Elf32_Word textSegMSize(int);
  Elf32_Off dataSegPAddr(int);
  Elf32_Off dataSegVAddr(int);
  Elf32_Off dataSegFAddr(int);
  Elf32_Word dataSegFSize(int);
  Elf32_Word dataSegMSize(int);
  Elf32_Off rodataSegPAddr(int);
  Elf32_Off rodataSegVAddr(int);
  Elf32_Off rodataSegFAddr(int);
  Elf32_Word rodataSegFSize(int);
  Elf32_Word rodataSegMSize(int);
  Elf32_Off bssSegVAddr(int);
  Elf32_Off bssSegFAddr(int);
  Elf32_Word bssSegMSize(int);
#endif
  ProgHdrTable64*  progHdrs(void) { return _progHdrs; }
#ifdef _SL1_VERSION_CHECK  
  void setSymVersionNumber(void);
#endif

}; // class ELF_object


#endif // _ELF_READER64_

