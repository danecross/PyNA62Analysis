// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2014-11-20
//
// ---------------------------------------------------------------

#ifndef L0TPData_H
#define L0TPData_H 1
#include "Rtypes.h"
#include "TObject.h"
#include "NA62Global.hh"

#define L0NMAXDETECTORS 7

// Subdetector Block Header (generic part)
#define O_L0EVENTLENGTH 0
#define O_L0DETECTORID 0
#define O_L0DATABLOCKFORMAT 0
#define O_L0TIMESTAMP 1

#define M_L0EVENTLENGTH 0x0000ffff
#define M_L0DETECTORID 0x00ff0000
#define M_L0DATABLOCKFORMAT 0xff000000
#define M_L0TIMESTAMP 0xffffffff

#define S_L0EVENTLENGTH 0
#define S_L0DETECTORID 16
#define S_L0DATABLOCKFORMAT 24
#define S_L0TIMESTAMP 0

// Subdetector Block Header (L0TP-specific part)
#define O_L0REFFINETIME 0
#define O_L0DATATYPE 0
#define O_L0PRIMITIVES 0
// Primitives in words 0-3
#define O_L0PREVTIMESTAMP 4
#define O_L0TRIGTYPE 5
#define O_L0PREVTRIGTYPE 5
#define O_L0TRIGFLAGS 5
#define O_L0NDELTAPRIM 6
// 32 bits reserved (word 7)
#define O_L0FINETIMES_TRIGGERSLOT 8
// Different format for N-1 and N+1 slots
#define O_L0FINETIMES_OTHERSLOTS 4

#define M_L0REFFINETIME 0x000000ff
#define M_L0DATATYPE 0x0000ff00
#define M_L0PRIMITIVE_MSB 0xffff0000
#define M_L0PRIMITIVE_LSB 0x0000ffff
#define M_L0PREVTIMESTAMP 0xffffffff
#define M_L0TRIGTYPE 0x000000ff
#define M_L0PREVTRIGTYPE 0x0000ff00
#define M_L0TRIGFLAGS 0xffff0000
#define M_L0NDELTAPRIM 0x000000ff
// 32 bits reserved (word 7)
#define M_L0FINETIME 0x000000ff

#define S_L0REFFINETIME 0
#define S_L0DATATYPE 8
#define S_L0PRIMITIVE_MSB 16
#define S_L0PRIMITIVE_LSB 0
#define S_L0PREVTIMESTAMP 0
#define S_L0TRIGTYPE 0
#define S_L0PREVTRIGTYPE 8
#define S_L0TRIGFLAGS 16
#define S_L0NDELTAPRIM 0
// 32 bits reserved (word 7)
#define S_L0FINETIME 0

class L0Primitive;

class L0TPData : public TObject {

public:

  L0TPData();
  ~L0TPData();
  void Clear(Option_t* = "");
  Bool_t   SetHeader(UInt_t *);
  UInt_t   GetEventLength()                  { return fEventLength;         }
  ULong_t  GetTimeStamp()                    { return fTimeStamp;           }
  UChar_t  GetReferenceFineTime()            { return fReferenceFineTime;   }
  void     SetReferenceFineTime(UChar_t val) { fReferenceFineTime = val;    }
  UChar_t  GetDataType()                     { return fDataType;            }
  void     SetDataType(UChar_t val)          { fDataType = val;             }
  ULong_t  GetPreviousTimeStamp()            { return fPreviousTimeStamp;   }
  UChar_t  GetTriggerType()                  { return fTriggerType;         }
  void     SetTriggerType(UChar_t val)       { fTriggerType = val;          }
  UChar_t  GetPreviousTriggerType()          { return fPreviousTriggerType; }
  UShort_t GetTriggerFlags()                 { return fTriggerFlags;        }
  void     SetTriggerFlags(UShort_t val)     { fTriggerFlags = val;         }
  UChar_t  GetNDeltaPrimitives(UInt_t iL0Detector);
  Int_t    GetNPrimitives()                  { return fPrimitives.size();   }
  Bool_t   PrimitiveExists(UInt_t iL0Slot, UInt_t iL0Detector);
  L0Primitive GetPrimitive(UInt_t iL0Slot, UInt_t iL0Detector);
  Int_t    GetPrimitiveCorrectedFineTime(UInt_t iL0Slot, UInt_t iL0Detector,Int_t BitFineTime);
  void     SetPrimitives(std::vector<L0Primitive> &input);
  void     PrintInfo();

private:

  // Subdetector Block Header (generic part)
  UInt_t  fEventLength;
  UInt_t  fDetectorID;        //!  Transient data member
  UInt_t  fDataBlockFormat;   //!  Transient data member
  ULong_t fTimeStamp;
  UInt_t  fNBlockHeaderWords; //!  Transient data member

  // Subdetector Block Header (L0TP-specific part)
  UChar_t   fReferenceFineTime;
  UChar_t   fDataType;
  ULong_t   fPreviousTimeStamp;
  UChar_t   fTriggerType;
  UChar_t   fPreviousTriggerType;
  UShort_t  fTriggerFlags;
  ULong_t   fReserved;
  std::vector<UChar_t>     fNDeltaPrimitives;
  std::vector<Int_t>       fNDeltaPrimitivesMapping; //!  Transient data member
  std::vector<L0Primitive> fPrimitives;

  ClassDef(L0TPData,1);
};

class L0Primitive : public TObject {

  public:

    L0Primitive();
    L0Primitive(const L0Primitive&);
    ~L0Primitive(){};
    void Clear(Option_t* = "");
    UInt_t  GetPrimitiveID()           { return fPrimitiveID;       }
    void    SetPrimitiveID(UInt_t val) { fPrimitiveID = val;        }
    UChar_t GetFineTime()              { return fFineTime;          }
    void    SetFineTime(UChar_t val)   { fFineTime = val;           }
    Int_t   GetCorrectedFineTime(Int_t,Int_t,Int_t);
    void    PrintInfo();

  private:

    UShort_t fPrimitiveID;
    UChar_t fFineTime;

    ClassDef(L0Primitive,1);
};

#endif
