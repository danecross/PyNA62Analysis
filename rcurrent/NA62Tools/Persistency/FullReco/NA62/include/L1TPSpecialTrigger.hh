// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L1TPSpecialTrigger_H
#define L1TPSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

#define O_L1SPTRGBLOCKLENGTH 0
#define O_L1SPTRGL1PCID 0
#define O_L1SPTRGDETECTORID 0
#define O_L1SPTRGTIMESTAMP 1
#define O_L1SPTRGDATAFORMAT 2
#define O_L1SPTRGTIMEOUTFLAG 2
// Reserved (word 3)
#define O_L1SPTRGNL1INPUTEVTS 4
#define O_L1SPTRGNL1SPECIALEVTS 5
#define O_L1SPTRGNL1CONTROLEVTS 6
#define O_L1SPTRGNL1PERIODICEVTS 7
#define O_L1SPTRGNL1PHYSICSEVTS 8
#define O_L1SPTRGNL1PHYSICSEVTSMM 9
#define O_L1SPTRGNL1DATAREQUESTS 10
#define O_L1SPTRGNL1OUTPUTEVTS 11
#define O_L1SPTRGNL1ACCEPTEDEVTS 12
#define O_L1SPTRGNL1TIMEOUTEVTS 13
#define O_L1SPTRGNL1ALLDISABLEDEVTS 14
#define O_L1SPTRGNL1BYPASSEDEVTS 15
#define O_L1SPTRGNL1FLAGALGOEVTS 16
#define O_L1SPTRGNL1AUTOPASSEVTS 17
#define O_L1SPTRGL1MASKSINFO 18

#define M_L1SPTRGBLOCKLENGTH 0x0000ffff
#define M_L1SPTRGL1PCID 0x00ff0000
#define M_L1SPTRGDETECTORID 0xff000000
#define M_L1SPTRGTIMESTAMP 0xffffffff
#define M_L1SPTRGDATAFORMAT 0x000000ff
#define M_L1SPTRGTIMEOUTFLAG 0x0000ff00

#define S_L1SPTRGBLOCKLENGTH 0
#define S_L1SPTRGL1PCID 16
#define S_L1SPTRGDETECTORID 24
#define S_L1SPTRGTIMESTAMP 0
#define S_L1SPTRGDATAFORMAT 0
#define S_L1SPTRGTIMEOUTFLAG 8

//--- L1Mask block
// Reserved (word 0)
#define O_L1SPMASKNINPUTEVTS 1
#define O_L1SPMASKNOUTPUTNEVTS 2

class L1MaskSpecialBlock;
class L1PCSpecialBlock;

class L1TPSpecialTrigger : public TObject {

  public:

    L1TPSpecialTrigger();
    ~L1TPSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  AddPCInfo(UInt_t *);

    std::vector<L1PCSpecialBlock> GetL1PCsInfo()  { return fL1PCsInfo;   }

  private:

    std::vector<L1PCSpecialBlock> fL1PCsInfo;

    ClassDef(L1TPSpecialTrigger,1);
};

class L1PCSpecialBlock : public TObject {

public:

  L1PCSpecialBlock();
  L1PCSpecialBlock(const L1PCSpecialBlock&);
  ~L1PCSpecialBlock() {}
  void Clear(Option_t* = "");

  UInt_t  GetBlockLength()                                 { return  fBlockLength;                     }
  void    SetBlockLength(UInt_t value)                     { fBlockLength = value;                     }
  UChar_t GetL1PCID()                                      { return  fL1PCID;                          }
  void    SetL1PCID(UChar_t value)                         { fL1PCID = value;                          }
  ULong_t GetTimeStamp()                                   { return  fTimeStamp;                       }
  void    SetTimeStamp(ULong_t value)                      { fTimeStamp = value;                       }
  UChar_t GetDataFormat()                                  { return  fDataFormat;                      }
  void    SetDataFormat(UChar_t value)                     { fDataFormat = value;                      }
  UChar_t GetTimeoutFlag()                                 { return  fTimeoutFlag;                     }
  void    SetTimeoutFlag(UChar_t value)                    { fTimeoutFlag = value;                     }
  UInt_t  GetNL1InputEvents()                              { return  fNL1InputEvents;                  }
  void    SetNL1InputEvents(UInt_t value)                  { fNL1InputEvents = value;                  }
  UInt_t  GetNL1SpecialEvents()                            { return  fNL1SpecialEvents;                }
  void    SetNL1SpecialEvents(UInt_t value)                { fNL1SpecialEvents = value;                }
  UInt_t  GetNL1ControlEvents()                            { return  fNL1ControlEvents;                }
  void    SetNL1ControlEvents(UInt_t value)                { fNL1ControlEvents = value;                }
  UInt_t  GetNL1PeriodicEvents()                           { return  fNL1PeriodicEvents;               }
  void    SetNL1PeriodicEvents(UInt_t value)               { fNL1PeriodicEvents = value;               }
  UInt_t  GetNL1PhysicsEvents()                            { return  fNL1PhysicsEvents;                }
  void    SetNL1PhysicsEvents(UInt_t value)                { fNL1PhysicsEvents = value;                }
  UInt_t  GetNL1PhysicsEventsInMultipleMasks()             { return  fNL1PhysicsEventsInMultipleMasks; }
  void    SetNL1PhysicsEventsInMultipleMasks(UInt_t value) { fNL1PhysicsEventsInMultipleMasks = value; }
  UInt_t  GetNL1DataRequests()                             { return  fNL1DataRequests;                 }
  void    SetNL1DataRequests(UInt_t value)                 { fNL1DataRequests = value;                 }
  UInt_t  GetNL1OutputEvents()                             { return  fNL1OutputEvents;                 }
  void    SetNL1OutputEvents(UInt_t value)                 { fNL1OutputEvents = value;                 }
  UInt_t  GetNL1AcceptedEvents()                           { return  fNL1AcceptedEvents;               }
  void    SetNL1AcceptedEvents(UInt_t value)               { fNL1AcceptedEvents = value;               }
  UInt_t  GetNL1TimeoutEvents()                            { return  fNL1TimeoutEvents;                }
  void    SetNL1TimeoutEvents(UInt_t value)                { fNL1TimeoutEvents = value;                }
  UInt_t  GetNL1AllDisabledEvents()                        { return  fNL1AllDisabledEvents;            }
  void    SetNL1AllDisabledEvents(UInt_t value)            { fNL1AllDisabledEvents = value;            }
  UInt_t  GetNL1BypassedEvents()                           { return  fNL1BypassedEvents;               }
  void    SetNL1BypassedEvents(UInt_t value)               { fNL1BypassedEvents = value;               }
  UInt_t  GetNL1FlagAlgoEvents()                           { return  fNL1FlagAlgoEvents;               }
  void    SetNL1FlagAlgoEvents(UInt_t value)               { fNL1FlagAlgoEvents = value;               }
  UInt_t  GetNL1AutopassEvents()                           { return  fNL1AutopassEvents;               }
  void    SetNL1AutopassEvents(UInt_t value)               { fNL1AutopassEvents = value;               }
  std::vector<L1MaskSpecialBlock> GetL1MasksInfo()         { return  fL1MasksInfo;                     }
  void    SetL1MasksInfo(std::vector<L1MaskSpecialBlock> &value) { fL1MasksInfo = value;                }

private:

  UInt_t  fBlockLength;
  UChar_t fL1PCID;
  UInt_t  fDetectorID;        //!  Transient data member
  ULong_t fTimeStamp;
  UChar_t fDataFormat;
  UChar_t fTimeoutFlag;
  UInt_t  fReserved;
  UInt_t  fNL1InputEvents;      ///< Total number of input events
  UInt_t  fNL1SpecialEvents;
  UInt_t  fNL1ControlEvents;    ///< Number of input control triggers
  UInt_t  fNL1PeriodicEvents;   ///< Number of input periodic triggers
  UInt_t  fNL1PhysicsEvents;    ///< Number of input special triggers
  UInt_t  fNL1PhysicsEventsInMultipleMasks;
  UInt_t  fNL1DataRequests;
  UInt_t  fNL1OutputEvents;     ///< Total number of output events
  UInt_t  fNL1AcceptedEvents;
  UInt_t  fNL1TimeoutEvents;
  UInt_t  fNL1AllDisabledEvents;
  UInt_t  fNL1BypassedEvents;
  UInt_t  fNL1FlagAlgoEvents;
  UInt_t  fNL1AutopassEvents;
  std::vector<L1MaskSpecialBlock> fL1MasksInfo;

  ClassDef(L1PCSpecialBlock,1);
};

class L1MaskSpecialBlock : public TObject {

  public:

    L1MaskSpecialBlock();
    L1MaskSpecialBlock(const L1MaskSpecialBlock&);
    ~L1MaskSpecialBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              }
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               }
    UInt_t   GetNL1InputEvents()                 { return fNL1InputEvents;        }
    void     SetNL1InputEvents(UInt_t val)       { fNL1InputEvents = val;         }
    UInt_t   GetNL1OutputEvents()                { return fNL1OutputEvents;       }
    void     SetNL1OutputEvents(UInt_t val)      { fNL1OutputEvents = val;        }

  private:

    UChar_t    fL0MaskID;
    UInt_t     fNL1InputEvents;
    UInt_t     fNL1OutputEvents;
    UInt_t     fReserved;

    ClassDef(L1MaskSpecialBlock,1);
};

#endif
