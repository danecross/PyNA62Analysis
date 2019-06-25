// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L2EBSpecialTrigger_H
#define L2EBSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

#define O_L2SPTRGBLOCKLENGTH 0
#define O_L2SPTRGL2PCID 0
#define O_L2SPTRGDETECTORID 0
#define O_L2SPTRGTIMESTAMP 1
#define O_L2SPTRGDATAFORMAT 2
#define O_L2SPTRGTIMEOUTFLAG 2
// Reserved (word 3)
#define O_L2SPTRGNL2INPUTEVTS 4
#define O_L2SPTRGNL2SPECIALEVTS 5
#define O_L2SPTRGNL2CONTROLEVTS 6
#define O_L2SPTRGNL2PERIODICEVTS 7
#define O_L2SPTRGNL2PHYSICSEVTS 8
#define O_L2SPTRGNL2PHYSICSEVTSMM 9
#define O_L2SPTRGNL2OUTPUTEVTS 10
#define O_L2SPTRGNL2ACCEPTEDEVTS 11
#define O_L2SPTRGNL2TIMEOUTEVTS 12
#define O_L2SPTRGNL2ALLDISABLEDEVTS 13
#define O_L2SPTRGNL2BYPASSEDEVTS 14
#define O_L2SPTRGNL2FLAGALGOEVTS 15
#define O_L2SPTRGNL2AUTOPASSEVTS 16
#define O_L2SPTRGL2MASKSINFO 17

#define M_L2SPTRGBLOCKLENGTH 0x0000ffff
#define M_L2SPTRGL2PCID 0x00ff0000
#define M_L2SPTRGDETECTORID 0xff000000
#define M_L2SPTRGTIMESTAMP 0xffffffff
#define M_L2SPTRGDATAFORMAT 0x000000ff
#define M_L2SPTRGTIMEOUTFLAG 0x0000ff00

#define S_L2SPTRGBLOCKLENGTH 0
#define S_L2SPTRGL2PCID 16
#define S_L2SPTRGDETECTORID 24
#define S_L2SPTRGTIMESTAMP 0
#define S_L2SPTRGDATAFORMAT 0
#define S_L2SPTRGTIMEOUTFLAG 8

//--- L2Mask block
// Reserved (word 0)
#define O_L2SPMASKNINPUTEVTS 1
#define O_L2SPMASKNOUTPUTNEVTS 2

class L2MaskSpecialBlock;
class L2PCSpecialBlock;

class L2EBSpecialTrigger : public TObject {

  public:

    L2EBSpecialTrigger();
    ~L2EBSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  AddPCInfo(UInt_t *);

    std::vector<L2PCSpecialBlock> GetL2PCsInfo()  { return fL2PCsInfo;   };

  private:

    std::vector<L2PCSpecialBlock> fL2PCsInfo;

    ClassDef(L2EBSpecialTrigger,1);
};

class L2PCSpecialBlock : public TObject {

  public:

    L2PCSpecialBlock();
    L2PCSpecialBlock(const L2PCSpecialBlock&);
    ~L2PCSpecialBlock() {}
    void Clear(Option_t* = "");

    UInt_t  GetBlockLength()                                 { return  fBlockLength;                     };
    void    SetBlockLength(UInt_t value)                     { fBlockLength = value;                     };
    UChar_t GetL2PCID()                                      { return  fL2PCID;                          };
    void    SetL2PCID(UChar_t value)                         { fL2PCID = value;                          };
    ULong_t GetTimeStamp()                                   { return  fTimeStamp;                       };
    void    SetTimeStamp(ULong_t value)                      { fTimeStamp = value;                       };
    UChar_t GetDataFormat()                                  { return  fDataFormat;                      };
    void    SetDataFormat(UChar_t value)                     { fDataFormat = value;                      };
    UChar_t GetTimeoutFlag()                                 { return  fTimeoutFlag;                     };
    void    SetTimeoutFlag(UChar_t value)                    { fTimeoutFlag = value;                     };
    UInt_t  GetNL2InputEvents()                              { return  fNL2InputEvents;                  };
    void    SetNL2InputEvents(UInt_t value)                  { fNL2InputEvents = value;                  };
    UInt_t  GetNL2SpecialEvents()                            { return  fNL2SpecialEvents;                };
    void    SetNL2SpecialEvents(UInt_t value)                { fNL2SpecialEvents = value;                };
    UInt_t  GetNL2ControlEvents()                            { return  fNL2ControlEvents;                };
    void    SetNL2ControlEvents(UInt_t value)                { fNL2ControlEvents = value;                };
    UInt_t  GetNL2PeriodicEvents()                           { return  fNL2PeriodicEvents;               };
    void    SetNL2PeriodicEvents(UInt_t value)               { fNL2PeriodicEvents = value;               };
    UInt_t  GetNL2PhysicsEvents()                            { return  fNL2PhysicsEvents;                };
    void    SetNL2PhysicsEvents(UInt_t value)                { fNL2PhysicsEvents = value;                };
    UInt_t  GetNL2PhysicsEventsInMultipleMasks()             { return  fNL2PhysicsEventsInMultipleMasks; };
    void    SetNL2PhysicsEventsInMultipleMasks(UInt_t value) { fNL2PhysicsEventsInMultipleMasks = value; };
    UInt_t  GetNL2OutputEvents()                             { return  fNL2OutputEvents;                 };
    void    SetNL2OutputEvents(UInt_t value)                 { fNL2OutputEvents = value;                 };
    UInt_t  GetNL2AcceptedEvents()                           { return  fNL2AcceptedEvents;               };
    void    SetNL2AcceptedEvents(UInt_t value)               { fNL2AcceptedEvents = value;               };
    UInt_t  GetNL2TimeoutEvents()                            { return  fNL2TimeoutEvents;                };
    void    SetNL2TimeoutEvents(UInt_t value)                { fNL2TimeoutEvents = value;                };
    UInt_t  GetNL2AllDisabledEvents()                        { return  fNL2AllDisabledEvents;            };
    void    SetNL2AllDisabledEvents(UInt_t value)            { fNL2AllDisabledEvents = value;            };
    UInt_t  GetNL2BypassedEvents()                           { return  fNL2BypassedEvents;               };
    void    SetNL2BypassedEvents(UInt_t value)               { fNL2BypassedEvents = value;               };
    UInt_t  GetNL2FlagAlgoEvents()                           { return  fNL2FlagAlgoEvents;               };
    void    SetNL2FlagAlgoEvents(UInt_t value)               { fNL2FlagAlgoEvents = value;               };
    UInt_t  GetNL2AutopassEvents()                           { return  fNL2AutopassEvents;               };
    void    SetNL2AutopassEvents(UInt_t value)               { fNL2AutopassEvents = value;               };
    std::vector<L2MaskSpecialBlock> GetL2MasksInfo()         { return  fL2MasksInfo;                     };
    void    SetL2MasksInfo(std::vector<L2MaskSpecialBlock> &value) { fL2MasksInfo = value;                };

  private:

    UInt_t  fBlockLength;
    UChar_t fL2PCID;
    UInt_t  fDetectorID;        //!  Transient data member
    ULong_t fTimeStamp;
    UChar_t fDataFormat;
    UChar_t fTimeoutFlag;
    UInt_t  fReserved;
    UInt_t  fNL2InputEvents;
    UInt_t  fNL2SpecialEvents;
    UInt_t  fNL2ControlEvents;
    UInt_t  fNL2PeriodicEvents;
    UInt_t  fNL2PhysicsEvents;
    UInt_t  fNL2PhysicsEventsInMultipleMasks;
    UInt_t  fNL2OutputEvents;
    UInt_t  fNL2AcceptedEvents;
    UInt_t  fNL2TimeoutEvents;
    UInt_t  fNL2AllDisabledEvents;
    UInt_t  fNL2BypassedEvents;
    UInt_t  fNL2FlagAlgoEvents;
    UInt_t  fNL2AutopassEvents;
    std::vector<L2MaskSpecialBlock> fL2MasksInfo;

    ClassDef(L2PCSpecialBlock,1);
};

class L2MaskSpecialBlock : public TObject {

  public:

    L2MaskSpecialBlock();
    L2MaskSpecialBlock(const L2MaskSpecialBlock&);
    ~L2MaskSpecialBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              };
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               };
    UInt_t   GetNL2InputEvents()                 { return fNL2InputEvents;        };
    void     SetNL2InputEvents(UInt_t val)       { fNL2InputEvents = val;         };
    UInt_t   GetNL2OutputEvents()                { return fNL2OutputEvents;       };
    void     SetNL2OutputEvents(UInt_t val)      { fNL2OutputEvents = val;        };

  private:

    UChar_t    fL0MaskID;
    UInt_t     fNL2InputEvents;
    UInt_t     fNL2OutputEvents;
    UInt_t     fReserved;

    ClassDef(L2MaskSpecialBlock,1);
};

#endif
