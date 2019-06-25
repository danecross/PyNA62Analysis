#ifndef EVENTHEADER_H
#define EVENTHEADER_H 1

#include "Rtypes.h"
#include "TObject.h"
#include "L0TPData.hh"
#include "L1TPData.hh"
#include "L2EBData.hh"
#include "BeamData.hh"
#include "HLTEvent.hh"
#include "L0TPSpecialTrigger.hh"
#include "L1TPSpecialTrigger.hh"
#include "L2EBSpecialTrigger.hh"
#include "BeamSpecialTrigger.hh"

#define O_EVENTNUMBER 0
#define O_EVENTLENGTH 1
#define O_BURSTID 2
#define O_TIMESTAMP 3
#define O_TRIGGERTYPE 4
#define O_FINETIME 5
#define O_NSUBDET 5
#define O_DATAPROCESSINGIDENTIFIER 6
#define O_BURSTTIME 7
#define O_DETECTORTABLE 8

#define M_EVENTNUMBER 0x00ffffff
#define M_TRIGGERTYPE 0x00ffffff
#define M_FINETIME 0x000000ff
#define M_NSUBDET 0x0000ff00
#define M_DETECTOROFFSET 0x00ffffff
#define M_DETECTORID 0xff000000

#define S_NSUBDET 8
#define S_DETECTORID 26

class EventHeader : public TObject {

  public:

    EventHeader();
    ~EventHeader();
    Bool_t SetHeader(UInt_t *);
    Bool_t SetL0TPData(UInt_t *);
    Bool_t SetL1TPData(UInt_t *);
    Bool_t SetL2EBData(UInt_t *);
    void   SetBeamInstantaneousIntensity(Double_t, Double_t);
    Bool_t SetL0TPSpecialTrigger(UInt_t *);
    Bool_t SetL1TPSpecialTrigger(UInt_t *);
    Bool_t SetL2EBSpecialTrigger(UInt_t *);
    Bool_t SetBeamSpecialTrigger(UInt_t *,UInt_t);
    UInt_t GetEventNumber()                      { return fEventNumber;        }
    UInt_t GetEventLength()                      { return fEventLength;        }
    UInt_t GetBurstID()                          { return fBurstID;            }
    UInt_t GetRunID()                            { return fRunID;              }
    UInt_t GetTimeStamp()                        { return fTimeStamp;          }
    UInt_t GetPreviousTimeStamp()                { return fPreviousTimeStamp;  }
    UInt_t GetTriggerType()                      { return fTriggerType;        }
    void   SetTriggerType(UInt_t val)            { fTriggerType = val;         }
    UInt_t GetNumOfDetectors()                   { return fNumOfDetectors;     }
    UInt_t GetFineTime()                         { return fFineTime;           }
    UInt_t GetBurstTime()                        { return fBurstTime;          }
    L0TPData * GetL0TPData()                     { return fL0TPData;           }
    L1TPData * GetL1TPData()                     { return fL1TPData;           }
    L2EBData * GetL2EBData()                     { return fL2EBData;           }
    BeamData * GetBeamData()                     { return fBeamData;           }
    HLTEvent * GetHLTEvent()                     { return fHLTEvent;           }
    L0TPSpecialTrigger * GetL0TPSpecialTrigger() { return fL0TPSpecialTrigger; }
    L1TPSpecialTrigger * GetL1TPSpecialTrigger() { return fL1TPSpecialTrigger; }
    L2EBSpecialTrigger * GetL2EBSpecialTrigger() { return fL2EBSpecialTrigger; }
    BeamSpecialTrigger * GetBeamSpecialTrigger() { return fBeamSpecialTrigger; }
    UInt_t GetEventQualityMask()                 { return fEventQualityMask;   }

    // BurstID and RunID initilised by DecodeBurstHeader for data format >= 2015
    void SetRunID(UInt_t val)                    { fRunID = val;               }
    void SetBurstID(UInt_t val)                  { fBurstID = val;             }
    void SetBurstTime(UInt_t val)                { fBurstTime = val;           }
    void SetEventNumber(UInt_t val)              { fEventNumber = val;         }
    void SetTimeStamp(UInt_t val)                { fTimeStamp = val;           }
    void SetEventQualityMask(UInt_t val)         { fEventQualityMask = val;    }
    void UpdateEventQualityMask(UInt_t);

    void SetStartByte(ULong64_t value)           { fStartByte=value;           }
    ULong64_t GetStartByte()                     { return fStartByte;          }
    void SetDetectorID(ULong64_t value)          { fDetectorID = value;        }
    UInt_t GetDetectorID()                       { return fDetectorID;         }
    void SetEOBFileDescriptor(FILE *value)       { fEOBFileDescriptor=value;   }
    FILE * GetEOBFileDescriptor()                { return fEOBFileDescriptor;  }

    void ClearDIMBlock();

  private:

    UInt_t fEventNumber;
    UInt_t fEventLength;
    UInt_t fBurstID;
    UInt_t fRunID;
    UInt_t fTimeStamp;
    UInt_t fPreviousTimeStamp;
    UInt_t fTriggerType;
    UInt_t fNumOfDetectors;
    UInt_t fFineTime;
    UInt_t fBurstTime;
    UInt_t fDetectorID; //!  Transient data member
    ULong64_t fStartByte; //!  Transient data member
    FILE *fEOBFileDescriptor; //!  Transient data member
    L0TPData *fL0TPData; //!  Transient data member
    L1TPData *fL1TPData; //!  Transient data member
    L2EBData *fL2EBData; //!  Transient data member
    BeamData *fBeamData; //!  Transient data member
    HLTEvent *fHLTEvent; //!  Transient data member
    L0TPSpecialTrigger *fL0TPSpecialTrigger; //!  Transient data member
    L1TPSpecialTrigger *fL1TPSpecialTrigger; //!  Transient data member
    L2EBSpecialTrigger *fL2EBSpecialTrigger; //!  Transient data member
    BeamSpecialTrigger *fBeamSpecialTrigger; //!  Transient data member
    UInt_t fEventQualityMask;

    ClassDef(EventHeader,1);
};
#endif
