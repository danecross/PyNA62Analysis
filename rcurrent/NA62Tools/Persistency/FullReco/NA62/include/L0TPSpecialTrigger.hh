// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L0TPSpecialTrigger_H
#define L0TPSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

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
#define O_L0DATATYPE 0
#define O_L0LATENCY 0
#define O_L0FINETIMEBITS 1
#define O_L0REFDET 1
#define O_L0CTRLDET 1
#define O_L0CTRLDWN 2
#define O_L0NCTRLTRIGGEN 3
#define O_L0PREVTIMESTAMP 4
#define O_L0TRIGTYPE 5
#define O_L0PREVTRIGTYPE 5
#define O_L0TRIGFLAGS 5
#define O_L0NPRIMITIVES 6
// Primitives in words 6-12
#define O_L0NCHOKES 13
#define O_L0NERRORS 14
#define O_L0NPERIODIC 15
#define O_L0NCALIB  16
#define O_L0NCTRLTRIGSNT 17
// Info from the L0 masks
#define O_L0MASK_ID  18
#define O_L0MASK_DWN 19
#define O_L0MASK_NTRGSENT 20
#define O_L0MASK_NTRGGENERATED 21
#define O_L0MASK_REQUIREDPRIMBIT 22
#define O_L0MASK_DONTCAREPRIMBIT 22
#define O_L0MASK_RESERVED 29

#define M_L0DATATYPE 0x0000ff00
#define M_L0LATENCY 0xffff0000
#define M_L0FINETIMEBITS 0x000000ff
#define M_L0REFDET 0x0000ff00
#define M_L0CTRLDET 0x00ff0000
#define M_L0CTRLDWN 0xffff0000
#define M_L0NCTRLTRIGGEN 0xffffffff
#define M_L0PREVTIMESTAMP 0xffffffff
#define M_L0TRIGTYPE 0x000000ff
#define M_L0PREVTRIGTYPE 0x0000ff00
#define M_L0TRIGFLAGS 0xffff0000
#define M_L0NPRIMITIVES 0xffffffff
#define M_L0NCHOKES 0xffffffff
#define M_L0NERRORS 0xffffffff
#define M_L0NPERIODIC 0xffffffff
#define M_L0NCALIB  0xffffffff
#define M_L0NCTRLTRIGSNT 0xffffffff
// Info from the L0 masks
#define M_L0MASK_ID  0x000000ff
#define M_L0MASK_DWN 0xffffffff
#define M_L0MASK_NTRGSENT 0xffffffff
#define M_L0MASK_NTRGGENERATED 0xffffffff
#define M_L0MASK_REQUIREDPRIMBIT 0x0000ffff
#define M_L0MASK_DONTCAREPRIMBIT 0xffff0000
#define M_L0MASK_RESERVED 0xffffffff

#define S_L0DATATYPE 8
#define S_L0LATENCY 16
#define S_L0FINETIMEBITS 0
#define S_L0REFDET 8
#define S_L0CTRLDET 16
#define S_L0CTRLDWN 16
#define S_L0NCTRLTRIGGEN 0
#define S_L0PREVTIMESTAMP 0
#define S_L0TRIGTYPE 0
#define S_L0PREVTRIGTYPE 8
#define S_L0TRIGFLAGS 16
#define S_L0NPRIMITIVES 0
#define S_L0NCHOKES 0
#define S_L0NERRORS 0
#define S_L0NPERIODIC 0
#define S_L0NCALIB  0
#define S_L0NCTRLTRIGSNT 0
// Info from the L0 masks
#define S_L0MASK_ID  0
#define S_L0MASK_DWN 0
#define S_L0MASK_NTRGSENT 0
#define S_L0MASK_NTRGGENERATED 0
#define S_L0MASK_REQUIREDPRIMBIT 0
#define S_L0MASK_DONTCAREPRIMBIT 16
#define S_L0MASK_RESERVED 0

class L0Mask;

class L0TPSpecialTrigger : public TObject {

  public:

    L0TPSpecialTrigger();
    ~L0TPSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  SetHeader(UInt_t *);
    UInt_t  GetEventLength()                     { return fEventLength;                     };
    ULong_t GetTimeStamp()                       { return fTimeStamp;                       };

    UInt_t  GetDataType()                        { return fDataType;                        };
    UInt_t  GetLatency()                         { return fLatency;                         };
    UInt_t  GetFineTimeBits()                    { return fFineTimeBits;                    };
    ULong_t GetReferenceDetector()               { return fReferenceDetector;               };
    ULong_t GetControlDetector()                 { return fControlDetector;                 };
    ULong_t GetPreviousTimeStamp()               { return fPreviousTimeStamp;               };
    UInt_t  GetTriggerType()                     { return fTriggerType;                     };
    UInt_t  GetPreviousTriggerType()             { return fPreviousTriggerType;             };
    UInt_t  GetTriggerFlags()                    { return fTriggerFlags;                    };
    UInt_t  GetNMaxDetectors()                   { return fNMaxDetectors;                   };
    std::vector<UInt_t> GetNPrimitives()         { return fNPrimitives;                     };
    UInt_t  GetNChokeTriggers()                  { return fNChokeTriggers;                  };
    UInt_t  GetNErrorTriggers()                  { return fNErrorTriggers;                  };
    UInt_t  GetNPeriodicTriggers()               { return fNPeriodicTriggers;               };
    UInt_t  GetNCalibTriggers()                  { return fNCalibTriggers;                  };
    UInt_t  GetNControlTriggersSent()            { return fNControlTriggersSent;            };
    UInt_t  GetNControlTriggersGenerated()       { return fNControlTriggersGenerated;       };
    UInt_t  GetControlTriggerDownscalingFactor() { return fControlTriggerDownscalingFactor; };
    std::vector<L0Mask> GetL0Masks()             { return fL0Masks;                         };

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;        //!  Transient data member
    UInt_t  fDataBlockFormat;   //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords; //!  Transient data member

    // Subdetector Block Header (L0TP-specific part)
    UInt_t  fDataType;            
    UInt_t  fLatency;             
    UInt_t  fFineTimeBits;        
    UInt_t  fReferenceDetector;  
    UInt_t  fControlDetector;  
    ULong_t fPreviousTimeStamp;   
    UInt_t  fTriggerType;         
    UInt_t  fPreviousTriggerType; 
    UInt_t  fTriggerFlags;        
    UInt_t  fNMaxDetectors;      //!  Transient data member    
    std::vector<UInt_t> fNPrimitives;         
    UInt_t  fNChokeTriggers;      
    UInt_t  fNErrorTriggers;      
    UInt_t  fNPeriodicTriggers;     
    UInt_t  fNCalibTriggers;      
    UInt_t  fNControlTriggersSent;  
    UInt_t  fNControlTriggersGenerated;  
    UInt_t  fControlTriggerDownscalingFactor;
    UInt_t  fReserved;      
    // Info from the L0 masks
    std::vector<L0Mask> fL0Masks;

    ClassDef(L0TPSpecialTrigger,1);
};

class L0Mask : public TObject {

  public:

    L0Mask();
    L0Mask(const L0Mask&);
    ~L0Mask(){};
    void Clear(Option_t* = "");
    UInt_t GetMaskID()                                     { return fMaskID;              };
    void   SetMaskID(UInt_t val)                           { fMaskID = val;               };
    UInt_t GetDownscalingFactor()                          { return fDownscalingFactor;   };
    void   SetDownscalingFactor(UInt_t val)                { fDownscalingFactor = val;    };
    UInt_t GetNTriggersSent()                              { return fNTriggersSent;       };
    void   SetNTriggersSent(UInt_t val)                    { fNTriggersSent = val;        };
    UInt_t GetNTriggersGenerated()                         { return fNTriggersGenerated;  };
    void   SetNTriggersGenerated(UInt_t val)               { fNTriggersGenerated = val;   };
    std::vector<UInt_t> GetRequiredPrimBitMask()           { return fRequiredPrimBitMask; };
    void   SetRequiredPrimBitMask(std::vector<UInt_t> &val){ fRequiredPrimBitMask = val;  };
    std::vector<UInt_t> GetDontcarePrimBitMask()           { return fDontcarePrimBitMask; };
    void   SetDontcarePrimBitMask(std::vector<UInt_t> &val){ fDontcarePrimBitMask = val;  };

  private:

    UInt_t              fMaskID;              
    UInt_t              fDownscalingFactor;         
    UInt_t              fNTriggersSent;       
    UInt_t              fNTriggersGenerated;  
    std::vector<UInt_t> fRequiredPrimBitMask; 
    std::vector<UInt_t> fDontcarePrimBitMask; 

    ClassDef(L0Mask,1);
};
#endif
