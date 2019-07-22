// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef NA62VRawEncoder_H
#define NA62VRawEncoder_H

#include "Rtypes.h"
#include "TFile.h"

#include "NA62VNamedModule.hh"
#include "NA62VReconstruction.hh"

#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"
#include "TSpecialTriggerEvent.hh"

class BinaryEvent;

class NA62VRawEncoder : public NA62VNamedModule{

  public:

    NA62VRawEncoder(NA62VReconstruction*, TString);
    virtual ~NA62VRawEncoder();
    virtual BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t) = 0;
    virtual void Init(){};
    virtual void End(){};
    void Open();
    void Close();

    NA62VRawEncoder * GetEncoder()                                          { return fEncoder;                      };
    void SetEncoder(NA62VRawEncoder * value)                                { fEncoder=value;                       };

  public:

    BinaryEvent *          GetBinaryEvent()                                     { return fBinaryEvent;                  };  
    void                   SetBinaryEvent(BinaryEvent * value)                  { fBinaryEvent = value;                 };
    TSpecialTriggerEvent * GetSpecialTriggerEvent()                             { return fSpecialTriggerEvent;          };
    void                   SetSpecialTriggerEvent(TSpecialTriggerEvent * value) { fSpecialTriggerEvent = value;         };

    FILE *                 GetBinaryFile()                                      { return fBinaryFile;                   };
    FILE *                 GetHeaderFile()                                      { return fHeaderFile;                   };
    TString                GetBinaryFileName()                                  { return fBinaryFileName;               };
    TString                GetHeaderFileName()                                  { return fHeaderFileName;               };
    void                   SetBinaryFileName(TString val)                       { fBinaryFileName=val;                  };
    void                   SetHeaderFileName(TString val)                       { fHeaderFileName=val;                  };
    Int_t                  GetNHitsOutOfSlot()                                  { return fNHitsOutOfSlot;               };
    void                   SetNHitsOutOfSlot(Int_t value)                       { fNHitsOutOfSlot = value;              };

  protected:

    NA62VReconstruction * fReco;

    BinaryEvent * fBinaryEvent;
    TSpecialTriggerEvent * fSpecialTriggerEvent;

    NA62VRawEncoder * fEncoder;

    Int_t fDetID;
    Int_t fNROBoards;
    Int_t fNROMezzanines;
    ULong_t fTimeStamp;
    ULong_t fFineTime;
    Double_t* fROMezzaninesT0;
    Int_t fNWords;
    Int_t * fNWordsPerROBoard;

    FILE * fBinaryFile;
    FILE * fHeaderFile;
    TString fBinaryFileName;
    TString fHeaderFileName;
    Int_t fNHitsOutOfSlot;        //for debug
};
#endif
