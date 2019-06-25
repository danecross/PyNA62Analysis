// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TSpecialTrigger_H
#define TSpecialTrigger_H
#include "TObject.h"

class TSpecialTrigger : public TObject {

  public:

    TSpecialTrigger();
    ~TSpecialTrigger(){};

    void Clear(Option_t* = "");

  public:

    void SetDataSourceID(Int_t value)       { fDataSourceID=value;      };
    Int_t GetDataSourceID()                 { return fDataSourceID;     };
    void SetROBoardID(Int_t value)          { fROBoardID=value;         };
    Int_t GetROBoardID()                    { return fROBoardID;        };
    void SetTimeStamp(UInt_t value)         { fTimeStamp=value;         };
    UInt_t GetTimeStamp()                   { return fTimeStamp;        };
    void SetTriggerType(Int_t value)        { fTriggerType=value;       };
    Int_t GetTriggerType()                  { return fTriggerType;      };
    void SetTriggerCount(UInt_t value)      { fTriggerCount=value;      };
    UInt_t GetTriggerCount()                { return fTriggerCount;     };
    void SetErrorsFlag(UInt_t value)        { fErrorsFlag=value;        };
    UInt_t GetErrorsFlag()                  { return fErrorsFlag;       };

  private:

    Int_t   fDataSourceID;
    Int_t   fROBoardID;
    UInt_t  fTimeStamp;
    Int_t   fTriggerType;
    UInt_t  fTriggerCount;
    UInt_t  fErrorsFlag;

    ClassDef(TSpecialTrigger,1);
};
#endif
