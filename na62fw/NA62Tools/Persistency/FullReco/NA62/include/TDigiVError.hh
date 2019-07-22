// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-12-31
//
// --------------------------------------------------------------
#ifndef TDigiVError_H
#define TDigiVError_H
#include "TObject.h"

class TDigiVError : public TObject {

  public:

    TDigiVError();
    ~TDigiVError(){};
    void Clear(Option_t* = "");

  public:

    Int_t                GetROMezzanineID()               { return fROMezzanineID;   };
    void                 SetROMezzanineID(Int_t value)    { fROMezzanineID = value;  };
    Int_t                GetROBoardID()                   { return fROBoardID;       };
    void                 SetROBoardID(Int_t value)        { fROBoardID = value;      };
    Int_t                GetType()                        { return fType;            };
    void                 SetType(Int_t value)             { fType = value;           };
    Bool_t               GetFatal()                       { return fFatal;           };
    void                 SetFatal(Bool_t value)           { fFatal = value;          };

  private:

    Int_t  fROMezzanineID;
    Int_t  fROBoardID;
    Int_t  fType;
    Bool_t fFatal;

    ClassDef(TDigiVError,1);
};
#endif
