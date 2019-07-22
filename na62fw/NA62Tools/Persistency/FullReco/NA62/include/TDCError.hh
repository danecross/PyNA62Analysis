// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TDCError_H
#define TDCError_H
#include "TDigiVError.hh"

class TDCError : public TDigiVError {

  public:

    TDCError();
    ~TDCError(){};
    void Clear(Option_t* = "");

  public:

    Int_t                GetTDCID()                                         { return fTDCID;                        };
    void                 SetTDCID(Int_t value)                              { fTDCID = value;                       };
    Int_t                GetTDCBID()                                        { return GetROMezzanineID();            };
    void                 SetTDCBID(Int_t value)                             { SetROMezzanineID(value);              };
    Int_t                GetTEL62ID()                                       { return GetROBoardID();                };
    void                 SetTEL62ID(Int_t value)                            { SetROBoardID(value);                  };

  private:

    Int_t fTDCID;

    ClassDef(TDCError,1);
};
#endif
