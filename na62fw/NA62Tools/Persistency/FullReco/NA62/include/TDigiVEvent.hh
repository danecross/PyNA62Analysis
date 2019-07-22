// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-18
//
// --------------------------------------------------------------
#ifndef TDigiVEvent_H
#define TDigiVEvent_H

#include "TDetectorVEvent.hh"
#include "TDigiVCandidate.hh"
#include "TDigiVError.hh"
#include "TClass.h"

#include "TDetectorVHit.hh"
#include "TVDigi.hh"

class TDigiVEvent : public TDetectorVEvent {

  public:

    TDigiVEvent();
    TDigiVEvent(const TDigiVEvent &);
    TDigiVEvent(TClass *, TClass *, TClass *);
    TDigiVEvent(TClass *, TClass *, TClass *, Int_t);
    ~TDigiVEvent();

    TVDigi * AddDigi();
    TVDigi * AddDigi(Int_t);
    TVDigi * AddDigi(TDetectorVHit *);
    Int_t GetNDigis();
    TClonesArray * GetDigis();
    TDigiVCandidate * AddCandidate();
    TDigiVCandidate * GetCandidate(Int_t);
    TDigiVError * AddError(Int_t ErrorType);
    TDigiVError * GetError(Int_t iError);
    void UpdateErrorMask(Int_t ErrorType);
    void Clear(Option_t* = "");
    void RemoveCandidate(Int_t);

    Int_t          GetNCandidates()                   { return fNCandidates;   }
    void           SetNCandidates(Int_t value)        { fNCandidates = value;  }
    TClonesArray*  GetCandidates()                    { return fCandidates;    }
    void           SetCandidates(TClonesArray* value) { fCandidates = value;   }
    ULong64_t      GetErrorMask()                     { return fErrorMask;     }
    Int_t          GetNErrors()                       { return fNErrors;       }
    void           SetNErrors(Int_t value)            { fNErrors = value;      }
    TClonesArray*  GetErrors()                        { return fErrors;        }

  private:

    Int_t         fNCandidates;
    TClonesArray* fCandidates;
    ULong64_t     fErrorMask;   //!  Transient data member
    Int_t         fNErrors;
    TClonesArray* fErrors;

    ClassDef(TDigiVEvent,1);
};
#endif
