// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHODEvent_H
#define TRecoCHODEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCHODCandidate.hh"
#include "TRecoCHODHit.hh"

class TRecoCHODEvent : public TRecoVEvent {

    public:

      TRecoCHODEvent();
      ~TRecoCHODEvent();

      void Clear(Option_t* = "");

      TRecoCHODCandidate * GetTimeCandidate();

    public:
      Int_t             GetNTimeCandidates()                          { return fNTimeCandidates;         };
      void              SetNTimeCandidates(Int_t value)               { fNTimeCandidates = value;        };

      Int_t             GetNQuadrants()                               { return fNQuadrants;              };
      void              SetNQuadrants(Int_t value)                    { fNQuadrants = value;             };

    private:
      Int_t fNTimeCandidates;
      Int_t fNQuadrants;

      ClassDef(TRecoCHODEvent,1);
};
#endif
