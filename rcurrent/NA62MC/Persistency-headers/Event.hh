// --------------------------------------------------------------
// History:
//
// Created by Emanuele Leonardi (Emanuele.Leonardi@cern.ch) 2007-01-10
// Modified by Sergey Podolsky 2011-01-21
// --------------------------------------------------------------
#ifndef Event_h
#define Event_h 1

#include "TObject.h"
#include "TClonesArray.h"
#include "EventBoundary.hh"
#include "TEventInfo.hh"
#include "KinePart.hh"
#include "Event.hh"
#include "TRandom3.h"
#include "Riostream.h"

class Event : public TObject {

public:
  Event();
  virtual ~Event() { Clear(); };
  void Merge(Event*);
  void Purge(TEventInfo);
  EventBoundary* FindEventBoundary(Int_t);
  Int_t    GetID();
  void     SetID(Int_t);
  Int_t    GetStreamID();
  void     SetStreamID(Int_t);
  Double_t GetTime();
  void     SetTime(Double_t);
  Double_t GetEventWeight()            { return fEventWeight; }
  void     SetEventWeight(Double_t val){ fEventWeight = val;  }
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;
  void PrintAll() const;

  Int_t         GetNMergedEvents() const { return fNMergedEvents; }
  TClonesArray* GetEventBoundaries()  const { return fEventBoundaries; }
  Int_t         GetNPurgedKineParts() { return fNPurgedKineParts;  }

  Int_t GetEventNumber()          { return fEventNumber; }
  void  SetEventNumber(Int_t val) { fEventNumber = val;  }

  Int_t         GetNKineParts() const { return fNKineParts; }
  TClonesArray* GetKineParts()  const { return fKineParts; }
  KinePart*     AddKinePart();
  KinePart*     GetKinePart(Int_t);

  Int_t         GetNGeneParts() const { return fNGeneParts; }
  TClonesArray* GetGeneParts()  const { return fGeneParts; }
  GenePart*     AddGenePart();
  GenePart*     GetGenePart(Int_t);

  void StoreRandomState(TRandom3* RandomDecayState, Long_t *RanecuState);
  TRandom3* GetRandomDecayState () {return fRandomDecayState;}
  Long_t* GetRanecuState() {return fRanecuState;}

private:

  Int_t fEventNumber;       ///< MC event number
  Int_t fNGeneParts;        ///< Number of generated particles
  Int_t fNKineParts;        ///< Number of saved particles
  Int_t fNMergedEvents;     ///< Number of merged events
  Int_t fNPurgedKineParts;  ///< Number of removed KineParts
  Double_t fEventWeight;    ///< Weight of the MC event (to account for biasing)

  TClonesArray*  fGeneParts;            ///< Array of generated particles in K rest frame
  TClonesArray*  fKineParts;            ///< Array of saved true particles in lab frame
  TClonesArray*  fEventBoundaries;      ///< Array of event boundaries to separate merged events
  EventBoundary* fCurrentEventBoundary; //! Transient

  static TRandom3* fgRandomDecayState;
  TRandom3* fRandomDecayState; ///< NA62MC random generator state
  Long_t fRanecuState[2];      ///< G4 (CLHEP) random generator state

  ClassDef(Event,1)
};

#endif
