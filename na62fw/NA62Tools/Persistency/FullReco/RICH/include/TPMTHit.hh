// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TPMTHIT_H
#define TPMTHIT_H

#include "TObject.h"
#include "TVector3.h"

class TPMTHit : public TObject {

public:

  TPMTHit() : fPosition(), fEnergy(0.),fTime(0.),fMCTrackID(-1) {}

  void Clear(Option_t* = "");

  TVector3             GetPosition()                                      { return fPosition;                     };
  void                 SetPosition(TVector3 value)                        { fPosition = value;                    };
  Double_t             GetEnergy()                                        { return fEnergy;                       };
  void                 SetEnergy(Double_t value)                          { fEnergy = value;                      };
  Double_t             GetTime()                                          { return fTime;                         };
  void                 SetTime(Double_t value)                            { fTime = value;                        };
  Int_t                GetMCTrackID()                                     { return fMCTrackID;                    };
  void                 SetMCTrackID(Int_t value)                          { fMCTrackID = value;                   };

private:

  TVector3   fPosition;
  Double_t   fEnergy;
  Double_t   fTime;
  Int_t      fMCTrackID; // For MCTruth Association

  ClassDef(TPMTHit,1);
};
#endif
