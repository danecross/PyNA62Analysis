// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECNEWCHODASSOCIATIONRECORD_HH 
#define INCLUDE_SPECNEWCHODASSOCIATIONRECORD_HH 1

#include "NA62Global.hh"
#include "TVector2.h"
#include "TVector3.h"

class SpectrometerNewCHODAssociationRecord {

public:

  SpectrometerNewCHODAssociationRecord();
  SpectrometerNewCHODAssociationRecord
  (Int_t, Int_t, Int_t,	Double_t, Double_t, Double_t, Double_t, Double_t, Bool_t);
  SpectrometerNewCHODAssociationRecord
  (Int_t, Int_t, Int_t,	Double_t, Double_t, Double_t, TVector2, Bool_t);

  virtual ~SpectrometerNewCHODAssociationRecord() {}

  Int_t    GetRecoHitID()              { return fRecoHitID;               }
  Int_t    GetType()                   { return fType;                    }
  Int_t    GetTileID()                 { return fTileID;                  }
  Double_t GetRecoHitTime()            { return fRecoHitTime;             }
  Double_t GetTrackTileDistance()      { return fTrackTileDistance;       }
  Double_t GetTrackRecoHitDistance()   { return fTrackRecoHitDistance;    }
  Bool_t   GetDirectMatch()            { return fDirectMatch;             }
  Double_t GetRecoHitX()               { return fRecoHitX;                }
  Double_t GetRecoHitY()               { return fRecoHitY;                }
  TVector2 GetRecoHitXY()              { return TVector2(fRecoHitX, fRecoHitY); }
  TVector3 GetRecoHitPosition()        { return TVector3(fRecoHitX, fRecoHitY, 238082.5); }

  Bool_t IsLoose()                     { return (fType==kLooseCandidate);       }
  Bool_t IsLooseMasked()               { return (fType==kLooseMaskedCandidate); }
  Bool_t IsTight()                     { return (fType==kTightCandidate);       }

  void SetRecoHitID   (Int_t val)                { fRecoHitID = val;                         }
  void SetType        (Int_t val)                { fType = val;                              }
  void SetTileID      (Int_t val)                { fTileID = val;                            }
  void SetRecoHitTime (Double_t val)             { fRecoHitTime = val;                       }
  void SetTrackTileDistance    (Double_t val)    { fTrackTileDistance = val;                 }
  void SetTrackRecoHitDistance (Double_t val)    { fTrackRecoHitDistance = val;              }
  void SetRecoHitX      (Double_t val)           { fRecoHitX = val;                          }
  void SetRecoHitY      (Double_t val)           { fRecoHitY = val;                          }
  void SetRecoHitXY     (Double_t x, Double_t y) { fRecoHitX = x; fRecoHitY = y;             }
  void SetRecoHitXY     (TVector2 val)           { fRecoHitX = val.X(); fRecoHitY = val.Y(); }

  void Clear();
  void Print();

private:

  Int_t    fRecoHitID;   ///< ID of the associated NewCHOD RecoHit
  Int_t    fTileID;      ///< NewCHOD RecoHit tile ID
  Int_t    fType;        ///< Associated candidate type: tight, loose, loose masked, undefined
  Double_t fRecoHitTime; ///< NewCHOD RecoHit time
  Double_t fTrackTileDistance; ///< Distance between extrapolated track and tile boundary (0 if inside tile)
  Double_t fTrackRecoHitDistance; ///< Distance between extrapolated track and centre of NewCHOD tile
  Double_t fRecoHitX;    ///< Centre of NewCHOD tile: X coordinate
  Double_t fRecoHitY;    ///< Centre of NewCHOD tile: Y coordinate
  Bool_t   fDirectMatch; ///< Extrapolated track in the same tile as RecoHit?
};

#endif
