#ifndef TSLIMRECOGIGATRACKERHIT_H
#define TSLIMRECOGIGATRACKERHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include "TVector3.h"
#include "TSlimRecoVHit.hh"

class TRecoGigaTrackerHit;

class TSlimRecoGigaTrackerHit : public TSlimRecoVHit{

public:
  TSlimRecoGigaTrackerHit() = default;
  explicit TSlimRecoGigaTrackerHit(TRecoGigaTrackerHit *hitReco);
  virtual ~TSlimRecoGigaTrackerHit() = default;

  // setters for members
  void SetIsPileUpHit(Bool_t ispileup)  { fIsPileUpHit = ispileup;       }
  void SetChannelID(Int_t channelID)    { fChannelID   = channelID;      }
  void SetRawTime(Float_t rawtime)      { fRawTime     = rawtime;        }
  void SetTime(Float_t time)            { fTime        = time;           }
  void SetToT(Float_t tot)              { fToT         = tot;            }
  void SetPosition(TVector3 position);

  // getters for members
  Bool_t   GetIsPileUpHit() const { return fIsPileUpHit;            }
  Int_t    GetChannelID()   const { return fChannelID;              }
  Float_t  GetRawTime()     const { return fRawTime;                }
  Float_t  GetTime()        const { return fTime;                   }
  Float_t  GetToT()         const { return fToT;                    }
  TVector3 GetPosition()    const;
  Int_t    GetStationNo()   const;
  Int_t    GetChipID()      const;
  Int_t    GetChipPixelID() const;
  UInt_t   GetColumn()      const;
  UInt_t   GetRow()         const;


  // conversion functions
  virtual void FromReco(TRecoVHit *hitReco);
  virtual void ToReco(TRecoVHit *hitReco);
private:

  Bool_t  fIsPileUpHit = 0;
  Int_t   fChannelID   = 0;
  Float_t fRawTime     = 0.;
  Float_t fTime        = 0.;
  Float_t fToT         = 0.;
  Float_t fPositionX   = 0.;
  Float_t fPositionY   = 0.;

  ClassDef(TSlimRecoGigaTrackerHit, 1)
};
#endif
