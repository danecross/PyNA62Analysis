#ifndef TSLIMRECOSPECTROMETERHIT_H
#define TSLIMRECOSPECTROMETERHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoSpectrometerHit;

class TSlimRecoSpectrometerHit : public TSlimRecoVHit
{
public:
    TSlimRecoSpectrometerHit() = default;
    explicit TSlimRecoSpectrometerHit(TRecoSpectrometerHit *hitReco);
    virtual ~TSlimRecoSpectrometerHit() = default;

    // setters for members
    void SetChannelID(Short_t channelID) { fChannelID = channelID; }
    void SetTime(Float_t time)           { fTime = time;           }
    void SetTimeWidth(Float_t width)     { fTimeWidth = width;     }
    void SetWireDistance(Float_t dist)   { fWireDistance = dist;   }

    // getters for members
    Short_t GetChannelID()    const { return fChannelID;    }
    Float_t GetTime()         const { return fTime;         }
    Float_t GetTimeWidth()    const { return fTimeWidth;    }
    Float_t GetWireDistance() const { return fWireDistance; }

    // getters for variables from Standard Persistency interface
    Int_t GetChamberID()    const { return fChannelID/1952;             }
    Int_t GetViewID()       const { return (fChannelID%1952)/488;       }
    Int_t GetHalfViewID()   const { return (fChannelID%488)/244;        }
    Int_t GetPlaneID()      const { return (fChannelID%244)/122;        }
    Int_t GetStrawID()      const { return fChannelID%122;              }
    Int_t GetEdgeStatus()   const { return (Int_t)(fTimeWidth >= -500); }
    Double_t GetDriftTime() const { return fTime;                       }
    Double_t GetRadius()    const { return fWireDistance;               }
    Int_t GetNUsedDigis()   const { return 1;
    }
    // interface for useless variables from Standard Persistency
    Int_t GetSingle()                 const { return 0; }
    Int_t GetID()                     const { return 0; }
    Int_t GetMCID()                   const { return 0; }
    Int_t GetTDCID()                  const { return 0; }
    Int_t GetRecoID()                 const { return 0; }
    Double_t GetEnergy()              const { return 0; }
    Double_t GetWireAverageDistance() const { return 0; }
    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID    = -9999;
    Float_t fTime         = -9999;
    Float_t fTimeWidth    = -9999;
    Float_t fWireDistance = -9999;

    ClassDef(TSlimRecoSpectrometerHit, 1)
};

#endif /* TSLIMRECOSPECTROMETERHIT_H */
