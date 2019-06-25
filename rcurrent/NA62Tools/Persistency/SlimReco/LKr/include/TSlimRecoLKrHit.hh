#ifndef TSlimRecoLKrHit_H
#define TSlimRecoLKrHit_H
#include <RtypesCore.h>
#include "TVector3.h"
#include "NA62Global.hh"
#include "TSlimRecoVHit.hh"

class TRecoLKrHit;

class TSlimRecoLKrHit : public TSlimRecoVHit {
    static constexpr Float_t LKrCellSize = 19.7383881; // mm

  public:
    TSlimRecoLKrHit() = default;
    explicit TSlimRecoLKrHit(TRecoLKrHit *hitReco);
    virtual ~TSlimRecoLKrHit() = default;

    void SetChannelID(Int_t val)  { fChannelID = val; }
    void SetTime(Float_t val)     { fTime = val;      }
    void SetPedestal(Float_t val) { fPedestal = val;  }
    void SetEnergy(Double_t val)  { fEnergy = val;    }

    Int_t GetChannelID()  const { return fChannelID; }
    Float_t GetTime()     const { return fTime;      }
    Float_t GetPedestal() const { return fPedestal;  }
    Double_t GetEnergy()  const { return fEnergy;    }

    // variables retrieved from Slim Persistency
    Int_t GetXCellID()      const;
    Int_t GetYCellID()      const;
    Int_t GetCPDID()        const;
    Int_t GetCPDChannelID() const;
    TVector3 GetPosition()  const { return TVector3((GetXCellID()-0.5*127)*LKrCellSize,(GetYCellID()-0.5*127)*LKrCellSize,0);}

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);

  private:
    Int_t     fChannelID  = 0;
    Float_t   fTime       = 0;
    Float_t   fPedestal   = 0;
    Double_t  fEnergy     = 0;

    ClassDef(TSlimRecoLKrHit, 1)
};
#endif




