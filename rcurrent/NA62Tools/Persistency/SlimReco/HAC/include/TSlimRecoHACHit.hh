#ifndef TSLIMRECOHACHIT_H
#define TSLIMRECOHACHIT_H

#include "Rtypes.h"

#include "TSlimRecoVHit.hh"

class TRecoHACHit;

class TSlimRecoHACHit : public TSlimRecoVHit {
public:
    TSlimRecoHACHit();
    explicit TSlimRecoHACHit(TRecoHACHit *hitReco);
    virtual ~TSlimRecoHACHit() = default;

    //Setters
    void SetChannelID(Short_t channelID)             { fChannelID = channelID;        }
    void SetChargeModuleSection(Float_t charge)      { fChargeModuleSection = charge; }
    void SetTime(Float_t time)                       { fTime = time;                  }
    void SetLeadingEdge(Short_t iThr, Float_t time);
    void SetTrailingEdge(Short_t iThr, Float_t time);

    //Getters
    Int_t   GetSiPMID()                  const;
    Int_t   GetModuleID()                const;
    Short_t GetChannelID()               const { return fChannelID;                                                 }
    Float_t GetChargeModuleSection()     const { return fChargeModuleSection;                                       }
    Float_t GetTime()                    const { return fTime;                                                      }
    Float_t GetLeadingEdge(Short_t iThr) const {
        return (iThr >= 0 && iThr < 4) ? fLeadingEdge[iThr] : -999.999;
    }
    Float_t GetTrailingEdge(Short_t iThr) const {
        return (iThr >= 0 && iThr < 4) ? fTrailingEdge[iThr] : -999.999;
    };

    //Getters for standard persistency class
    Int_t GetEdgeMask() const;
    Double_t GetToTSum() const;

    //Methods to convert to and from Standard persistency
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID;
    Float_t fTime;
    Float_t fChargeModuleSection; //Cannot be computed outside HACReconstruction
    Float_t fLeadingEdge[4];
    Float_t fTrailingEdge[4];

    ClassDef(TSlimRecoHACHit, 1)
};

#endif /* TSLIMRECOHACHIT_HH */

