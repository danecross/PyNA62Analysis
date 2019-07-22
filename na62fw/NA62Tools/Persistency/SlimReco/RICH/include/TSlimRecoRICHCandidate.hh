#ifndef TSLIMRECORICHCANDIDATE_H
#define TSLIMRECORICHCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector2.h"
#include "TArrayI.h"
#include "TSlimRecoVCandidate.hh"

class TRecoRICHCandidate;

class TSlimRecoRICHCandidate : public TSlimRecoVCandidate {
  public:
    TSlimRecoRICHCandidate() = default;
    explicit TSlimRecoRICHCandidate(TRecoRICHCandidate *candReco);
    virtual ~TSlimRecoRICHCandidate() {};

    // setters for members
    void SetRingCenter(TVector2 value)        { fRingCenter_X = value.X(); fRingCenter_Y = value.Y(); }
    void SetRingRadius(Float_t value)         { fRingRadius = value;                                  }
    void SetRingChi2(Float_t value)           { fRingChi2 = value;                                    }
    void SetRingTime(Float_t value)           { fRingTime = value;                                    }
    void SetTimeCandidateIndex(Float_t value) { fTimeCandidateIndex = value;                          }

    // setters methods for the iteration fit
    void SetRingCenterSingleRing(TVector2 value)      { fRingCenterSingleRing_X = value.X(); fRingCenterSingleRing_Y = value.Y();           }
    void SetRingCenterErrorSingleRing(TVector2 value) { fRingCenterErrorSingleRing_X = value.X(); fRingCenterErrorSingleRing_Y = value.Y(); }
    void SetRingRadiusSingleRing(Float_t value)       { fRingRadiusSingleRing = value;                                                      }
    void SetRingRadiusErrorSingleRing(Float_t value)  { fRingRadiusErrorSingleRing = value;                                                 }
    void SetRingChi2SingleRing(Float_t value)         { fRingChi2SingleRing = value;                                                        }
    void SetRingTimeSingleRing(Float_t value)         { fRingTimeSingleRing = value;                                                        }
    void SetNIterationsSingleRing(Short_t value)      { fNIterationsSingleRing = value;                                                     }
    void AddHitIndex(Short_t index)                   { fHitIndexes.emplace_back(index);                                                    }
    void AddHitIndexSingleRing(Short_t index)         { fHitIndexesSingleRing.emplace_back(index);                                          }

    // getters for members

    TVector2 GetRingCenter()         const { return TVector2(fRingCenter_X, fRingCenter_Y); }
    Float_t  GetRingRadius()         const { return fRingRadius;                            }
    Float_t  GetRingChi2()           const { return fRingChi2;                              }
    Float_t  GetRingTime()           const { return fRingTime;                              }
    Float_t  GetTime()               const { return fTime;                                  }
    Short_t  GetTimeCandidateIndex() const { return fTimeCandidateIndex;                    }

    // getters methods for the iteration fit
    TVector2 GetRingCenterSingleRing()                    const { return TVector2(fRingCenterSingleRing_X, fRingCenterErrorSingleRing_Y);      }
    TVector2 GetRingCenterErrorSingleRing()               const { return TVector2(fRingCenterErrorSingleRing_X, fRingCenterErrorSingleRing_Y); }
    Float_t  GetRingRadiusSingleRing()                    const { return fRingRadiusSingleRing;                                                }
    Float_t  GetRingRadiusErrorSingleRing()               const { return fRingRadiusErrorSingleRing;                                           }
    Float_t  GetRingChi2SingleRing()                      const { return fRingChi2SingleRing;                                                  }
    Float_t  GetRingTimeSingleRing()                      const { return fRingTimeSingleRing;                                                  }
    Float_t  GetNHits()                                   const { return fHitIndexes.size();                                                   }
    Float_t  GetNHitsSingleRing()                         const { return fHitIndexesSingleRing.size();                                         }
    Float_t  GetNIterationsSingleRing()                   const { return fNIterationsSingleRing;                                               }
    const std::vector<Short_t>& GetHitIndexes()           const { return fHitIndexes;                                                          }
    const std::vector<Short_t>& GetHitIndexesSingleRing() const { return fHitIndexesSingleRing;                                                }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);

  private:

    Short_t  fTimeCandidateIndex =    -1;  ///< index of the time candidate from which the ring candidate has been reconstructed
    Float_t  fRingRadius         =  -10.;  ///< radius of the ring as obtained from the fit
    Float_t  fRingChi2           =  -10.;  ///< chi2 of the fit to the ring
    Float_t  fRingTime           = 9999.;  ///< time of the ring candidate
    Float_t  fRingCenter_X       =  500.;  ///< center position of the ring as obtained from the fit
    Float_t  fRingCenter_Y       =  500.;  ///< center position of the ring as obtained from the fit
    Float_t  fTime               = 9999.;

    // variables for the iteration fit
    Short_t  fNIterationsSingleRing       = -1;
    Float_t  fRingRadiusSingleRing        = -9999.;  ///< radius of the single ring ring
    Float_t  fRingRadiusErrorSingleRing   = -9999.;
    Float_t  fRingChi2SingleRing          = -9999.;  ///< chi2 of the single ring fit
    Float_t  fRingTimeSingleRing          = -9999.;  ///< time of the average over all hits in the single ring fit
    Float_t  fRingCenterSingleRing_X      = -9999.;  ///< center position of the ring obtained in the single ring the fit
    Float_t  fRingCenterSingleRing_Y      = -9999.;  ///< center position of the ring obtained in the single ring the fit
    Float_t  fRingCenterErrorSingleRing_X = -9999.;
    Float_t  fRingCenterErrorSingleRing_Y = -9999.;
    std::vector<Short_t> fHitIndexes;
    std::vector<Short_t> fHitIndexesSingleRing;

    ClassDef(TSlimRecoRICHCandidate, 1)
};

#endif /* TSLIMRECORICHCANDIDATE_H */
