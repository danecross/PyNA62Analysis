// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHCandidate_H
#define TRecoRICHCandidate_H

#include "TRecoVCandidate.hh"
#include "TArrayI.h"

class TRecoRICHCandidate : public TRecoVCandidate {

  public:

    TRecoRICHCandidate();
    ~TRecoRICHCandidate(){};

    void Clear(Option_t* = "");

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }

    void UpdateTime();
    void UpdateTime(Double_t);

    Bool_t               GetIsSelected()                                    { return fIsSelected;                   };
    void                 SetIsSelected(Bool_t value)                        { fIsSelected = value;                  };
    Double_t             GetDeltaTimeClosestCandidate()                     { return fDeltaTimeClosestCandidate;    };
    void                 SetDeltaTimeClosestCandidate(Double_t val)         { fDeltaTimeClosestCandidate = val;     };
    Int_t                GetNHitsClosestCandidate()                         { return fNHitsClosestCandidate;        };
    void                 SetNHitsClosestCandidate(Int_t val)                { fNHitsClosestCandidate = val;         };

    TVector2             GetRingCenter()                                    { return fRingCenter;                   };
    void                 SetRingCenter(TVector2 value)                      { fRingCenter = value;                  };
    Double_t             GetRingRadius()                                    { return fRingRadius;                   };
    void                 SetRingRadius(Double_t value)                      { fRingRadius = value;                  };
    Double_t             GetRingChi2()                                      { return fRingChi2;                     };
    void                 SetRingChi2(Double_t value)                        { fRingChi2 = value;                    };
    Double_t             GetRingTime()                                      { return fRingTime;                     };
    void                 SetRingTime(Double_t value)                        { fRingTime = value;                    };
    Int_t                GetTimeCandidateIndex()                            { return fTimeCandidateIndex;           };
    void                 SetTimeCandidateIndex(Double_t value)              { fTimeCandidateIndex = value;          };

  // methods for the iteration fit
    TVector2             GetRingCenterSingleRing()                                    { return fRingCenterSingleRing;                   };
    void                 SetRingCenterSingleRing(TVector2 value)                      { fRingCenterSingleRing = value;                  };
    TVector2             GetRingCenterErrorSingleRing()                                    { return fRingCenterErrorSingleRing;                   };
    void                 SetRingCenterErrorSingleRing(TVector2 value)                      { fRingCenterErrorSingleRing = value;                  };
    Double_t             GetRingRadiusSingleRing()                                    { return fRingRadiusSingleRing;                   };
    void                 SetRingRadiusSingleRing(Double_t value)                      { fRingRadiusSingleRing = value;                  };
    Double_t             GetRingRadiusErrorSingleRing()                                    { return fRingRadiusErrorSingleRing;                   };
    void                 SetRingRadiusErrorSingleRing(Double_t value)                      { fRingRadiusErrorSingleRing = value;                  };
    Double_t             GetRingChi2SingleRing()                                      { return fRingChi2SingleRing;                     };
    void                 SetRingChi2SingleRing(Double_t value)                        { fRingChi2SingleRing = value;                    };
    Double_t             GetRingTimeSingleRing()                                      { return fRingTimeSingleRing;                     };
    void                 SetRingTimeSingleRing(Double_t value)                        { fRingTimeSingleRing = value;                    };
    Int_t*               GetHitIndexesSingleRing()                                    { return fHitIndexesSingleRing.GetArray();                     };
    void                 SetHitIndexesSingleRing(Int_t *value, Int_t NHits)           
                         { fHitIndexesSingleRing.Set(NHits); for(Int_t iHit=0; iHit<NHits; iHit++) fHitIndexesSingleRing[iHit] = value[iHit];  };
    Double_t             GetNHitsSingleRing()                                         { return fNHitsSingleRing;                     };
    void                 SetNHitsSingleRing(Double_t value)                           { fNHitsSingleRing = value;                    };
    Double_t             GetNIterationsSingleRing()                                      { return fNIterationsSingleRing;                     };
    void                 SetNIterationsSingleRing(Double_t value)                        { fNIterationsSingleRing = value;                    };

  private:

    Bool_t fIsSelected; //!  Transient data member
    Double_t fDeltaTimeClosestCandidate; //!  Transient data member
    Int_t fNHitsClosestCandidate; //!  Transient data member

    TVector2 fRingCenter;             ///< center position of the ring as obtained from the fit
    Double_t fRingRadius;             ///< radius of the ring as obtained from the fit
    Double_t fRingChi2;               ///< chi2 of the fit to the ring
    Double_t fRingTime;               ///< time of the ring candidate
    Int_t    fTimeCandidateIndex;     ///< index of the time candidate from which the ring candidate has been reconstructed 

  // variables for the iteration fit
    TVector2 fRingCenterSingleRing;             ///< center position of the ring obtained in the single ring the fit
    TVector2 fRingCenterErrorSingleRing; 
    Double_t fRingRadiusSingleRing;             ///< radius of the single ring ring
    Double_t fRingRadiusErrorSingleRing;
    Double_t fRingChi2SingleRing;               ///< chi2 of the single ring fit   
    Double_t fRingTimeSingleRing;               ///< time of the average over all hits in the single ring fit
    Int_t fNHitsSingleRing;
    Int_t fNIterationsSingleRing;
    TArrayI fHitIndexesSingleRing;

    ClassDef(TRecoRICHCandidate,1);
};
#endif
