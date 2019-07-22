#ifndef TSLIMRECOSPECTROMETERCANDIDATE_H
#define TSLIMRECOSPECTROMETERCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include <TVector3.h>

#include "TSlimRecoVCandidate.hh"

class TRecoSpectrometerCandidate;

class TSlimRecoSpectrometerCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoSpectrometerCandidate() = default;
    explicit TSlimRecoSpectrometerCandidate(TRecoSpectrometerCandidate *candReco);
    virtual ~TSlimRecoSpectrometerCandidate() = default;

    // setters for members
    void SetTime(Float_t time)                       { fTime = time;                      }
    void SetMomentum(Double_t momentum)              { fMomentum = momentum;              }
    void SetMomentumBeforeFit(Double_t momentum)     { fMomentumBeforeFit = momentum;     }
    void SetCharge(Short_t charge)                   { fCharge = charge;                  }
    void SetNChambers(Short_t nchambers)             { fNChambers = nchambers;            }
    void SetMissingChamberID(Short_t ch)             { fMissingChamberID = ch;            }
    void SetSlopeXBeforeMagnet(Double_t slope)       { fSlopeXBeforeMagnet = slope;       }
    void SetSlopeYBeforeMagnet(Double_t slope)       { fSlopeYBeforeMagnet = slope;       }
    void SetPositionXBeforeMagnet(Double_t position) { fPositionXBeforeMagnet = position; }
    void SetPositionYBeforeMagnet(Double_t position) { fPositionYBeforeMagnet = position; }
    void SetSlopeXAfterMagnet(Double_t slope)        { fSlopeXAfterMagnet = slope;        }
    void SetSlopeYAfterMagnet(Double_t slope)        { fSlopeYAfterMagnet = slope;        }
    void SetPositionXAfterMagnet(Double_t position)  { fPositionXAfterMagnet = position;  }
    void SetPositionYAfterMagnet(Double_t position)  { fPositionYAfterMagnet = position;  }
    void SetChi2(Float_t chi2)                       { fChi2 = chi2;                      }
    void SetLeadingTime(Float_t leadingTime)         { fLeadingTime = leadingTime;        }
    void AddHitIndex(Short_t index)                  { fHitsIndexes.emplace_back(index);  }
    void SetCovariance(Int_t i, Int_t j, Double_t value);

    // getters for members
    Float_t GetTime()                            const { return fTime;                  }
    Float_t GetChi2()                            const { return fChi2;                  }
    Float_t GetLeadingTime()                     const { return fLeadingTime;           }
    Double_t GetMomentum()                       const { return fMomentum;              }
    Double_t GetMomentumBeforeFit()              const { return fMomentumBeforeFit;     }
    Short_t GetCharge()                          const { return fCharge;                }
    Short_t GetNChambers()                       const { return fNChambers;             }
    Short_t GetMissingChamberID()                const { return fMissingChamberID;      }
    Double_t GetSlopeXBeforeMagnet()             const { return fSlopeXBeforeMagnet;    }
    Double_t GetSlopeYBeforeMagnet()             const { return fSlopeYBeforeMagnet;    }
    Double_t GetPositionXBeforeMagnet()          const { return fPositionXBeforeMagnet; }
    Double_t GetPositionYBeforeMagnet()          const { return fPositionYBeforeMagnet; }
    Double_t GetSlopeXAfterMagnet()              const { return fSlopeXAfterMagnet;     }
    Double_t GetSlopeYAfterMagnet()              const { return fSlopeYAfterMagnet;     }
    Double_t GetPositionXAfterMagnet()           const { return fPositionXAfterMagnet;  }
    Double_t GetPositionYAfterMagnet()           const { return fPositionYAfterMagnet;  }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;           }
    TVector3 GetPositionBeforeMagnet()           const;
    TVector3 GetPositionAfterMagnet()            const;
    TVector3 GetThreeMomentumBeforeMagnet()      const;
    TVector3 GetThreeMomentumAfterMagnet()       const;

    Double_t GetCovariance(Int_t i, Int_t j)     const;

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    // utility function for index conversion:
    static Int_t GetArrayIndexCovarianceMatrix(Int_t i, Int_t j);

    static constexpr Int_t kCovarianceDimension = 15;
    Short_t  fCharge                = -9999;
    Short_t  fNChambers             = -9999; ///< N(chambers) used to reconstruct the candidate
    Short_t  fMissingChamberID      = -9999; ///< ID of the missing chamber for 3-chamber candidates, -1 otherwise
    Float_t  fTime                  = -9999;
    Float_t  fChi2                  = -9999;
    Float_t  fLeadingTime           = -9999;
    Double_t fMomentum              = -9999;
    Double_t fMomentumBeforeFit     = -9999;
    Double_t fSlopeXBeforeMagnet    = -9999;
    Double_t fSlopeYBeforeMagnet    = -9999;
    Double_t fPositionXBeforeMagnet = -9999;
    Double_t fPositionYBeforeMagnet = -9999;
    Double_t fSlopeXAfterMagnet     = -9999;
    Double_t fSlopeYAfterMagnet     = -9999;
    Double_t fPositionXAfterMagnet  = -9999;
    Double_t fPositionYAfterMagnet  = -9999;
    // covariance values should be stored in std::array<Double_t, 15>
    // however this is not supported as of ROOT version 6.08 (SLC6)
    // therefore C-style array is used
    Double_t fCovariance[kCovarianceDimension] = {0};
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoSpectrometerCandidate, 1)
};


#endif /* TSLIMRECOSPECTROMETERCANDIDATE_H */
