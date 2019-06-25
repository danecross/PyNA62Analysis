#ifndef TSLIMRECOGIGATRACKERCANDIDATE_H
#define TSLIMRECOGIGATRACKERCANDIDATE_H

#include <RtypesCore.h> // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector3.h"
#include "TSlimRecoVCandidate.hh"

class TRecoGigaTrackerCandidate;

class TSlimRecoGigaTrackerCandidate : public TSlimRecoVCandidate
{
public:
  TSlimRecoGigaTrackerCandidate() = default;
  explicit TSlimRecoGigaTrackerCandidate(TRecoGigaTrackerCandidate *candReco);
  virtual ~TSlimRecoGigaTrackerCandidate() = default;
  // setters for members
  void SetTime(Float_t hittime)                           { fTime                 = hittime;     }
  void SetTimeError(Float_t timeerror)                    { fTimeError            = timeerror;   }
  void SetMomentum(TVector3 momentum);
  void SetChi2(Float_t chi2)                              { fChi2                 = chi2;        }
  void SetChi2X(Float_t chi2x)                            { fChi2X                = chi2x;       }
  void SetChi2Y(Float_t chi2y)                            { fChi2Y                = chi2y;       }
  void SetChi2Time(Float_t chi2time)                      { fChi2Time             = chi2time;    }
  void SetTimeStation(Int_t station, Float_t timestation) { fTimeStation[station] = timestation; }
  void SetPosition(Int_t station, TVector3 position);
  void SetCovariance(Int_t i, Int_t j, Double_t value);
  void AddHitIndex(Short_t index)                         { fHitsIndexes.emplace_back(index);    }

  // getters for members
  Float_t GetTime()                            const { return fTime;                 }
  Float_t GetTimeError()                       const { return fTimeError;            }
  TVector3 GetMomentum()                       const;
  Float_t GetChi2()                            const { return fChi2;                 }
  Float_t GetChi2X()                           const { return fChi2X;                }
  Float_t GetChi2Y()                           const { return fChi2Y;                }
  Float_t GetChi2Time()                        const { return fChi2Time;             }
  Float_t GetTimeStation(Int_t station)        const { return fTimeStation[station]; }
  TVector3 GetPosition(Int_t station)          const;
  Int_t GetNHits()                             const { return fHitsIndexes.size();   }
  const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;          }
  Int_t GetType()                              const { return 123;                   }

  Double_t GetCovariance(Int_t i, Int_t j)     const;

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);

private:
  // utility function for index conversion:
  static Int_t GetArrayIndexCovarianceMatrix(Int_t i, Int_t j);

  static constexpr Int_t kCovarianceDimension = 15;
  Float_t fTime           = 0;
  Float_t fTimeError      = 0;
  Float_t fMomentumX      = 0;
  Float_t fMomentumY      = 0;
  Float_t fMomentumZ      = 0;
  Float_t fChi2           = 0;
  Float_t fChi2X          = 0;
  Float_t fChi2Y          = 0;
  Float_t fChi2Time       = 0;
  Float_t fTimeStation[3] = {0, 0, 0};
  Float_t fPositionX[3]   = {0, 0, 0};
  Float_t fPositionY[3]   = {0, 0, 0};
  // covariance values should be stored in std::array<Double_t, 15>
  // however this is not supported as of ROOT version 6.08 (SLC6)
  // therefore C-style array is used
  Double_t fCovariance[kCovarianceDimension] = {0};
  std::vector<Short_t> fHitsIndexes;

  ClassDef(TSlimRecoGigaTrackerCandidate, 1)
};

#endif /* TSLIMRECoGigaTrackerCANDIDATE_H */
