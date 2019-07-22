// --------------------------------------------------------------
// History:
//
// 2019-04-12 T. Spadaro (tommaso.spadaro@lnf.infn.it) and S. Martellotti (silvia.martellotti@lnf.infn.it)
//
// --------------------------------------------------------------
#ifndef TSlimRecoLAVCandidate_H
#define TSlimRecoLAVCandidate_H

#include "TVector3.h"
#include "TSlimRecoVCandidate.hh"

class TRecoLAVCandidate;


class TSlimRecoLAVCandidate : public TSlimRecoVCandidate {

public:

  TSlimRecoLAVCandidate();
  explicit TSlimRecoLAVCandidate(TRecoLAVCandidate* candReco);
  virtual ~TSlimRecoLAVCandidate() { fHitsIndexes.clear(); }

  void SetAlgorithm(Short_t val)                 { fAlgorithm = val;                                          }
  void SetClusterType(Short_t val)               { fClusterType = val;                                        } // Classification of the cluster: MIP, Shower, Unknown
  void SetNHitsPerLayer(Int_t ilay, Short_t val) { if (ilay<0 || ilay>=5) return; fNHitsPerLayer[ilay] = val; }
  void SetTime(Float_t val)                      { fTime = val;                                               }
  void SetEnergy(Float_t val)                    { fEnergy = val;                                             }
  void SetZUnweightedError(Float_t val)          { fZUnweightedError = val;                                   }
  void SetPhiUnweightedError(Float_t val)        { fPhiUnweightedError = val;                                 }
  void SetZWeightedError(Float_t val)            { fZWeightedError = val;                                     }
  void SetPhiWeightedError(Float_t val)          { fPhiWeightedError = val;                                   }
  void SetPosition(TVector3 val)                 { fPosition = val;                                           } // Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average
  void SetWeightedPosition(TVector3 val)         { fWeightedPosition = val;                                   } // Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average
  void AddHitIndex(Short_t index)                { fHitsIndexes.emplace_back(index);                          }

  Short_t GetAlgorithm()                       const { return fAlgorithm;                                                                                }
  Short_t GetClusterType()                     const { return fClusterType;                                                                              }
  Short_t GetNHitsPerLayer(Int_t ilay)         const { if (ilay<0 || ilay>=5) return 0; return fNHitsPerLayer[ilay];                                     }
  Float_t GetTime()                            const { return fTime;                                                                                     }
  Float_t GetEnergy()                          const { return fEnergy;                                                                                   }
  Float_t GetZUnweightedError()                const { return fZUnweightedError;                                                                         }
  Float_t GetPhiUnweightedError()              const { return fPhiUnweightedError;                                                                       }
  Float_t GetZWeightedError()                  const { return fZWeightedError;                                                                           }
  Float_t GetPhiWeightedError()                const { return fPhiWeightedError;                                                                         }
  TVector3 GetPosition()                       const { return fPosition;                                                                                 }
  TVector3 GetWeightedPosition()               const { return fWeightedPosition;                                                                         }
  Int_t GetNHits()                             const { return fNHitsPerLayer[0]+fNHitsPerLayer[1]+fNHitsPerLayer[2]+fNHitsPerLayer[3]+fNHitsPerLayer[4]; }
  const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;                                                                              }
  Double_t GetCentroidErrorMatrix(TVector3 position, Double_t sigmaPhi, Double_t sigmaZ, Double_t SigmaR, Int_t i1, Int_t i2) const;

  // Methods not implemented at the moment
  // Bool_t TVCandidate::AddHit(Int_t  )
  // void TRecoLAVCandidate::Clear(Option_t * = "" )
  // TRecoVHit * TRecoVCandidate::GetHit(Int_t iHit )
  // void TVCandidate::Merge(TVCandidate *  )
  // void TVCandidate::RemoveHit(Int_t  )
  // void TVCandidate::SetEvent(TDetectorVEvent * value ) [inline]

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);
private:
  Short_t fAlgorithm;  ///< =0 if cluster is done by grouping adjacent blocks; =1 if cluster is done by grouping blocks close in phi and time (so-called "tracking"); =2 if no algo is passed
  Short_t fClusterType;  ///<  =1 for MIP cluster (>=2 rows hit with 1 hit per row); =2 for showerLike (>=2 rows hit with > 1 hit per row in at least one row); =0 else
  Short_t fNHitsPerLayer[5];   ///< Number of hits per layer

  Float_t fTime;
  Float_t fEnergy;             ///< Total cluster energy (at the moment, in fact, it is the total charge)
  Float_t fZUnweightedError;   ///< Error on the unweighted average of the z coordinate
  Float_t fPhiUnweightedError; ///< Error on the unweighted average of the azimuthal angle
  Float_t fZWeightedError;     ///< Error on the energy-weighted average of the z coordinate
  Float_t fPhiWeightedError;   ///< Error on the energy-weighted average of the azimuthal angle

  TVector3 fPosition;           ///< Cluster centroid position: no weight is used while averaging fired block positions
  TVector3 fWeightedPosition;   ///< Cluster centroid position: the block energy (in fact, at the moment, the charge) is used while averaging fired block positions

  std::vector<Short_t> fHitsIndexes;

  ClassDef(TSlimRecoLAVCandidate, 1)
};
#endif
