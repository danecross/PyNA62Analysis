#ifndef TSLIMRECOMUVCANDIDATE_H
#define TSLIMRECOMUVCANDIDATE_H

#include "TSlimRecoVCandidate.hh"
#include "NA62Global.hh"

#include <RtypesCore.h>
#include <TVector2.h>
#include <TMath.h>

#include <vector>

class TSlimRecoMUVCandidate : public TSlimRecoVCandidate {

public:
	TSlimRecoMUVCandidate ()          { Clear(); }
	virtual ~TSlimRecoMUVCandidate () {}

	virtual void Clear(Option_t *option="");

	//Setters
	void SetChargeH(Float_t chargeH) { fChargeH = chargeH;               }
	void SetChargeV(Float_t chargeV) { fChargeV = chargeV;               }
	void SetEnergy(Float_t energy)   { fEnergy = energy;                 }
	void SetEnergyH(Float_t energyH) { fEnergyH = energyH;               }
	void SetEnergyV(Float_t energyV) { fEnergyV = energyV;               }
	void AddHitIndex(Short_t index)  { fHitsIndexes.emplace_back(index); }
	void SetHitsIndexes(Int_t Nhits, Int_t *hitsIndexes) {
	    fHitsIndexes.clear();
		fHitsIndexes.resize(Nhits);
		for (int i=0; i<Nhits; i++) fHitsIndexes[i] = hitsIndexes[i];
	}

	void SetIndexH(Int_t indexH, Int_t sideH)               { fIndexH = 100*sideH + indexH;                   }
	void SetIndexV(Short_t indexV, Int_t sideV)             { fIndexV = 100*sideV + indexV;                   }
	void SetInnerEnergyH(Float_t innerEnergyH)              { fInnerEnergyH = innerEnergyH;                   }
	void SetInnerEnergyV(Float_t innerEnergyV)              { fInnerEnergyV = innerEnergyV;                   }
	void SetSeedEnergyH(Float_t seedEnergyH)                { fSeedEnergyH = seedEnergyH;                     }
	void SetSeedEnergyV(Float_t seedEnergyV)                { fSeedEnergyV = seedEnergyV;                     }
	void SetSeedIndexes(Int_t seedIndexH, Int_t seedIndexV) { fSeedIndexes = 100*seedIndexV+seedIndexH;       }
	void SetShowerWidthH(Float_t showerWidthH)              { fShowerWidthH = showerWidthH;                   }
	void SetShowerWidthV(Float_t showerWidthV)              { fShowerWidthV = showerWidthV;                   }
	void SetTimeH(Float_t timeH)                            { fTimeH = timeH;                                 }
	void SetTimeV(Float_t timeV)                            { fTimeV = timeV;                                 }
	void SetTimeSigmaH(Float_t timeSigmaH)                  { fTimeSigmaH = timeSigmaH;                       }
	void SetTimeSigmaV(Float_t timeSigmaV)                  { fTimeSigmaV = timeSigmaV;                       }
	void SetPosition (Float_t x, Float_t y)                 { fPosition[0] = x; fPosition[1] = y;             }
	void SetPosition (TVector2 pos)                         { fPosition[0] = pos.X(); fPosition[1] = pos.Y(); }

	//Getters
	Float_t GetCharge()                   const { return 0.5*(fChargeH + fChargeV);                                            }
	Float_t GetChargeH()                  const { return fChargeH;                                                             }
	Float_t GetChargeV()                  const { return fChargeV;                                                             }
	Float_t GetEnergy()                   const { return fEnergy;                                                              }
	Float_t GetEnergyH()                  const { return fEnergyH;                                                             }
	Float_t GetEnergyV()                  const { return fEnergyV;                                                             }
	Int_t GetNHits()                      const { return fHitsIndexes.size();                                                  }
	std::vector<Short_t> GetHitsIndexes() const { return fHitsIndexes;                                                         }
	Short_t GetIndexH()                   const { return fIndexH;                                                              }
	Short_t GetIndexV()                   const { return fIndexV;                                                              }
	Float_t GetInnerEnergy()              const { return 0.5*(fInnerEnergyH+fInnerEnergyV);                                    }
	Float_t GetInnerEnergyH()             const { return fInnerEnergyH;                                                        }
	Float_t GetInnerEnergyV()             const { return fInnerEnergyV;                                                        }
	TVector2 GetPosition()                const { return TVector2(fPosition[0],fPosition[1]);                                  }
	Float_t GetSeedEnergy()               const { return 0.5*(fSeedEnergyH+fSeedEnergyV);                                      }
	Float_t GetSeedEnergyH()              const { return fSeedEnergyH;                                                         }
	Float_t GetSeedEnergyV()              const { return fSeedEnergyV;                                                         }
	Short_t GetSeedIndexH()               const { return fSeedIndexes%100;                                                     }
	Short_t GetSeedIndexV()               const { return static_cast<Short_t>(fSeedIndexes/100);                               }
	Float_t GetShowerWidth()              const { return TMath::Sqrt(fShowerWidthH*fShowerWidthH+fShowerWidthV*fShowerWidthV); }
	Float_t GetShowerWidthH()             const { return fShowerWidthH;                                                        }
	Float_t GetShowerWidthV()             const { return fShowerWidthV;                                                        }
	Float_t GetTime()                     const { return 0.5*(fTimeH+fTimeV);                                                  }
	Float_t GetTimeH()                    const { return fTimeH;                                                               }
	Float_t GetTimeV()                    const { return fTimeV;                                                               }
	Float_t GetTimeSigmaHorizontal()      const { return fTimeSigmaH;                                                          }
	Float_t GetTimeSigmaVertical()        const { return fTimeSigmaV;                                                          }

	//Replicating Standard Persistency
	Int_t GetHorizontalIndex()   const { return fIndexH/100;     }
	Int_t GetVerticalIndex()     const { return fIndexV/100;     }
	Int_t GetHorizontalChannel() const { return fIndexH%100;     }
	Int_t GetVerticalChannel()   const { return fIndexV%100;     }
	Float_t GetPlaneTimeDiff()   const { return fTimeH - fTimeV; }
	Float_t GetX()               const { return fPosition[0];    }
	Float_t GetY()               const { return fPosition[1];    }
	Int_t GetQuadrant()          const;

protected:
	Short_t fIndexH, fIndexV;
	Short_t fSeedIndexes;
	Float_t fTimeH, fTimeV;
	Float_t fEnergy, fEnergyH, fEnergyV;
	Float_t fInnerEnergyH, fInnerEnergyV;
	Float_t fSeedEnergyH, fSeedEnergyV;
	Float_t fChargeH, fChargeV;
	Float_t fShowerWidthH, fShowerWidthV;
	Float_t fTimeSigmaH, fTimeSigmaV;
	Float_t fPosition[2];

	std::vector<Short_t> fHitsIndexes;

	ClassDef(TSlimRecoMUVCandidate, 1)
};


#endif
