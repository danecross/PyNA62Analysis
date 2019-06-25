#ifndef TSlimRecoLKrCandidate_H
#define TSlimRecoLKrCandidate_H
#include <RtypesCore.h>
#include "TSlimRecoVCandidate.hh"
#include "TClonesArray.h"
#include "TSlimRecoLKrHit.hh"

class TRecoLKrCandidate;

class TSlimRecoLKrCandidate : public TSlimRecoVCandidate {

public:

  TSlimRecoLKrCandidate()  = default;
  explicit TSlimRecoLKrCandidate(TRecoLKrCandidate *candReco);
  virtual ~TSlimRecoLKrCandidate() = default;

  void SetIDSeed(Int_t idseed)                         { fIdSeed = idseed;                       }
  void SetNCells(Int_t nCells)                         { fNCells = nCells;                       }
  void SetClusterTime(Float_t time)                    { fTime = time;                           }
  void SetClusterEnergy(Float_t clusterEnergy)         { fClusterEnergy = clusterEnergy;         }
  void SetClusterX(Float_t clusterX)                   { fClusterX = clusterX;                   }
  void SetClusterY(Float_t clusterY)                   { fClusterY = clusterY;                   }
  void SetClusterRMSX(Float_t clusterRmsx)             { fClusterRMSX = clusterRmsx;             }
  void SetClusterRMSY(Float_t clusterRmsy)             { fClusterRMSY = clusterRmsy;             }
  void SetClusterDDeadCell(Float_t clusterDDeadCell)   { fClusterDDeadCell = clusterDDeadCell;   }
  void SetClusterSeedEnergy(Float_t clusterSeedEnergy) { fClusterSeedEnergy = clusterSeedEnergy; }
  void SetCluster77Energy(Float_t cluster77Energy)     { fCluster77Energy = cluster77Energy;     }

  Int_t   GetIdSeed()            const { return fIdSeed;            }
  Int_t   GetNCells()            const { return fNCells;            }
  Float_t GetClusterTime()       const { return fTime;              }
  Float_t GetClusterEnergy()     const { return fClusterEnergy;     }
  Float_t GetClusterX()          const { return fClusterX;          }
  Float_t GetClusterY()          const { return fClusterY;          }
  Float_t GetClusterRMSX()       const { return fClusterRMSX;       }
  Float_t GetClusterRMSY()       const { return fClusterRMSY;       }
  Float_t GetClusterDDeadCell()  const { return fClusterDDeadCell;  }
  Float_t GetClusterSeedEnergy() const { return fClusterSeedEnergy; }
  Float_t GetCluster77Energy()   const { return fCluster77Energy;   }

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);
private:
  Int_t   fIdSeed            = 0.;
  Int_t   fNCells            = 0;
  Float_t fTime              = 0.;
  Float_t fClusterEnergy     = 0.;
  Float_t fClusterX          = 9999.;
  Float_t fClusterY          = 9999.;
  Float_t fClusterRMSX       = 0.;
  Float_t fClusterRMSY       = 0.;
  Float_t fClusterDDeadCell  = 0.;
  Float_t fClusterSeedEnergy = 0.;
  Float_t fCluster77Energy   = 0.;

  ClassDef(TSlimRecoLKrCandidate, 1)
};
#endif
