// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-28
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECMUV12ASSOCIATIONOUTPUT_HH
#define INCLUDE_SPECMUV12ASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "TVector3.h"

class SpectrometerMUV12AssociationOutput {

public:

  SpectrometerMUV12AssociationOutput();
  explicit SpectrometerMUV12AssociationOutput(Int_t);
  ~SpectrometerMUV12AssociationOutput() {}

  Int_t    GetTrackID()                  { return fTrackID;                  }
  void     SetTrackID(Int_t val)         { fTrackID = val;                   }

  Int_t    GetMUV1ClusterID()            { return fMUV1ClusterID;            }
  TVector3 GetMUV1ClusterPosition()      { return fMUV1ClusterPosition;      }
  Double_t GetTrackMUV1ClusterDistance() { return fTrackMUV1ClusterDistance; }
  Double_t GetMUV1ClusterEnergy()        { return fMUV1ClusterEnergy;        }
  Double_t GetMUV1ClusterTime()          { return fMUV1ClusterTime;          }

  Int_t    GetMUV2ClusterID()            { return fMUV2ClusterID;            }
  TVector3 GetMUV2ClusterPosition()      { return fMUV2ClusterPosition;      }
  Double_t GetTrackMUV2ClusterDistance() { return fTrackMUV2ClusterDistance; }
  Double_t GetMUV2ClusterEnergy()        { return fMUV2ClusterEnergy;        }
  Double_t GetMUV2ClusterTime()          { return fMUV2ClusterTime;          }

  void SetMUV1ClusterID(Int_t val)               { fMUV1ClusterID = val;            }
  void SetMUV1ClusterPosition(TVector3 val)      { fMUV1ClusterPosition = val;      }
  void SetTrackMUV1ClusterDistance(Double_t val) { fTrackMUV1ClusterDistance = val; }
  void SetMUV1ClusterEnergy(Double_t val)        { fMUV1ClusterEnergy = val;        }
  void SetMUV1ClusterTime(Double_t val)          { fMUV1ClusterTime = val;          }

  void SetMUV2ClusterID(Int_t val)               { fMUV2ClusterID = val;            }
  void SetMUV2ClusterPosition(TVector3 val)      { fMUV2ClusterPosition = val;      }
  void SetTrackMUV2ClusterDistance(Double_t val) { fTrackMUV2ClusterDistance = val; }
  void SetMUV2ClusterEnergy(Double_t val)        { fMUV2ClusterEnergy = val;        }
  void SetMUV2ClusterTime(Double_t val)          { fMUV2ClusterTime = val;          }

  void Clear();
  void Print();

private:

  Int_t    fTrackID;                  ///< ID of the spectrometer track

  Int_t    fMUV1ClusterID;            ///< ID of the associated MUV1 cluster, -1 if no association
  TVector3 fMUV1ClusterPosition;      ///< Position of the associated MUV1 cluster [mm]
  Double_t fTrackMUV1ClusterDistance; ///< Distance from track extrapolation to cluster in MUV1 front plane [mm]
  Double_t fMUV1ClusterEnergy;        ///< Cluster energy [MeV], 0 if no association
  Double_t fMUV1ClusterTime;          ///< Cluster time [ns], 0 if no association

  Int_t    fMUV2ClusterID;            ///< ID of the associated MUV2 cluster, -1 if no association
  TVector3 fMUV2ClusterPosition;      ///< Position of the associated MUV2 cluster [mm]
  Double_t fTrackMUV2ClusterDistance; ///< Distance from track extrapolation to cluster in MUV2 front plane [mm]
  Double_t fMUV2ClusterEnergy;        ///< Cluster energy [MeV], 0 if no association
  Double_t fMUV2ClusterTime;          ///< Cluster time [ns], 0 if no association

};

#endif
