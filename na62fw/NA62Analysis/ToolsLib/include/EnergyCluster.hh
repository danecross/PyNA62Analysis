// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#ifndef ENERGYCLUSTER_HH
#define ENERGYCLUSTER_HH

#include "TMath.h"
#include "TVector2.h"
#include "TVector3.h"
#include <iostream>
#include <vector>

#include "TRecoLKrCandidate.hh"
#include "LKrSpectrometerAssociationOutput.hh"
#include "ProtoParticle.hh"

class DownstreamTrack;

class EnergyCluster  : public ProtoParticle {

public:

  EnergyCluster();
  EnergyCluster(Int_t,TRecoLKrCandidate*);
  ~EnergyCluster() {}
  void Clear();
  void Print() const;

  void SetClusterID(Int_t val)  { fClusterID = val;             }
  Int_t GetClusterID() const    { return fClusterID;            }

  void SetIsElectromagnetic(Bool_t val) { fIsElectromagnetic = val;}
  Bool_t GetIsElectromagnetic() const   { return fIsElectromagnetic;}

  void SetIsIsolated(Bool_t val) { fIsIsolated = val; }
  Bool_t GetIsIsolated() const   { return fIsIsolated;}

  void SetIsInLKrAcceptance(Bool_t val) { fIsInLKrAcceptance = val; }
  Bool_t GetIsInLKrAcceptance() const   { return fIsInLKrAcceptance;}

  // Pointer to the LKr candidate
  void SetLKrCandidate(TRecoLKrCandidate* val) { fLKrCandidate = val; }
  TRecoLKrCandidate* GetLKrCandidate() const { return fLKrCandidate; }

  // most important cluster properties provided here
  inline double GetClusterX() const { return fLKrCandidate->GetClusterX();}
  inline double GetClusterY() const { return fLKrCandidate->GetClusterY();}
  inline double GetEnergy() const { return fLKrCandidate->GetClusterEnergy();}
  inline double GetClusterEnergy() const { return fLKrCandidate->GetClusterEnergy();}
  inline double GetTime() const { return fLKrCandidate->GetTime();}
  inline double GetClusterDDeadCell() const { return fLKrCandidate->GetClusterDDeadCell();}

  ///////////////////////////
  // Subdetector associations

  // Spectrometer
  void SetSpectrometerAssociation(const LKrSpectrometerAssociationOutput &val) { fSpectrometerAssociation = val;  }
  LKrSpectrometerAssociationOutput  GetSpectrometerAssociation()        { return fSpectrometerAssociation; }
  Bool_t SpectrometerAssociationExists() const { return fSpectrometerAssociation.isAssociated();           }
  UInt_t GetNSpectrometerAssociationRecords() const { return fSpectrometerAssociation.GetNAssociationRecords(); }
  void AddTrackForWhichThisClusterIsBestMatch(DownstreamTrack* pDownstreamTrk){ fTracksForWhichThisClusterIsBestMatch.push_back(pDownstreamTrk);}
  DownstreamTrack* GetTrackForWhichThisClusterIsBestMatch(UInt_t iTrack){ if(iTrack<fTracksForWhichThisClusterIsBestMatch.size()) return fTracksForWhichThisClusterIsBestMatch[iTrack]; else return 0; };
  std::vector<DownstreamTrack*> GetTracksForWhichThisClusterIsBestMatch(){ return fTracksForWhichThisClusterIsBestMatch; };

  private:

  Int_t    fClusterID;              ///< ID of the LKr cluster
  TRecoLKrCandidate* fLKrCandidate; ///< Pointer to the LKr Candidate
  Bool_t   fIsElectromagnetic;      ///< Is the LKr cluster electromagnetic
  Bool_t   fIsIsolated;             ///< Is the LKr cluster isolated from all other clusters
  Bool_t   fIsInLKrAcceptance;      ///< Is the LKr cluster in geometrical acceptance of LKr

  LKrSpectrometerAssociationOutput fSpectrometerAssociation;  ///< Output of LKrSpectrometerAssociation

  //DownstreamTrack best matches
  std::vector<DownstreamTrack*> fTracksForWhichThisClusterIsBestMatch;    ///< Container of the DownstreamTracks for which this cluster is the best match

};

#endif
