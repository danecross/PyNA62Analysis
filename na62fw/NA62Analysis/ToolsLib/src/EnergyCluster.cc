// ---------------------------------------------------------
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-05-15
//  - added isolation, LKr acceptance
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

/// \class EnergyCluster
/// \Brief
/// A container for the results of LKr cluster association to the other subdetectors
/// \EndBrief
/// \Detailed
/// Contains LKr cluster info, and the results of cluster association to Spectrometer candidates
/// (including pointers to associated candidates).
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include "EnergyCluster.hh"
#include "GeometricAcceptance.hh"
#include <iostream>

EnergyCluster::EnergyCluster() {
  Clear();
}

EnergyCluster::EnergyCluster(Int_t iCluster, TRecoLKrCandidate* LKrCand){
  Clear();
  fClusterID = iCluster;
  fLKrCandidate = LKrCand;
}

void EnergyCluster::Clear() {
  fClusterID = -1;
  fLKrCandidate = 0;
  fIsElectromagnetic = false;
  fSpectrometerAssociation.Clear();
  fIsIsolated = false;
  fIsInLKrAcceptance = false;

  //DownstreamTrack best matches
  fTracksForWhichThisClusterIsBestMatch.clear();
}

void EnergyCluster::Print() const {
  std::cout << "===== ClsID: " << fClusterID << "=====" << std::endl;
  fSpectrometerAssociation.Print();
  std::cout << "===================" << std::endl;
}
