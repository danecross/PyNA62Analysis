// ---------------------------------------------------------------
// History:
//
// Created by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) 2016-12-01
//
// ---------------------------------------------------------------

/// \class ProtoParticle
/// \Brief
/// Precursor of Particle
/// \EndBrief
/// \Detailed
/// This class is a virtual class from which derive DownstramTrack, EnergyCluster...
/// It contains all the pointer to the hits/candidates from which the physics 
/// object derives.
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
/// \EndDetailed


#ifndef ProtoParticle_HH
#define ProtoParticle_HH

#include <vector>
#include <map>
#include "TRecoVCandidate.hh"
#include <string>



class ProtoParticle
{

public:

  enum CandidatesTypes{kGigaTrackerCluster, kTRecoCedarCandidate, kTRecoCHANTICandidate, kTRecoCHODCandidate, kTRecoGigaTrackerCandidate, kTRecoHACCandidate, kTRecoIRCCandidate, kTRecoLAVCandidate, kTRecoLKrCandidate, kTRecoMUV0Candidate, kTRecoMUV1Candidate, kTRecoMUV2Candidate, kTRecoMUV3Candidate, kTRecoNewCHODCandidate, kTRecoRICHCandidate, kTRecoSACCandidate, kTRecoSAVCandidate, kTRecoSpectrometerCandidate};


  ProtoParticle();
  ~ProtoParticle() {}

  std::vector<TRecoVCandidate*>  GetCandidates(CandidatesTypes);
  TRecoVCandidate*  GetCandidate(int, CandidatesTypes);
  void  AddCandidate(TRecoVCandidate*,CandidatesTypes);
  
private:
  std::map<int, std::vector<TRecoVCandidate*> > fCandidates;


  protected:
};


#endif//~ProtoParticle_HH

