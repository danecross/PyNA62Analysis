#ifndef EVENTCANDIDATE_HH
#define EVENTCANDIDATE_HH

#include "DownstreamTrack.hh"
#include "UpstreamTrack.hh"
#include "EventVetoes.hh"
#include "ChParticleCandidate.hh"
#include "NeParticleCandidate.hh"
#include "GeometricAcceptance.hh"
#include "TRecoLKrCandidate.hh"
#include "EnergyCluster.hh"
#include <iostream>
#include <vector>

class EventCandidate {

public:
  
  EventCandidate();
  explicit EventCandidate(DownstreamTrack* track);
  explicit EventCandidate(std::vector<DownstreamTrack*> tracks);
  ~EventCandidate(){};
  void Clear();
  void Print();

  void AddEventVetoes(EventVetoes* vetoes);
  void AddUpstreamTrack(UpstreamTrack* track);
  void AddDownstreamTrack(DownstreamTrack* track);
  void AddDownstreamTracks(std::vector<DownstreamTrack*> tracks);
  void AddEnergyCluster(EnergyCluster* energyCluster);
  void SetEnergyClusters(const std::vector<EnergyCluster*> &clusters);
  void SetDownstreamTracks(const std::vector<DownstreamTrack*> &tracks);

  //ParticleCandidates methods

  //void AddChargedParticleCandidate(chParticleCandidate* chPart);
  //void AddChargedParticleCandidates(std::vector<chParticleCandidate*> chParts);
  //void AddNeutralParticleCandidate(neParticleCandidate* nePart);
  //void AddNeutralParticleCandidates(std::vector<neParticleCandidate*> neParts);
  void SetChargedParticleCandidates(const std::vector<ChParticleCandidate*> &chParts);
  void SetNeutralParticleCandidates(const std::vector<NeParticleCandidate*> &neParts);
  void SetPionsIndices(const std::vector<int> &pIndex);
  void SetElectronsIndices(const std::vector<int> &eIndex);
  void SetMuonsIndices(const std::vector<int> &mIndex);
  
  std::vector<ChParticleCandidate*> GetChargedParticleCandidates(){return fChargedParticleCandidates;};
  std::vector<NeParticleCandidate*> GetNeutralParticleCandidates(){return fNeutralParticleCandidates;};
  std::vector<int> GetPionsIndices(){return fPionsIndices;};
  std::vector<int> GetMuonsIndices(){return fMuonsIndices;};
  std::vector<int> GetElectronsIndices(){return fElectronsIndices;};

  Int_t GetNDownstreamTracks(){return fDownstreamTracks.size();};
  std::vector<DownstreamTrack*> GetDownstreamTracks(){return fDownstreamTracks;};
  DownstreamTrack* GetDownstreamTrack(Int_t iTrack){return fDownstreamTracks.at(iTrack);};  

  EventVetoes* GetEventVetoes(){return fEventVetoes;};
  UpstreamTrack* GetUpstreamTrack(){return fUpstreamTrack;};

  Int_t GetNEnergyClusters(){return fEnergyClusters.size();};
  std::vector<EnergyCluster*> GetEnergyClusters(){return fEnergyClusters;};
  EnergyCluster* GetEnergyCluster(Int_t iCluster){return fEnergyClusters.at(iCluster);};

  //Add to the EventCandidate the DownstreamTrack and EnergyCluster of "eventToMerge" without changing the latter
  void MergeWith(EventCandidate* eventToMerge);

private:

  EventVetoes* fEventVetoes;
  UpstreamTrack* fUpstreamTrack;
  std::vector<ChParticleCandidate*> fChargedParticleCandidates;
  std::vector<NeParticleCandidate*> fNeutralParticleCandidates;
  std::vector<int> fPionsIndices;
  std::vector<int> fElectronsIndices;
  std::vector<int> fMuonsIndices;

  std::vector<DownstreamTrack*> fDownstreamTracks;
  std::vector<EnergyCluster*> fEnergyClusters;


};

#endif
