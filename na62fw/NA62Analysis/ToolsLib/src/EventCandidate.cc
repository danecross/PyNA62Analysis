/// \class EventCandidate
/// \Brief
/// A container for event candidates 
/// \EndBrief
/// \Detailed
/// Contains an UpstreamTrack, a vector of DownstreamTrack and a vector of EnergyCluster
/// \endcode
/// \author
/// \EndDetailed

#include "EventCandidate.hh"
#include "DownstreamTrack.hh"


EventCandidate::EventCandidate(){
  Clear();
}

EventCandidate::EventCandidate(DownstreamTrack *track) :
  fEventVetoes(nullptr),
  fUpstreamTrack(nullptr)
{
  fDownstreamTracks.push_back(track);
}

EventCandidate::EventCandidate(std::vector<DownstreamTrack*> tracks) :
  fEventVetoes(nullptr),
  fUpstreamTrack(nullptr)
{
  if(!fDownstreamTracks.empty()) fDownstreamTracks.insert(fDownstreamTracks.end(), tracks.begin(), tracks.end());
}

void EventCandidate::Print(){
  std::cout << "* Nr. of DonwstreamTrack = " << fDownstreamTracks.size() << std::endl;
  std::cout << "* DownstreamTrack information: " << std::endl;
  for (UInt_t iTrk=0; iTrk<fDownstreamTracks.size(); iTrk++) {
    std::cout << "ID: " << fDownstreamTracks[iTrk]->GetTrackID()
              << " CHOD association = " << fDownstreamTracks[iTrk]->CHODAssociationExists()
              << " NewCHOD association = " << fDownstreamTracks[iTrk]->NewCHODAssociationExists();
    if(fDownstreamTracks[iTrk]->CHODAssociationExists())
              std::cout << " CHOD time = " << fDownstreamTracks[iTrk]->GetCHODTime();
    //if(fDownstreamTracks[iTrk]->NewCHODAssociationExists())
              //std::cout << " NewCHOD time = " << fDownstreamTracks[iTrk]->GetNewCHODTime(0);
              //<< " NewCHOD time = " << fDownstreamTracks[iTrk]->GetNewCHODTime(0) 
    std::cout << " track time = " << fDownstreamTracks[iTrk]->GetTrackTime() << std::endl;
    std::cout << " vertex = " << fDownstreamTracks[iTrk]->GetNominalBeamAxisVertex().X()
              << " " << fDownstreamTracks[iTrk]->GetNominalBeamAxisVertex().Y() << " "
              << fDownstreamTracks[iTrk]->GetNominalBeamAxisVertex().Z()
              << " position at zLKr: X = " << fDownstreamTracks[iTrk]->xAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr())
              << " Y = " << fDownstreamTracks[iTrk]->yAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr()) << std::endl;
  }
  std::cout << "* Nr. of EnergyCluster = " << fEnergyClusters.size() << std::endl;
  if(fEnergyClusters.size()) std::cout << "* EnergyCluster information: " << std::endl;
  for (UInt_t iClu=0; iClu<fEnergyClusters.size(); iClu++) {
    std::cout <<"ID: " << fEnergyClusters[iClu]->GetClusterID()
              <<" LKr Candidate information: " << std::endl;
    std::cout <<" ClusterX = " << fEnergyClusters[iClu]->GetLKrCandidate()->GetClusterX()
              <<" ClusterY = " << fEnergyClusters[iClu]->GetLKrCandidate()->GetClusterY()
              <<" Cluster time = " << fEnergyClusters[iClu]->GetLKrCandidate()->GetTime() << std::endl;
  } 
}

void EventCandidate::Clear(){
  fEventVetoes=0;
  fUpstreamTrack=0;
  fDownstreamTracks.clear();
  fEnergyClusters.clear();
  fChargedParticleCandidates.clear();
  fNeutralParticleCandidates.clear();
  fPionsIndices.clear();
  fMuonsIndices.clear();
  fElectronsIndices.clear();
}

void EventCandidate::AddEventVetoes(EventVetoes* EventVetoes){
  fEventVetoes = EventVetoes;
}

void EventCandidate::AddUpstreamTrack(UpstreamTrack* upstreamTrack){
  fUpstreamTrack = upstreamTrack;
}

void EventCandidate::AddDownstreamTrack(DownstreamTrack* track){
  fDownstreamTracks.push_back(track);
}

void EventCandidate::AddDownstreamTracks(std::vector<DownstreamTrack*> tracks){
  if(!fDownstreamTracks.empty()) fDownstreamTracks.insert(fDownstreamTracks.end(), tracks.begin(), tracks.end());
}

void EventCandidate::SetDownstreamTracks(const std::vector<DownstreamTrack*> &tracks){
  fDownstreamTracks.clear();
  fDownstreamTracks=tracks;
}

void EventCandidate::SetEnergyClusters(const std::vector<EnergyCluster*> &clusters){
  fEnergyClusters.clear();
  fEnergyClusters=clusters;
}
void EventCandidate::AddEnergyCluster(EnergyCluster* energyCluster){
  fEnergyClusters.push_back(energyCluster);
}

void EventCandidate::MergeWith(EventCandidate* eventToMerge){
  //std::cout << "downstream track size in current event = " << fDownstreamTracks.size() << "max_size() = " << fDownstreamTracks.max_size() << std::endl;
  //std::cout << "downstream track size in event to be merged = " << eventToMerge->GetDownstreamTracks().size() << std::endl;
  std::vector<DownstreamTrack*> tracksToAdd = eventToMerge->GetDownstreamTracks();
  std::vector<EnergyCluster*> clustersToAdd = eventToMerge->GetEnergyClusters();
  if(!fDownstreamTracks.empty())
    fDownstreamTracks.insert(fDownstreamTracks.end(), tracksToAdd.begin(), tracksToAdd.end());
  if(!fEnergyClusters.empty())
    fEnergyClusters.insert(fEnergyClusters.end(), clustersToAdd.begin(), clustersToAdd.end());
}


//ParticleCandidate methods
void EventCandidate::SetChargedParticleCandidates(const std::vector<ChParticleCandidate*> &chParticles){
  fChargedParticleCandidates.clear();
  fChargedParticleCandidates=chParticles;
}

void EventCandidate::SetNeutralParticleCandidates(const std::vector<NeParticleCandidate*> &neParticles){
  fNeutralParticleCandidates.clear();
  fNeutralParticleCandidates=neParticles;
}

void EventCandidate::SetPionsIndices(const std::vector<int> &fPiIndices){
  fPionsIndices.clear();
  fPionsIndices=fPiIndices;
}

void EventCandidate::SetMuonsIndices(const std::vector<int> &fMuIndices){
  fMuonsIndices.clear();
  fMuonsIndices=fMuIndices;
}

void EventCandidate::SetElectronsIndices(const std::vector<int> &fElIndices){
  fElectronsIndices.clear();
  fElectronsIndices=fElIndices;
}
