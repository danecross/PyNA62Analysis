/// \class ProtoParticle
/// \Brief
/// Precursor for Particle. All classes like DownStreamTrack or EneryCluster should inherit from ProtoParticle
/// \EndBrief
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)

#include "ProtoParticle.hh"
#include <iostream>

using namespace std;

//  ==============================================  
ProtoParticle::ProtoParticle()
{
   
}

//==============================================
void  ProtoParticle::AddCandidate(TRecoVCandidate* cand, CandidatesTypes d){

  std::map<const int, std::vector<TRecoVCandidate*> >::iterator it;
  it = fCandidates.find(d);
  if(  it == fCandidates.end() ) {
    vector<TRecoVCandidate*> cands;
    cands.push_back(cand);
    fCandidates.insert( pair< int, vector<TRecoVCandidate*> >(d,cands) );
  }
  else{
    (it->second).push_back(cand);
  }
}

//==============================================
vector<TRecoVCandidate*>  ProtoParticle::GetCandidates(CandidatesTypes d){

  if (fCandidates.empty())  return vector<TRecoVCandidate*>();
  
  std::map<int, std::vector<TRecoVCandidate*> >::iterator it = fCandidates.find(d);
  if(it==fCandidates.end()) return  vector<TRecoVCandidate*> ();

  return it->second;
}

//==============================================
TRecoVCandidate*  ProtoParticle::GetCandidate( int i, CandidatesTypes det ){

  vector<TRecoVCandidate*> v = GetCandidates(det);
  if(v.size() ==0 || i >= int(v.size())) return NULL;

  return v[i];
}


