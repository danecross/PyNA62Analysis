#ifndef LAVClusterMaker_H
#define LAVClusterMaker_H 1

#include "LAVGeometry.hh"
#include "TRecoLAVEvent.hh"
#include "TRecoLAVHit.hh"
#include "LAVCluster.hh"

#define MAXBLOCKMAP 130000
#define MAXBLOCKS 2500
#define MAXHITSPERBLOCK 1000

class LAVClusterMaker
{
public:

  LAVClusterMaker();
  ~LAVClusterMaker();
  void Clusterize(TRecoLAVEvent*);
  
  Int_t GetNClusters(){return fNClusters;}
  LAVCluster* GetCluster(Int_t ival){if (ival < fNClusters) {return fLAVClusters.at(ival);} else {return NULL;}}
  void Print();
  void Clear();
  
  void SetTimeDistanceMax(Double_t val){fTimeDistanceMax = val;}
  void SetTimeDistanceMarginal(Double_t val){fTimeDistanceMarginal = val;}
  void ComputeObservables();
private:

  LAVGeometry* fGeometryInstance;
  Int_t fNClusters;
  std::vector<LAVCluster*> fLAVClusters;
  Int_t* fNeighbouringBlocksHash[MAXBLOCKMAP];
  Bool_t* fUsedRecoHit;

// static block map

  Int_t fNFiredBlock;
  Int_t fBlockMapHash[MAXBLOCKMAP]; // blockChannelID to fHitPointers map
  std::vector<Int_t*> fHitPointers; // array of indexes to TClonesArray of TRecoLAVHits
  Int_t fNHitsPerBlock[MAXBLOCKS];  // number of hits for each block fired 
  Int_t fBlockChannelID[MAXBLOCKS]; // channel ID for for each block fired



  Double_t fTimeDistanceMax;
  Double_t fTimeDistanceMarginal;

  Bool_t ClusterSeedQuality(TRecoLAVHit*);
  Bool_t ClusterMemberQuality(TRecoLAVHit*);
  Bool_t CloseInTime(TRecoLAVHit*, TRecoLAVHit*);
  Bool_t MarginalInTime(TRecoLAVHit*, TRecoLAVHit*);
  Int_t* NeighbouringBlocks(Int_t);
};

#endif
