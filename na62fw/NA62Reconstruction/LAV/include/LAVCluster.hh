#ifndef LAVCluster_H
#define LAVCluster_H 1

#define NMAXHITSPERCLUSTER 30

#include "TRecoLAVEvent.hh"
#include "TRecoLAVHit.hh"
#include "TVector3.h"
#include "Riostream.h"

class LAVCluster
{
public:
  LAVCluster();  
  //  LAVCluster(){};
  ~LAVCluster(){};

  void Print();
  void SetEnergy(Double_t val)    {fEnergy = val;}
  Double_t GetEnergy()            {return fEnergy;}
  void SetTime(Double_t val)      {fTime = val;}
  Double_t GetTime()              {return fTime;}

// cluster position methods

  void SetPosition(TVector3 val)        {fPosition = val;}
  TVector3 GetPosition()                {return fPosition;} 
  void SetWeightedPosition(TVector3 val){fWeightedPosition = val;}
  TVector3 GetWeightedPosition()        {return fWeightedPosition;} 

// cluster topology methods
  
  void SetNLayers(Int_t val)      {fNLayers = val;} 
  Int_t GetNLayers()              {return fNLayers;}
  void SetUpLayer(Int_t val)      {fUpLayer = val;}
  Int_t GetUpLayer()              {return fUpLayer;} 
  void SetDownLayer(Int_t val)    {fDownLayer = val;}
  Int_t GetDownLayer()            {return fDownLayer;}

// azimuthal angle methods

  Double_t GetDPhiLeft(Int_t lay) {return fPhiMinLayer[lay];}   
  Double_t GetPhiCenter(Int_t lay){return fPhiCenter[lay];}    
  Double_t GetDPhiRight(Int_t lay){return fPhiMaxLayer[lay];}  
  void SetPhiRange(Double_t phiMin, Double_t phiMax){fPhiMin = phiMin; fPhiMax = phiMax;}
  Double_t GetPhiMin() {return fPhiMin;}                      
  Double_t GetPhiMax() {return fPhiMax;}                      

// methods to retrieve errors on the averages

  Double_t GetZUnweightedError()     {return fZUnweightedError;}     
  Double_t GetPhiUnweightedError()   {return fPhiUnweightedError;} 
  Double_t GetZWeightedError()       {return fZWeightedError;}         
  Double_t GetPhiWeightedError()     {return fPhiWeightedError;}     

// hit retrieval methods: per layer or global

  Int_t GetNHitsPerLayer(Int_t lay)                    {return fNHitsPerLayer[lay];} 
  Int_t GetHitIndexLayer(Int_t lay, Int_t nHitPerLayer){return fHitListLayer[lay][nHitPerLayer];}
  Int_t GetStationID() {return fStation;}  
  Bool_t AddHit(Int_t);
  Int_t GetNHits(){return fNHits;}
  Int_t GetHitIndex(Int_t);

// Link with the TRecoLAVEvent instance

  void SetEvent(TRecoLAVEvent* val){fEvent = val;}
  TRecoLAVEvent* GetEvent(){return fEvent;}

  void ComputeClusterProperties(); ///< Method to evaluate various cluster properties


private:
  TRecoLAVHit* GetHit(Int_t);
  TVector3 fPosition;        ///< Cluster centroid position: no weight is used while averaging fired block positions; average is performed using rho, phi and z
  TVector3 fWeightedPosition;///< Cluster centroid position: the block energy (in fact, at the moment, the charge) is used while averaging fired block positions in cylndrical coordinates
  Double_t fTime; ///< Cluster centroid time: the block energy (in fact, at the moment, the charge) is used while averaging fired block times
  Double_t fEnergy; ///< Total cluster energy (at the moment, in fact, it is the total charge)
  Int_t fHitList[NMAXHITSPERCLUSTER]; ///< List of recoHit indexes
  Int_t fHitListLayer[5][NMAXHITSPERCLUSTER]; ///< List of recoHit indexes per layer
  Int_t fStation; ///< Cluster station (at the moment, intra-station clusters are not created)
  Int_t fNHits;   ///< Number of reconstructed hits grouped into the cluster
  Int_t fNLayers; ///< Number of layers fired
  Int_t fUpLayer; ///< index of most upstream layer fired [0,nlayers]
  Int_t fDownLayer; ///< index of most downstream layer fired [0,nlayers]
  Int_t fNHitsPerLayer[5]; ///< Number of reconstructed hits per layer
  Double_t fPhiMinLayer[5];///< Minimum phi per layer, evaluated minimizing the angular distance wrt the minimum phi in the cluster
  Double_t fPhiCenter[5];  ///< Average phi layer
  Double_t fPhiMaxLayer[5];///< Maximum phi per layer, evaluated maximizing the angular distance wrt the minimum phi in the cluster 
  Double_t fPhiMin; ///< Minimum phi in the cluster (between -pi and pi)
  Double_t fPhiMax; ///< Maximum phi in the cluster (between -pi and pi)

  Double_t fZUnweightedError;   ///< Error on the unweighted average of the z coordinate
  Double_t fPhiUnweightedError; ///< Error on the unweighted average of the azimuthal angle
  Double_t fZWeightedError;     ///< Error on the energy-weighted average of the z coordinate   
  Double_t fPhiWeightedError;   ///< Error on the energy-weighted average of the azimuthal angle

  TRecoLAVEvent* fEvent; ///< Link with the TRecoLAVEvent instance
};
#endif
