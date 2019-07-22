#include "CalorimeterCluster.hh"

ClassImp(CalorimeterCluster)

CalorimeterCluster::CalorimeterCluster(){
  Clear();
}


CalorimeterCluster::~CalorimeterCluster() {
}

void CalorimeterCluster::Clear (Option_t *opt){

  TObject::Clear(opt);

  fTrackID = -1;

  fTime = 0;
  fEnergy = 0;
  fHadronicEnergy = 0;

  fIsElectronProbability = 0;
  fIsMuonProbability = 0;
  fIsPionProbability = 0;
  fIsElectronOldProbability = 0;
  fIsMuonOldProbability = 0;
  fIsPionOldProbability = 0;

  fLKrCand = nullptr;
  fMUV1Cand = nullptr;
  fMUV2Cand = nullptr;

  fIsLKrMatch = false;
  fIsMUV1Match = false;
  fIsMUV2Match = false;

  fLKrEnergy = 0;
  fLKrHadronicEnergy = 0;
  fMUV1Energy = 0;
  fMUV2Energy = 0;
  fOldMUV1Energy = 0;
  fOldMUV2Energy = 0;

  fLKrTime = 0;
  fMUV1Time = 0;
  fMUV2Time = 0;

  fLKrWeight = 0;
  fMUV1Weight = 0;
  fMUV2Weight = 0;
  fOldMUV1Weight = 0;
  fOldMUV2Weight = 0;

  fLKrEnergyCorrections = 0;
  fMUV1EnergyCorrections = 0;
  fMUV2EnergyCorrections = 0;

  fMUV1InTimeEnergy=0;
  fMUV1OuterEnergy=0;
  fMUV1OuterNhits=0;
	fMUV1OuterPositionX = fMUV1OuterPositionY = 0.;

  fMUV2InTimeEnergy=0;
  fMUV2OuterEnergy=0;
  fMUV2OuterNhits=0;

  fHACWeight = 0;
  fHACShowerWidth = 0;
  fHACPosition.Set(0.,0.);

}
