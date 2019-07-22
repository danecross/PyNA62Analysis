// LAVCluster.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added treatments of position error matrix and uncertainties in the phi and z coordinates
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVCluster
/// \Brief
/// Class for LAV clusters (soft class, living in the context of the reconstruction code)
/// \EndBrief

#include "LAVCluster.hh"

LAVCluster::LAVCluster() :
  fPosition(0,0,0),
  fWeightedPosition(0,0,0),
  fTime(0),
  fEnergy(0),
  fStation(0), // stations range from 1 to 12
  fNHits(0),
  fNLayers(0),
  fUpLayer(10),
  fDownLayer(-1),
  fPhiMin(.0),
  fPhiMax(.0),
  fZUnweightedError(0),
  fPhiUnweightedError(0),
  fZWeightedError(0),
  fPhiWeightedError(0),
  fEvent(nullptr)
{
  for (Int_t i=0; i<NMAXHITSPERCLUSTER; i++) fHitList[i]=0;


  for (Int_t i=0; i<5; i++) {
    fNHitsPerLayer[i] = 0;
    for (Int_t j=0; j<NMAXHITSPERCLUSTER; j++) fHitListLayer[i][j]=-1;
    fPhiMinLayer[i] = 999;
    fPhiMaxLayer[i] = 999;
    fPhiCenter[i] = 999;
  }
}

void LAVCluster::Print(){
  
  std::cout << "LAVCluster print -- Number of hits = " << fNHits << " in Station " << fStation << std::endl; 
  
  for (Int_t i=0; i<fNHits; i++) {
    std::cout << " Hit " << i << " in cluster is " << fHitList[i] << std::endl;
  }
  std::cout << "Clus E = " << fEnergy << " T = " << fTime << 
    " <phi> = " << fPosition.Phi() << " <Rho> = " << fPosition.Perp() << " <z> = " << fPosition.Z() << 
    " <phiW> = " << fWeightedPosition.Phi() << " <RhoW> = " << fWeightedPosition.Perp() << " <zW> = " << fWeightedPosition.Z() << 
    " NLayers = " << fNLayers << " from " << fUpLayer << " to " << fDownLayer << std::endl;
  std::cout << "Clus Phi Range " << fPhiMin << " " << fPhiMax << std::endl;
  for (Int_t ilay=0; ilay<5; ilay++) {
    std::cout << "Layer " << ilay << " NHits " << fNHitsPerLayer[ilay] << " phimin = " << fPhiMinLayer[ilay] << " phimax " << fPhiMaxLayer[ilay] << " phiC = " << fPhiCenter[ilay] << std::endl;
  }

  std::cout << "LAVCluster " 
       << " phiUWErr = " << fPhiUnweightedError << " zUWErr = " << fZUnweightedError 
       << " phiWErr = " << fPhiWeightedError << " zWErr = " << fZWeightedError 
       << std::endl;
}


Int_t LAVCluster::GetHitIndex(Int_t iHit){
  if (iHit >=0 && iHit < fNHits) return fHitList[iHit];
  std::cerr << "LAVCluster GetHitIndex called with out of range input " << iHit << std::endl;
  return -1;  
}


Bool_t LAVCluster::AddHit(Int_t iHit){
  if (fNHits < NMAXHITSPERCLUSTER) {
    fHitList[fNHits++] = iHit;
    return kTRUE;
  }
  else {
    std::cerr << "LAVCluster >> Max hit limit reached " << iHit << " " << fEvent << std::endl;
    return kFALSE;
  }
}
  
TRecoLAVHit * LAVCluster::GetHit(Int_t iHit){
    if(fEvent && iHit < fNHits)
      return static_cast<TRecoLAVHit*>(fEvent->GetHit(fHitList[iHit]));
    else
      return nullptr;
}


void LAVCluster::ComputeClusterProperties(){  
/// \MemberDescr
/// Method to evaluate various cluster properties
/// \EndMemberDescr
  fEnergy = 0;
  fNLayers = 0;
  fUpLayer = 10;
  fDownLayer = -1;
  fTime = 0;

  fPosition.SetXYZ(0,0,0);
  fWeightedPosition.SetXYZ(0,0,0);

  fZUnweightedError = 0;
  fPhiUnweightedError = 0;
  fZWeightedError = 0;
  fPhiWeightedError = 0;


  Double_t phiUnweightedAverage = 0;
  Double_t zUnweightedAverage = 0;
  Double_t phiWeightedAverage = 0;
  Double_t zWeightedAverage = 0;

  Double_t phiSums[5];
  for (Int_t i=0; i<5; i++) {
    phiSums[i] = 0;

    fNHitsPerLayer[i] = 0;
    for (Int_t j=0; j<NMAXHITSPERCLUSTER; j++) fHitListLayer[i][j]=-1;
    fPhiMinLayer[i] = 999;
    fPhiCenter[i] = 999;
    fPhiMaxLayer[i] = 999;
  }

  Double_t rhoAverage = 721.5; // init default for small stations 
  //Double_t singleBlockRhoError = 107.;  // init default: 370 mm / sqrt(12)
  Double_t singleBlockPhiError = 0.057; // init for the small station
  Double_t singleBlockZError = 34.8;    // init default: 120 mm / sqrt(12)

  Double_t zUnweightedSum = 0; 
  Double_t zWeightedSum = 0; 
  Double_t phiUnweightedSum = 0; 
  Double_t phiWeightedSum = 0; 
  Double_t e2Sum = 0; 


  for (Int_t i=0; i<fNHits; i++) {
    TRecoLAVHit* recoLAVHit = LAVCluster::GetHit(i);
    TVector3 blockPosition;
    recoLAVHit->GetBlockPosition(blockPosition);

    if (fStation == 0) {
      fStation = recoLAVHit->GetLAVID();
      singleBlockPhiError = recoLAVHit->GetBlockPhiSpan(fStation)*0.29; // Flat distribution: 1/Sqrt(12.)
      rhoAverage = blockPosition.Perp();
    }

    Double_t hitEnergy = recoLAVHit->GetEnergy();

// Time and Energy

    fTime += recoLAVHit->GetTime()*hitEnergy;
    fEnergy += hitEnergy;

// z-Coordinate

    zUnweightedSum += blockPosition.Z();
    zWeightedSum += blockPosition.Z()*hitEnergy;

// information per layer

    Int_t ilay = recoLAVHit->GetLayerID();
    fHitListLayer[ilay][fNHitsPerLayer[ilay]]=fHitList[i];
    if (fNHitsPerLayer[ilay]==0) fNLayers++;
    fNHitsPerLayer[ilay]++;
    if (ilay > fDownLayer) fDownLayer = ilay;
    if (ilay < fUpLayer) fUpLayer = ilay;

// phi evaluation

    Double_t phi = blockPosition.Phi(); // between -pi and pi
    Double_t dPhiLeft = phi-fPhiMin;
    if (dPhiLeft < 0) dPhiLeft += 2*TMath::Pi();
    Double_t dPhiRight = fPhiMax-phi;
    if (dPhiRight < 0) dPhiRight += 2*TMath::Pi();

    if (dPhiLeft < fPhiMinLayer[ilay]) fPhiMinLayer[ilay] = dPhiLeft;
    if (dPhiRight < fPhiMaxLayer[ilay]) fPhiMaxLayer[ilay] = dPhiRight;

    phiSums[ilay] += dPhiLeft;

    phiUnweightedSum += dPhiLeft;
    phiWeightedSum += dPhiLeft*hitEnergy;

    e2Sum += hitEnergy*hitEnergy;
  }

// evaluate weighted averages

  
  if (fEnergy) {

// time

    fTime = fTime/fEnergy;

// phi

    phiWeightedAverage = phiWeightedSum/fEnergy;
    phiWeightedAverage += fPhiMin;
    if (phiWeightedAverage > TMath::Pi()) phiWeightedAverage -= 2*TMath::Pi();
    fPhiWeightedError = TMath::Sqrt(e2Sum)/fEnergy*singleBlockPhiError;

// zeta

    zWeightedAverage = zWeightedSum/fEnergy;
    fZWeightedError = TMath::Sqrt(e2Sum)/fEnergy*singleBlockZError;
  }

// evaluate unweighted average for phi and z

  phiUnweightedAverage = phiUnweightedSum/fNHits;
  phiUnweightedAverage += fPhiMin;
  if (phiUnweightedAverage > TMath::Pi()) phiUnweightedAverage -= 2*TMath::Pi();
  fPhiUnweightedError = singleBlockPhiError/TMath::Sqrt(fNHits);

  zUnweightedAverage = zUnweightedSum/fNHits;
  fZUnweightedError = singleBlockZError/TMath::Sqrt(fNHits);

// Fill average phi per layer

  for (Int_t ilay = 0; ilay<5; ilay++) {
    if (fNHitsPerLayer[ilay]) {
      fPhiCenter[ilay] = fPhiMin + phiSums[ilay]/fNHitsPerLayer[ilay];
      if (fPhiCenter[ilay] > TMath::Pi()) fPhiCenter[ilay] -= 2*TMath::Pi();
      
      fPhiMinLayer[ilay] += fPhiMin;
      if (fPhiMinLayer[ilay] > TMath::Pi()) fPhiMinLayer[ilay] -= 2*TMath::Pi();

      fPhiMaxLayer[ilay] = fPhiMax - fPhiMaxLayer[ilay];
      if (fPhiMaxLayer[ilay] < -TMath::Pi()) fPhiMaxLayer[ilay] += 2*TMath::Pi();      
    }
  }


  fPosition.SetXYZ(rhoAverage*TMath::Cos(phiUnweightedAverage),rhoAverage*TMath::Sin(phiUnweightedAverage),zUnweightedAverage);
  fWeightedPosition.SetXYZ(rhoAverage*TMath::Cos(phiWeightedAverage),rhoAverage*TMath::Sin(phiWeightedAverage),zWeightedAverage);
}
