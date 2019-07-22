#include "TSlimRecoLKrCandidate.hh"
#include "TRecoLKrCandidate.hh"

#include <iostream>

ClassImp(TSlimRecoLKrCandidate)

TSlimRecoLKrCandidate::TSlimRecoLKrCandidate(TRecoLKrCandidate *candReco){
  FromReco(candReco);
}

void TSlimRecoLKrCandidate::FromReco(TRecoVCandidate *candVReco){
  TRecoLKrCandidate *candReco = static_cast<TRecoLKrCandidate*>(candVReco);
  fTime = candReco->GetTime();
  fNCells = candReco->GetNCells();
  fClusterEnergy = candReco->GetClusterEnergy();
  fClusterX = candReco->GetClusterX();
  fClusterY = candReco->GetClusterY();
  fClusterRMSX = candReco->GetClusterRMSX();
  fClusterRMSY = candReco->GetClusterRMSY();
  fClusterDDeadCell = candReco->GetClusterDDeadCell();
  fClusterSeedEnergy = candReco->GetClusterSeedEnergy();
  fCluster77Energy = candReco->GetCluster77Energy();
  fIdSeed = candReco->GetIdSeed();
}

void TSlimRecoLKrCandidate::ToReco(TRecoVCandidate *candVReco){
  TRecoLKrCandidate *candReco = static_cast<TRecoLKrCandidate*>(candVReco);
  candReco->SetTime(fTime);
  candReco->SetClusterEnergy(fClusterEnergy);
  candReco->SetClusterX(fClusterX);
  candReco->SetClusterY(fClusterY);
  candReco->SetClusterRMSX(fClusterRMSX);
  candReco->SetClusterRMSY(fClusterRMSY);
  candReco->SetClusterDDeadCell(fClusterDDeadCell);
  candReco->SetClusterSeedEnergy(fClusterSeedEnergy);
  candReco->SetNCells(fNCells);
  candReco->SetCluster77Energy(fCluster77Energy);
  candReco->SetIdSeed(fIdSeed);
  // set variables retrieved form Slim Persistency methods
  // set useless variable defined in Standard Persistency
  for(Int_t i=0; i<50; i++){
    candReco->SetId77Cell(i,0);
    candReco->SetFlag77Cell(i,0);
    candReco->SetEnergy77Cell(i,0);
    candReco->SetTime77Cell(i,0);
  }
  candReco->SetId(-9999);
  candReco->SetClusterEnergyError(-9999);
  candReco->SetClusterStatus(-9999);
  candReco->SetClusterChi2RMS(-9999);
  candReco->SetClusterTimeLateralCell(-9999);
  candReco->SetClusterUEnergy(-9999);
  candReco->SetClusterEnoise(-9999);
  candReco->SetSpaceChargeCorr(-9999);
  candReco->SetClusterKe3Energy(-9999);
  candReco->SetN77Cells(-9999);
  candReco->SetCluster77Enoise(-9999);

}

