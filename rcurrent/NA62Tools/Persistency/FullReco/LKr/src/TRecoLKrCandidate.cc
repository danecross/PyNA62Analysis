#include "TRecoLKrCandidate.hh"
#include "TRecoLKrHit.hh"
#include "TLKrHit.hh"
#include "Riostream.h"
#include "TRecoVEvent.hh"
#include "TMath.h" 

/// \class LKrCandidate
/// \Brief
/// LKrCandidate class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the LKr cluster candidates.
/// \EndDetailed

ClassImp(TRecoLKrCandidate)

TRecoLKrCandidate::TRecoLKrCandidate(){}

void TRecoLKrCandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
  fId = 0;
  fNCells = 0; 
  fIdSeed = 0; 
  fClusterEnergy = 0;
  fClusterEnergyError = 0;
  fClusterStatus = 0;
  fClusterX = 9999;
  fClusterY = 9999;
  fClusterRMSX = 0;
  fClusterRMSY = 0;
  fClusterTime = 0;
  fClusterChi2RMS = 99999;
  fClusterTimeLateralCell = 0;
  fClusterDDeadCell = 0;
  fClusterUEnergy = 0;
  fClusterEnoise = 0;
  fCluster77Energy = 0;
  fSpaceChargeCorr = 0;
  fClusterKe3Energy = 0;
  fClusterUTime = 0;
  fN77Cells = 0;
  for (Int_t j=0; j<50; j++){
    fId77Cell[j] = 0;
    fFlag77Cell[j] = 0;
    fEnergy77Cell[j] = 0;
    fTime77Cell[j] = 0;
  }
  fClusterSeedEnergy = 0;
}
