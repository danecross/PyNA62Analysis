#include "Riostream.h"
#include "Track.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"
#include <TMatrixD.h>
#include <TMath.h>
#include "TSpline.h"

Track::Track() :
  //######################################
  // TODO: These were not initialised. I used 0 as default everywhere
  // Please fix it correctly.
  //######################################
  fIteration(0),
  jCombination(nullptr),
  fChamber(nullptr),
  fCandidate(nullptr),
  fDATA(1),
  fNChambers(0),
  fNResidualHits(0),
  fPMom(0.),
  fFastMomentum(0.),
  fFastThetaX(0.),
  fFastThetaY(0.),
  fFastX0(0.),
  fFastY0(0.),
  fFastZ0(0.),
  fFastCharge(0.),
  fTTime(0.),
  fLTime(0.),
  fPtrack(0.),
  fThetaX(0.),
  fThetaY(0.),
  fPosX(0.),
  fPosY(0.),
  fCharge(0.),
  fChi2(0.),
  fZRef(0.),

  type_of_data(0),
  fXMag(0.),
  fYMag(0.),
  deltaX(0.),
  Distance_ToNext_Hit(0.),
  Hit_Count(0),
  Hit_Added(0),
  fFinalMomentum(0.),
  viewID(0.),
  Measured_Hit_Position_X(0.),
  Kal_Sigma(0.),
  UpStream_ChamberID(0),
  DownStream_ChamberID(0),
  DistanceTo_Magnet(0.),
  DistanceFrom_Magnet(0.),
  Fit_Direction(0),
  badfit(false),
  trans_mag(false),

  KickDirection_Charge(0),
  KickDirection_Fit(0),

  fPtrack_original(0.),
  fThetaX_original(0.),
  fThetaY_original(0.),
  fPosX_original(0.),
  fPosY_original(0.),
  fCharge_original(0.),
  chi2_original(0.),

  fHitCountStartBackward(0),
  fHitAddedStartBackward( 0),
  fHitCountStartForward(0),
  fHitAddedStartForward(0),

  fPositionZ(0.),

  fHResidual(nullptr),
  fHResidualvsR(nullptr),
  fHResidualvsR_304(nullptr),
  fHResidualvsR_313(nullptr),
  fHResidualvsR_2379(nullptr),
  fHResidualvsR_2388(nullptr),
  fHStrawPos(nullptr),
  fHTimeCor(nullptr)
{
  for (int i = 0; i < 16; i++) {
    fHStrawPosVSAngle[i] = nullptr;
    fHTimeCorPerView[i] = nullptr;
  }

  for (Int_t j=0; j<100; j++) fTimeCorX[j] = 0; // Correction for signal propagation. Adding this line here fixes a "conditional jump or move depends on uninitialised value"
  //######################################

  fSpecGeo = SpectrometerGeometry::GetInstance(); 
  fDMag = SpectrometerGeometry::GetInstance()->GetMagnetZLength();
  fZMag = SpectrometerGeometry::GetInstance()->GetMagnetZPosition();
  fEC = SpectrometerParameters::GetInstance()->GetEC();
  fBMag = SpectrometerGeometry::GetInstance()->GetMagnetFieldStrength()*1000;//0.6928Tm*1000
  fZRef_Upstream = 180000;//mm
  fZRef_Downstream = 222000;//mm
  fXX0 = SpectrometerParameters::GetInstance()->GetXX0perPlane();
  fGuessAverageSigma = SpectrometerParameters::GetInstance()->GetGuessAverageSigma();
  fGuessSigmaNoSlope = SpectrometerParameters::GetInstance()->GetGuessSingletSigma(); 
  fIsMuonRun = SpectrometerParameters::GetInstance()->GetIsMuonRun();
  fPar = SpectrometerParameters::GetInstance();
  fRTMode = fPar->GetRTMode();
  fXTMode = fPar->GetXTMode();

  // RT dependence
  fRTdependence = new TF1("RTdependence",
                          [&](double *x, double *){return fPar->GetRTDependenceData(x[0]/1000);},
                          15, 300, 0);

  // B integral measured 
  SpectrometerParameters::GetInstance()->SetBIntegral();
  SpectrometerParameters::GetInstance()->SetBMNP33();
  SpectrometerParameters::GetInstance()->SetBFringeField();
  fScale = 1;
  fPtKick = fEC*fBMag*fDMag*1000; // Uniform Ptkick for MC 
  fPtKickY = 0;
  
  // Fill in local arrays
  for (Int_t jj=0; jj<26; jj++) {
    for (Int_t kk=0; kk<26; kk++) {
      for (Int_t ll=0; ll<169; ll++) {
        fBxMNP33[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBMNP33(jj,kk,ll).X(); 
        fByMNP33[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBMNP33(jj,kk,ll).Y(); 
        fBzMNP33[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBMNP33(jj,kk,ll).Z(); 
        if (jj==0 && kk==0) fZcoorMNP33[ll] = SpectrometerParameters::GetInstance()->GetZMNP33(ll);
      }
    }
  } 
  for (Int_t jj=0; jj<17; jj++) {
    for (Int_t kk=0; kk<17; kk++) {
      for (Int_t ll=0; ll<30; ll++) {
        fBxFringe[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBFringe(jj,kk,ll).X(); 
        fByFringe[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBFringe(jj,kk,ll).Y(); 
        fBzFringe[ll][jj][kk] = SpectrometerParameters::GetInstance()->GetBFringe(jj,kk,ll).Z(); 
        if (jj==0 && kk==0) fZcoorFringe[ll] = SpectrometerParameters::GetInstance()->GetZFringe(ll);
      }
    }
  } 

  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "      KALMAN FILTER         " << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << "============================" << std::endl;
  std::cout << fIsMuonRun << std::endl;

  // Matrices initialization  
  Int_t nParameters = !fIsMuonRun ? 5 : 4;
  Kal_Eta_prime.ResizeTo(nParameters,1);
  Kal_V_prime.ResizeTo(nParameters,nParameters);  
  Kal_Eta.ResizeTo(nParameters,1);      
  Kal_V.ResizeTo(nParameters,nParameters);        
  Kal_A.ResizeTo(nParameters,nParameters);
  for(Int_t i = 0; i < 2; i++){
    Eta_Final[i].ResizeTo(nParameters,1);
    V_Final[i].ResizeTo(nParameters,nParameters);
  }
  for(Int_t i = 0; i < 4; i++) {
    Kal_D[i].ResizeTo(nParameters,1);
    for (Int_t j=0; j<nParameters; j++) Kal_D[i](j,0) = 0;
  }
  Kal_D[0](2,0)=1/TMath::Sqrt2();
  Kal_D[0](3,0)=-1/TMath::Sqrt2();
  Kal_D[1](2,0)=1/TMath::Sqrt2();
  Kal_D[1](3,0)=1/TMath::Sqrt2();
  Kal_D[2](2,0)=1;
  Kal_D[3](3,0)=1;
  NumberOfEvents = 0;
  
}

Track::~Track()
{ 
}

//////////////////////
// Steering routine //
//////////////////////
void Track::Reconstruct(Combination *jc, ChamberCollector *c, TRecoSpectrometerCandidate *t, Int_t flag)
{
  // Initialize common variables
  jCombination = jc;
  fChamber = c;
  fCandidate = t; 
  fIteration = flag;

  // Steps 
  Init();
  ExtractCoordinate();

  // Forward fit
  Fit_Direction = 0;
  MakeInitialFitMatrices();
  FitFullMNP33Forward();
  FinalForwardMatrices();

  // Backward fit
  Fit_Direction = 1;
  MakeInitialFitMatrices();
  FitFullMNP33Backward();
  FinalBackwardMatrices();

  // Chi2 computation
  Weigh_Matrices();
  fChi2 = chi2_calc();

  // Computation of track time using leadings
  fLTime = ComputeTrackTimeLeading();

  // Store tracks
  if (fIteration==2) StoreTrack(0); 

  NumberOfEvents++;  
}


/////////////////////////
// Matrices definition //
/////////////////////////
void Track::Init()
{
  // Initialize some variables
  fNResidualHits = jCombination->GetNTotalHits();
  fNChambers = jCombination->GetType();
  for (Int_t iCh=0; iCh<4; iCh++) {
    fNTotalHitPerChamber[iCh] = 0;
    fN2HitClusterPerChamber[iCh] = 0;
    fN3HitClusterPerChamber[iCh] = 0;
    fNViewsPerChamber[iCh] = 0;
    fIdChamber[iCh] = -1;
  }

  // Clean matrices
//  Kal_Hit_Var.ResizeTo(fNResidualHits,9);
  Kal_Hit_Var.ResizeTo(fNResidualHits,10);
  Kal_Eta_EachHit_Forward.clear();
  Kal_Eta_EachHit_Backward.clear();
  Kal_V_EachHit_Forward.clear();
  Kal_V_EachHit_Backward.clear();
  V_Weighted.clear();
  Eta_Weighted.clear();
  V_Weighted.resize(fNResidualHits);
  Eta_Weighted.resize(fNResidualHits);
  for(Int_t i = 0; i < fNResidualHits; i++){
    V_Weighted[i].ResizeTo(5,5);
    Eta_Weighted[i].ResizeTo(5,1);
  }

  // Clean momentum
  fFinalMomentum = 0;
}

/////////////////////////////
// Matrices initialization //
/////////////////////////////
void Track::MakeInitialFitMatrices(){

  // Initialting the parameter
  switch (Fit_Direction) {

    case 0: // forward
    switch (fIteration) {
      case 0:
      fFastMomentum = jCombination->GetP();
      fFastThetaX = jCombination->GetThetaX();
      fFastThetaY = jCombination->GetThetaY();
      fFastX0 = jCombination->GetX0();
      fFastY0 = jCombination->GetY0();
      fFastZ0 = jCombination->GetZ0();
      fTTime = jCombination->GetTrailingTime();
      for (Int_t j=0; j<100; j++) fTimeCorX[j] = 0; // Correction for signal propagation
      StoreTrack(-1);
      break;

      default: // use results from backward fit to initialize forward fit
      fFastMomentum = 1/Eta_Final[0](4,0);
      fFastThetaX = Eta_Final[0](0,0);
      fFastThetaY = Eta_Final[0](1,0);
      fFastX0 = Eta_Final[0](2,0);
      fFastY0 = Eta_Final[0](3,0);
      fFastZ0 = fZRef_Upstream;
      break;  
    }
    Kal_Eta(2,0) = fFastX0+fFastThetaX*(Kal_Hit_Var(0,1)-fFastZ0);
    Kal_Eta(3,0) = fFastY0+fFastThetaY*(Kal_Hit_Var(0,1)-fFastZ0);
    break;

    case 1: // backward, use results from forward fit to initialize backward fit
    fFastMomentum = 1/Eta_Final[1](4,0);
    fFastThetaX = Eta_Final[1](0,0);
    fFastThetaY = Eta_Final[1](1,0);
    fFastX0 = Eta_Final[1](2,0);
    fFastY0 = Eta_Final[1](3,0);
    fFastZ0 = fZRef_Downstream;
    Kal_Eta(2,0) = fFastX0+fFastThetaX*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);
    Kal_Eta(3,0) = fFastY0+fFastThetaY*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);
    break;
  }
  Kal_Eta(0,0) = fFastThetaX;
  Kal_Eta(1,0) = fFastThetaY;
  Kal_Eta(4,0) = 1/fFastMomentum;

  
  // Initiating the covariance matrix
  Kal_V.Zero();
  switch (fIteration) {
    case 0:
    switch (Fit_Direction) {
      case 0: // forward
      Kal_V(4,4) =  0.01/(fFastMomentum*fFastMomentum); 
      Kal_V(0,0) = .1*.1;
      Kal_V(1,1) = .1*.1;
      Kal_V(2,2) = 2000.*2000.;
      Kal_V(3,3) = 2000.*2000.;
      break;
      case 1: // Backward
      Kal_V(4,4) = 2000*V_Final[1](4,4); 
      Kal_V(0,0) = 2000*V_Final[1](0,0);
      Kal_V(1,1) = 2000*V_Final[1](1,1);
      Kal_V(2,2) = 2000*V_Final[1](2,2);
      Kal_V(3,3) = 2000*V_Final[1](3,3);
      break; 
    }
    break;

    case 1:
    switch (Fit_Direction) {
      case 0: // forward
      Kal_V(4,4) = 200*V_Final[0](4,4); 
      Kal_V(0,0) = 200*V_Final[0](0,0);
      Kal_V(1,1) = 200*V_Final[0](1,1);
      Kal_V(2,2) = 200*V_Final[0](2,2);
      Kal_V(3,3) = 200*V_Final[0](3,3);
      break;
      case 1: // Backward
      Kal_V(4,4) = 200*V_Final[1](4,4); 
      Kal_V(0,0) = 200*V_Final[1](0,0);
      Kal_V(1,1) = 200*V_Final[1](1,1);
      Kal_V(2,2) = 200*V_Final[1](2,2);
      Kal_V(3,3) = 200*V_Final[1](3,3);
      break; 
    }
    break;

    case 2:
    switch (Fit_Direction) {
      case 0: // forward
      Kal_V(4,4) = 10*V_Final[0](4,4); 
      Kal_V(0,0) = 10*V_Final[0](0,0);
      Kal_V(1,1) = 10*V_Final[0](1,1);
      Kal_V(2,2) = 10*V_Final[0](2,2);
      Kal_V(3,3) = 10*V_Final[0](3,3);
      break;
      case 1: // Backward
      Kal_V(4,4) = 10*V_Final[1](4,4); 
      Kal_V(0,0) = 10*V_Final[1](0,0);
      Kal_V(1,1) = 10*V_Final[1](1,1);
      Kal_V(2,2) = 10*V_Final[1](2,2);
      Kal_V(3,3) = 10*V_Final[1](3,3);
      break; 
    }
    break;

    case 3: // not used
    switch (Fit_Direction) {
      case 0: // forward
      Kal_V(4,4) = 500*V_Final[0](4,4); 
      Kal_V(0,0) = 500*V_Final[0](0,0);
      Kal_V(1,1) = 500*V_Final[0](1,1);
      Kal_V(2,2) = 500*V_Final[0](2,2);
      Kal_V(3,3) = 500*V_Final[0](3,3);
      break;
      case 1: // Backward
      Kal_V(4,4) = 500*V_Final[1](4,4); 
      Kal_V(0,0) = 500*V_Final[1](0,0);
      Kal_V(1,1) = 500*V_Final[1](1,1);
      Kal_V(2,2) = 500*V_Final[1](2,2);
      Kal_V(3,3) = 500*V_Final[1](3,3);
      break; 
    }
    break;
  }

}

/////////////////
// Forward fit //
/////////////////
void Track::FitFullMNP33Forward()
{
  // Flags for forward fit
  KickDirection_Fit = 1;
  Hit_Count = 1;
  Hit_Added = 0;

  // Fit including fringe field 
  Int_t jfz = 0;
  Double_t zinit = fZcoorFringe[jfz]+fZMag;
  Double_t zstart = zinit;
  Double_t zstop = zstart;     
  Bool_t loop2 = true;
  while (loop2) {
    Double_t zhit = Kal_Hit_Var(Hit_Count-1,1);
    Kalman_Algorithm(1);
    if (zhit>=zinit) {  
      // BUG: missing transport from zinit to zhit within the fringe field between chamber 1 and 2 if chamber 1 is missing !
      zstart = zhit;
      break;
    }
    if (Kal_Hit_Var(Hit_Count,1)<=zinit) {
      Transport_free(zhit,Kal_Hit_Var(Hit_Count,1));
      Hit_Count++;
    } else {
      Transport_free(zhit,zinit);
      zstart = zinit;
      while (jfz<=13) { // fringe field between chamber 1 and 2
        if (jfz==12) break;
        if (jfz<13) zstop = fZcoorFringe[jfz+1]+fZMag;
        else zstop = fZcoorMNP33[0]+fZMag;
        Transport_magnet(jfz,zstart,zstop,1);
        zstart = zstop;
        jfz++;
      } 
      break;
    }
  }

  // MNP33
  Int_t jz = 0;
  while (jz<168) {
    if (jz==168) break;
    zstop = fZcoorMNP33[jz+1]+fZMag;
    if (zstop<zstart) {
      jz++;
      continue;
    }
    Double_t zhit = Kal_Hit_Var(Hit_Count,1);
    if (zhit<=zstop) {
      zstop = zhit;
      Transport_magnet(jz,zstart,zstop,0);
      Hit_Count++;
      Kalman_Algorithm(1);
      zstart = zstop;
      continue;      
    }
    Transport_magnet(jz,zstart,zstop,0);
    zstart = zstop;
    jz++;
  } 

  // Fringe field
  jfz = 16;
  while (jfz<29) {
    if (jfz==29) break;
    zstop = fZcoorFringe[jfz+1]+fZMag;
    Double_t zhit = Hit_Count<fNResidualHits ? Kal_Hit_Var(Hit_Count,1) : 999999.;
    if (zhit<=zstop) {
      zstop = zhit;
      Transport_magnet(jfz,zstart,zstop,1);
      Hit_Count++;
      Kalman_Algorithm(1);
      zstart = zstop;
      continue;      
    }
    Transport_magnet(jfz,zstart,zstop,1);
    zstart = zstop;
    jfz++;
  } 

  // Fit outside magnet region (in case of hits in chamber 4) 
  if (Hit_Count<fNResidualHits) {
    Transport_free(zstart,Kal_Hit_Var(Hit_Count,1));
    Hit_Count++;
    UpStream_ChamberID = 3; 
    DownStream_ChamberID = 3;
    Kalman_Algorithm(0);
  }

}

//////////////////
// Backward fit //
//////////////////
void Track::FitFullMNP33Backward()
{
  // Flags for forward fit
  KickDirection_Fit = -1;
  Hit_Count = fNResidualHits;
  Hit_Added = 0;

  // Fit chamber 4 down to the beginning of the fringe field region
  Bool_t loop1 = true;
  Double_t zinit = fZcoorFringe[29]+fZMag;
  while (loop1) {
    Double_t zhit = Kal_Hit_Var(Hit_Count-1,1);
    if (zhit<=zinit) { 
      Kalman_Algorithm(1);
      // BUG: missing transport from zinit to zhit within the fringe field between chamber 4 and 3 if chamber 4 is missing !
      zinit = zhit;
      break;
    }
    Kalman_Algorithm(1);
    if (Kal_Hit_Var(Hit_Count-2,1)>zinit) {
      Transport_free(zhit,Kal_Hit_Var(Hit_Count-2,1));
      Hit_Count--;
    } else {
      Transport_free(zhit,zinit);
      break;
    }
  }

  // Fringe field
  Int_t jfz = 29;
  Double_t zstart = zinit;
  Double_t zstop = zstart;     
  while(jfz>=17) {
    if (jfz==16) break;
    if (jfz>17) zstop = fZcoorFringe[jfz-1]+fZMag;
    else zstop = fZcoorMNP33[168]+fZMag;
    Double_t zhit = -9999;
    if (Hit_Count>=2) zhit = Kal_Hit_Var(Hit_Count-2,1);
    if (zstop<=zhit) {
      zstop = zhit;
      Transport_magnet(jfz,zstart,zstop,1);
      Hit_Count--;
      Kalman_Algorithm(1);
      zstart = zstop;
      continue;
    }
    Transport_magnet(jfz,zstart,zstop,1);
    zstart = zstop;
    jfz--;
  }

  // MNP33
  Int_t jz = 168;
  zstart = fZcoorMNP33[jz]+fZMag;
  zstop = zstart;
  while(jz>0) { 
    if (jz==0) break;
    zstop = fZcoorMNP33[jz-1]+fZMag;
    Double_t zhit = -9999;
    if (Hit_Count>=2) zhit = Kal_Hit_Var(Hit_Count-2,1);
    if (zstop<=zhit) {
      zstop = zhit;
      Transport_magnet(jz,zstart,zstop,0);
      Hit_Count--;
      Kalman_Algorithm(1);
      zstart = zstop;
      continue;
    }
    Transport_magnet(jz,zstart,zstop,0);
    zstart = zstop;
    jz--;
  }

  // Fringe field
  jfz = 14;
  while(jfz>0) {
    if (jfz==0) break;
    zstop = fZcoorFringe[jfz-1]+fZMag;
    Double_t zhit = -9999;
    if (Hit_Count>=2) zhit = Kal_Hit_Var(Hit_Count-2,1);
    if (zstop<=zhit) {
      zstop = zhit;
      Transport_magnet(jfz,zstart,zstop,1);
      Hit_Count--;
      Kalman_Algorithm(1);
      zstart = zstop;
      continue;
    }
    Transport_magnet(jfz,zstart,zstop,1);
    zstart = zstop;
    jfz--;
  }

  // Fit outside magnet region
  if (Hit_Count>=2) { 
    Transport_free(zstart,Kal_Hit_Var(Hit_Count-2,1));
    Hit_Count--;
    UpStream_ChamberID = 0;
    DownStream_ChamberID = 1;
    Kalman_Algorithm(0);
  }

}

//////////////////////
// Kalman Algorithm //
//////////////////////
Bool_t Track::Kalman_Algorithm(Int_t bflag){

  // Add hit
  viewID = Kal_Hit_Var(Hit_Count-1,0);
  Measured_Hit_Position_X = Kal_Hit_Var(Hit_Count-1,4);
  Kal_Sigma = SigmaRadius(Kal_Hit_Var(Hit_Count-1,5));
  
  // Using the initial matrices for the first hit added and using the calculated matrices to add on the hit recursively
  TMatrixD Kal_V_tmp, Kal_Eta_tmp;
  Kal_V_tmp.ResizeTo(Kal_V);
  Kal_Eta_tmp.ResizeTo(Kal_Eta);
  if(Hit_Added == 0 && Hit_Count == fNResidualHits){ 
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_prime.Zero(); 
  }else if(Hit_Added == 0 && Hit_Count == 1){
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_prime.Zero();
  }else{
    Kal_V_tmp = Kal_V_prime;
    Kal_Eta_tmp = Kal_Eta_prime;
  }

  // Adding the hit to the covariance matrix
  TMatrixD num1(Kal_D[viewID],TMatrixD::kTransposeMult,Kal_V_tmp);
  TMatrixD num2(Kal_D[viewID],TMatrixD::kMult,num1);
  Kal_V_prime.Mult(Kal_V_tmp,num2);
  TMatrixD deno1(Kal_V_tmp,TMatrixD::kMult,Kal_D[viewID]);
  TMatrixD deno2(Kal_D[viewID],TMatrixD::kTransposeMult,deno1);
  deno2(0,0) += Kal_Sigma*Kal_Sigma;
  Kal_V_prime *= -1/deno2(0,0);
  Kal_V_prime += Kal_V_tmp;

  //Adding the hit to the parameter matrix
  Kal_Eta_prime.Mult(Kal_V_prime,Kal_D[viewID]);
  Kal_Eta_prime *= (Measured_Hit_Position_X - dEta(viewID,Kal_Eta_tmp))/(Kal_Sigma*Kal_Sigma);
  Kal_Eta_prime += Kal_Eta_tmp;

  // Multiple scattering
  if(Hit_Added > 0) AddMultipleScattering();

  // Weighing matrices
  if (Fit_Direction == 0){ // Forward
    Kal_Eta_EachHit_Forward.push_back(Kal_Eta_prime);
    Kal_V_EachHit_Forward.push_back(Kal_V_prime);
  }
  if (Fit_Direction == 1){ // Backward
    Kal_Eta_EachHit_Backward.push_back(Kal_Eta_prime);
    Kal_V_EachHit_Backward.push_back(Kal_V_prime);
  }

  Hit_Added++; //counting the number of hits added to the track

  if (bflag) return false;

  // Transporting
  if ((Hit_Count > 1 && Fit_Direction == 1) && Kal_Hit_Var(Hit_Count-2, 2) >= UpStream_ChamberID  &&  Kal_Hit_Var(Hit_Count-2, 2) <= DownStream_ChamberID) Transport();
  if ((Hit_Count < fNResidualHits && Fit_Direction == 0) && Kal_Hit_Var(Hit_Count, 2) >= UpStream_ChamberID  &&  Kal_Hit_Var(Hit_Count, 2) <= DownStream_ChamberID) Transport();
  
  return false;
}

////////////////////////////////////
// Transportation without b field //
////////////////////////////////////
void Track::Transport()
{

  if(Fit_Direction == 0) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count,1);   // Forward
  if(Fit_Direction == 1) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count-2,1); // Backward

  // Translating Eta_prime
  Kal_Eta_prime(2,0) = Kal_Eta_prime(2,0)-Kal_Eta_prime(0,0)*Distance_ToNext_Hit; 
  Kal_Eta_prime(3,0) = Kal_Eta_prime(3,0)-Kal_Eta_prime(1,0)*Distance_ToNext_Hit;

  // Transforming the Kal_V (the covariance matrix)
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -Distance_ToNext_Hit;
  Kal_A(3,1) = -Distance_ToNext_Hit; 
  TMatrixD transp1(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A);
  Kal_V_prime.Mult(Kal_A,transp1);

  if(Fit_Direction == 0) Hit_Count++; // Forward 
  if(Fit_Direction == 1) Hit_Count--; // Backward

  Kalman_Algorithm(0);
}

////////////////////////////////////
// Transportation without b field //
////////////////////////////////////
void Track::Transport_free(Double_t zstart, Double_t zstop)
{
  Double_t dz = zstart-zstop;
  Kal_Eta_prime(2,0) = Kal_Eta_prime(2,0)-Kal_Eta_prime(0,0)*dz; 
  Kal_Eta_prime(3,0) = Kal_Eta_prime(3,0)-Kal_Eta_prime(1,0)*dz;
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -dz;
  Kal_A(3,1) = -dz; 
  TMatrixD transp1(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A);
  Kal_V_prime.Mult(Kal_A,transp1);
}

/////////////////////////////////
// Transportation with b field //
/////////////////////////////////
void Track::Transport_magnet(Int_t jz, Double_t zstart, Double_t zstop, Bool_t bfringe)
{
  // Initialization
  Double_t dz = zstart-zstop;
  TVector3 bsurface[2];
  Double_t zstepin = zstart;
  Double_t zstepout = zstop;
  switch (bfringe) {
    case 0:
    zstepin =  fZcoorMNP33[jz]+fZMag;
    zstepout = fZcoorMNP33[jz+KickDirection_Fit]+fZMag;
    break; 
    case 1:
    zstepin =  fZcoorFringe[jz]+fZMag;
    zstepout = fZcoorFringe[jz+KickDirection_Fit]+fZMag;
    break;
  }

  // X and Y coordinate at in surface, B cell identification, B extraction and computation
  Double_t xposin = Kal_Eta_prime(2,0);
  Double_t yposin = Kal_Eta_prime(3,0);
  Double_t xypos[2]; 
  xypos[0] = xposin+Kal_Eta_prime(0,0)*(zstepin-zstart);
  xypos[1] = yposin+Kal_Eta_prime(1,0)*(zstepin-zstart);
  bsurface[0] = ComputeBFieldAtXYLinear(bfringe,xypos,jz);

  // Compute xpos, zpos on the out surface (hyp. no mag field)
  Double_t xposout = Kal_Eta_prime(2,0)-Kal_Eta_prime(0,0)*dz;
  Double_t yposout = Kal_Eta_prime(3,0)-Kal_Eta_prime(1,0)*dz;
  xypos[0] = xposout+Kal_Eta_prime(0,0)*(zstepout-zstop); 
  xypos[1] = yposout+Kal_Eta_prime(1,0)*(zstepout-zstop);
  bsurface[1] = ComputeBFieldAtXYLinear(bfringe,xypos,jz+KickDirection_Fit);

  // B at Z
  Double_t zss[4];
  zss[0] = zstart;
  zss[1] = zstop;
  zss[2] = zstepin;
  zss[3] = zstepout;
  Double_t *baverage; baverage = ComputeBFieldAtZLinear(bfringe,zss,bsurface,jz);
  Double_t bmagx = baverage[0];
  Double_t bmagy = baverage[1];
  Double_t bmagz = baverage[2];

  // Momentum in 
  Double_t invp = Kal_Eta_prime(4,0);
  Double_t thex = Kal_Eta_prime(0,0);
  Double_t they = Kal_Eta_prime(1,0);
  Double_t ainvp = fabs(invp);
  Double_t vpinz = (1/ainvp)/sqrt(1.+thex*thex+they*they);
  Double_t vpinx = vpinz*thex;
  Double_t vpiny = vpinz*they;
  Double_t vpartdirx = thex;
  Double_t vpartdiry = they;
  Double_t vpartdirz = 0.;

  // Reference system rotation (y along B)
  Double_t baveragemag = sqrt(bmagx*bmagx+bmagy*bmagy+bmagz*bmagz);
  Double_t bperpx = -bmagz/baveragemag;
  Double_t bperpz = bmagx/baveragemag;
  if (baveragemag==0.0) bperpx=bperpz=0.;
  Double_t arg = bmagy/baveragemag;
  if (arg>1.0) arg = 1.0;
  if (arg<-1.0) arg = -1.0;
  Double_t alphab = TMath::ACos(arg);
  if (baveragemag<=0.0) alphab = 0;

  // Rotation of aphab around bperp
  Double_t bperpmag = sqrt(bperpx*bperpx+bperpz*bperpz);
  Double_t deltax = bperpx/bperpmag;
  Double_t deltaz = bperpz/bperpmag;
  Double_t cosa = TMath::Cos(alphab);
  Double_t sina = TMath::Sin(alphab);
  Double_t m11 = cosa+(1-cosa)*deltax*deltax;
  Double_t m12 = -sina*deltaz;
  Double_t m13 = (1-cosa)*deltax*deltaz;
  Double_t m21 = sina*deltaz;
  Double_t m22 = cosa;
  Double_t m23 = -sina*deltax;
  Double_t m31 = (1-cosa)*deltaz*deltax;
  Double_t m32 = sina*deltax;
  Double_t m33 = cosa+(1-cosa)*deltaz*deltaz;
  if (alphab==0.0 || bperpmag==0.0) {
    m11=m22=m33=1;
    m12=m13=m21=m23=m31=m32=0;
  }
  Double_t baveragex = m11*bmagx+m12*bmagy+m13*bmagz;
  Double_t baveragey = m21*bmagx+m22*bmagy+m23*bmagz;
  Double_t baveragez = m31*bmagx+m32*bmagy+m33*bmagz;
  Double_t pinx = m11*vpinx+m12*vpiny+m13*vpinz;
  Double_t piny = m21*vpinx+m22*vpiny+m23*vpinz;
  Double_t pinz = m31*vpinx+m32*vpiny+m33*vpinz; 
  Double_t partdirx = m11*vpartdirx+m12*vpartdiry+m13*vpartdirz;
  Double_t partdiry = m21*vpartdirx+m22*vpartdiry+m23*vpartdirz;
  Double_t partdirz = m31*vpartdirx+m32*vpartdiry+m33*vpartdirz;
  Double_t partposx = m11*xposout+m12*yposout;
  Double_t partposy = m21*xposout+m22*yposout;
  Double_t partposz = m31*xposout+m32*yposout;
  Double_t partposinx = m11*xposin+m12*yposin;
  Double_t partposiny = m21*xposin+m22*yposin;
  //Double_t partposinz = m31*xposin+m32*yposin;

  // Computation of ptkick in the rotated reference system
  Double_t ptkickx = 0.;
  Double_t ptkickz = 0.;
  Double_t posoutx = partposx;
  Double_t posouty = partposy;
  //Double_t posoutz = partposz;
  for (Int_t jiter=0; jiter<2; jiter++) {
    Double_t ddx = posoutx-partposinx;
    Double_t ddy = posouty-partposiny;
    Double_t path = sqrt(ddx*ddx+ddy*ddy+dz*dz);
    Double_t bpcrossx = ainvp*(piny*baveragez-baveragey*pinz);
    Double_t bpcrossz = ainvp*(pinx*baveragey-baveragex*piny);
    ptkickx = -1000*fEC*bpcrossx*path;
    ptkickz = -1000*fEC*bpcrossz*path;
    posoutx = partposx+KickDirection_Fit*0.5*dz*ptkickx*invp;
    //posoutz = partposz+KickDirection_Fit*0.5*dz*ptkickz*invp;
  }
  Double_t ptkx = KickDirection_Fit*ptkickx*invp;
  Double_t ptkz = KickDirection_Fit*ptkickz*invp;
  partposx += ptkx*0.5*dz;
  partposz += ptkz*0.5*dz;
  Double_t vpartposx = partposx;
  Double_t vpartposy = partposy;
  Double_t vpartposz = partposz;
  partdirx -= ptkx;
  partdirz -= ptkz;
  vpartdirx = partdirx;
  vpartdiry = partdiry;
  vpartdirz = partdirz;

  // Derivative in the rotated system with respect 1/p
  Double_t tkx = ptkx/invp; 
  Double_t tkz = ptkz/invp; 
  Double_t vderivposx = tkx*0.5*dz;
  Double_t vderivposz = tkz*0.5*dz;

  // Rotate back in the NA62 reference system
  cosa = TMath::Cos(-alphab);
  sina = TMath::Sin(-alphab);
  m11 = cosa+(1-cosa)*deltax*deltax;
  m12 = -sina*deltaz;
  m13 = (1-cosa)*deltax*deltaz;
  m21 = sina*deltaz;
  m22 = cosa;
  m23 = -sina*deltax;
  m31 = (1-cosa)*deltaz*deltax;
  m32 = sina*deltax;
  m33 = cosa+(1-cosa)*deltaz*deltaz;
  if (alphab==0.0 || bperpmag==0.0) {
    m11=m22=m33=1;
    m12=m13=m21=m23=m31=m32=0;
  }

  // New track parameters
  Kal_Eta_prime(0,0) = m11*vpartdirx+m12*vpartdiry+m13*vpartdirz;
  Kal_Eta_prime(1,0) = m21*vpartdirx+m22*vpartdiry+m23*vpartdirz;
  Kal_Eta_prime(2,0) = m11*vpartposx+m12*vpartposy+m13*vpartposz;  
  Kal_Eta_prime(3,0) = m21*vpartposx+m22*vpartposy+m23*vpartposz;  

  // Covariance matrix
  Kal_A.UnitMatrix();
  Kal_A(0,4) = -m11*tkx-m13*tkz;
  Kal_A(1,4) = -m21*tkx-m23*tkz;
  Kal_A(2,4) = m11*vderivposx+m13*vderivposz;
  Kal_A(3,4) = m21*vderivposx+m23*vderivposz;
  Kal_A(2,0) = -dz;
  Kal_A(3,1) = -dz;
//  TMatrixD transp1(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A);
//  Kal_V_prime.Mult(Kal_A,transp1);
  Kal_V_prime.Mult(Kal_A,TMatrixD(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A));
}

TVector3 Track::ComputeBFieldAtXYLinear(Int_t bfringe, Double_t *xypos, Int_t jz) 
{
  // B at xpos, ypos
  Int_t idmax[2]; idmax[0] = 25; idmax[1] = 16;
  Double_t xypass[2]; xypass[0] = 80.; xypass[1] = 100.;
  Double_t xyoffs[2]; xyoffs[0] = -1000.; xyoffs[1] = -800.;

  Double_t invaxypass; invaxypass = 1./fabs(xypass[bfringe]);
  Int_t jx; jx = (Int_t)(xypos[0]-xyoffs[bfringe])*invaxypass; 
  Int_t jy; jy = (Int_t)(xypos[1]-xyoffs[bfringe])*invaxypass; 
  Double_t dx; dx = fabs(xypos[0]-(xyoffs[bfringe]+jx*xypass[bfringe]))*invaxypass;
  Double_t dy; dy = fabs(xypos[1]-(xyoffs[bfringe]+jy*xypass[bfringe]))*invaxypass;

  Int_t kx(jx>=0?(jx<idmax[bfringe]?jx:idmax[bfringe]):0);
  Int_t kxnext(jx>=0?(kx<idmax[bfringe]?kx+1:kx):0);
  Int_t ky(jy>=0?(jy<idmax[bfringe]?jy:idmax[bfringe]):0);
  Int_t kynext(jy>=0?(ky<idmax[bfringe]?ky+1:ky):0);

  Double_t bxvalue(0);
  Double_t byvalue(0);
  Double_t bzvalue(0);
  switch (bfringe) {
    case 0:
    bxvalue = (1-dx-dy+dx*dy)*fBxMNP33[jz][kx][ky]+(dx-dx*dy)*fBxMNP33[jz][kxnext][ky]+(dy-dx*dy)*fBxMNP33[jz][kx][kynext]+dx*dy*fBxMNP33[jz][kxnext][kynext];
    byvalue = (1-dx-dy+dx*dy)*fByMNP33[jz][kx][ky]+(dx-dx*dy)*fByMNP33[jz][kxnext][ky]+(dy-dx*dy)*fByMNP33[jz][kx][kynext]+dx*dy*fByMNP33[jz][kxnext][kynext];
    bzvalue = (1-dx-dy+dx*dy)*fBzMNP33[jz][kx][ky]+(dx-dx*dy)*fBzMNP33[jz][kxnext][ky]+(dy-dx*dy)*fBzMNP33[jz][kx][kynext]+dx*dy*fBzMNP33[jz][kxnext][kynext];
    return TVector3(bxvalue,byvalue,bzvalue);
    break;

    case 1:
    bxvalue = (1-dx-dy+dx*dy)*fBxFringe[jz][kx][ky]+(dx-dx*dy)*fBxFringe[jz][kxnext][ky]+(dy-dx*dy)*fBxFringe[jz][kx][kynext]+dx*dy*fBxFringe[jz][kxnext][kynext];
    byvalue = (1-dx-dy+dx*dy)*fByFringe[jz][kx][ky]+(dx-dx*dy)*fByFringe[jz][kxnext][ky]+(dy-dx*dy)*fByFringe[jz][kx][kynext]+dx*dy*fByFringe[jz][kxnext][kynext];
    bzvalue = (1-dx-dy+dx*dy)*fBzFringe[jz][kx][ky]+(dx-dx*dy)*fBzFringe[jz][kxnext][ky]+(dy-dx*dy)*fBzFringe[jz][kx][kynext]+dx*dy*fBzFringe[jz][kxnext][kynext];
    return TVector3(bxvalue,byvalue,bzvalue);
    break;
  }

  return TVector3(0,0,0);
}

Double_t *Track::ComputeBFieldAtZLinear(Int_t bfringe, Double_t* zss, TVector3 *bsurface, Int_t jz)
{
  Double_t zpass; zpass = !bfringe ? fabs(1/(fZcoorMNP33[jz]-fZcoorMNP33[jz+KickDirection_Fit])) : fabs(1/(fZcoorFringe[jz]-fZcoorFringe[jz+KickDirection_Fit]));
  Double_t weight[2];
  weight[0] = fabs(zss[0]-zss[2])*zpass;
  weight[1] = fabs(zss[1]-zss[3])*zpass;
  Double_t b0[3]; 
  b0[0] = bsurface[0].X();  
  b0[1] = bsurface[0].Y();  
  b0[2] = bsurface[0].Z();  
  Double_t b1[3]; 
  b1[0] = bsurface[1].X();  
  b1[1] = bsurface[1].Y();  
  b1[2] = bsurface[1].Z();  
  static Double_t baverage[3];
  for (Int_t jc=0; jc<3; jc++) baverage[jc] = 0.5*((b0[jc]*(1-weight[0])+b1[jc]*weight[0])+(b0[jc]*weight[1]+b1[jc]*(1-weight[1])));
  return baverage;
}

/////////////////////////////////////////
// Compute best estimator at each step //
/////////////////////////////////////////
void Track::FinalForwardMatrices()
{
  V_Weighted[fNResidualHits-1] = Kal_V_EachHit_Forward[fNResidualHits-1];
  Eta_Weighted[fNResidualHits-1] = Kal_Eta_EachHit_Forward[fNResidualHits-1];
  Double_t deltaz = Kal_Hit_Var(fNResidualHits-1,1) - fZRef_Downstream;
  Eta_Final[1] = Eta_Weighted[fNResidualHits-1];
  Eta_Final[1](2,0) = Eta_Final[1](2,0) - Eta_Final[1](0,0)*deltaz;
  Eta_Final[1](3,0) = Eta_Final[1](3,0) - Eta_Final[1](1,0)*deltaz;
  V_Final[1] = V_Weighted[fNResidualHits-1];
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -deltaz;
  Kal_A(3,1) = -deltaz; 
  TMatrixD transp1(V_Final[1],TMatrixD::kMultTranspose,Kal_A);
  V_Final[1].Mult(Kal_A,transp1);
  fFinalMomentum += 1/Eta_Weighted[fNResidualHits-1](4,0);
}

void Track::FinalBackwardMatrices()
{
  V_Weighted[0] = Kal_V_EachHit_Backward[fNResidualHits-1];
  Eta_Weighted[0] = Kal_Eta_EachHit_Backward[fNResidualHits-1];
  Double_t deltaz = Kal_Hit_Var(0,1) - fZRef_Upstream;
  Eta_Final[0] = Eta_Weighted[0];
  Eta_Final[0](2,0) = Eta_Final[0](2,0) - Eta_Final[0](0,0)*deltaz;
  Eta_Final[0](3,0) = Eta_Final[0](3,0) - Eta_Final[0](1,0)*deltaz;
  V_Final[0] = V_Weighted[0];
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -deltaz;
  Kal_A(3,1) = -deltaz; 
  TMatrixD transp1(V_Final[0],TMatrixD::kMultTranspose,Kal_A);
  V_Final[0].Mult(Kal_A,transp1);
  fFinalMomentum += 1/Eta_Weighted[0](4,0);
}

void Track::Weigh_Matrices()
{
  for (Int_t k=1; k<fNResidualHits-1; k++) {
    TMatrixD V_inverse_b(TMatrixD::kInverted,Kal_V_EachHit_Backward[(fNResidualHits)-(k+1)]);    
    TMatrixD V_inverse_f(TMatrixD::kInverted,Kal_V_EachHit_Forward[k]);  
    V_Weighted[k].Plus(V_inverse_b,V_inverse_f);
    V_Weighted[k].InvertFast(); 
    TMatrixD VEta_b(V_inverse_b,TMatrixD::kMult,Kal_Eta_EachHit_Backward[(fNResidualHits)-(k+1)]);
    TMatrixD VEta_f(V_inverse_f,TMatrixD::kMult,Kal_Eta_EachHit_Forward[k]);
    TMatrixD EtaSum(VEta_b,TMatrixD::kPlus,VEta_f);
    Eta_Weighted[k].Mult(V_Weighted[k],EtaSum);
    fFinalMomentum += 1/Eta_Weighted[k](4,0); 
  }
}

//////////////////
// Compute chi2 //
//////////////////
Double_t Track::chi2_calc(){

  Double_t chi2 = 0;
  Bool_t makeHistos = false;
  if (fIteration == 2 || fIsMuonRun) makeHistos = true;

  for(Int_t i = 0; i<fNResidualHits; i++){
    Double_t sigma = SigmaRadius(Kal_Hit_Var(i,5));
    viewID = Kal_Hit_Var(i,0);
    Double_t residual = (Kal_Hit_Var(i,4) - dEta(viewID,Eta_Weighted[i]));
    Double_t Resolution_Value = Resolution(V_Weighted[i],viewID);
    chi2 += TMath::Power(residual, 2)/(TMath::Power(Resolution_Value, 2) + TMath::Power(sigma, 2));

    Double_t angle = 0.;
    Double_t thx = Eta_Weighted[i](0,0);
    Double_t thy = Eta_Weighted[i](1,0);
    Double_t posx = Eta_Weighted[i](2,0);
    Double_t posy = Eta_Weighted[i](3,0);
    Double_t orthcoord = 0.;
    if (viewID==0) { orthcoord = (posx+posy)/TMath::Sqrt2(); angle = (thx-thy)/TMath::Sqrt2(); }
    if (viewID==1) { orthcoord = -(posx-posy)/TMath::Sqrt2(); angle = (thx+thy)/TMath::Sqrt2(); }
    if (viewID==2) { orthcoord = posy; angle = thx; }
    if (viewID==3) { orthcoord = posx; angle = thy; }

    if (fIteration==1 && fXTMode==1){
      fTimeCorX[i] = SpectrometerParameters::GetInstance()->GetXTDependence(viewID,orthcoord);
    }

    if (!makeHistos) continue;

    Int_t plane = Kal_Hit_Var(i,6)/122;
    Int_t viewplaneID = plane/4;
    Int_t channelID = Kal_Hit_Var(i,6);
    if (fNChambers==4) {
      fHResidual->Fill(residual,Kal_Hit_Var(i,6));
      fHResidualvsR->Fill(residual,Kal_Hit_Var(i,9));
      if (channelID==304) fHResidualvsR_304->Fill(residual,Kal_Hit_Var(i,9));
      if (channelID==313) fHResidualvsR_313->Fill(residual,Kal_Hit_Var(i,9));
      if (channelID==2379) fHResidualvsR_2379->Fill(residual,Kal_Hit_Var(i,9));
      if (channelID==2388) fHResidualvsR_2388->Fill(residual,Kal_Hit_Var(i,9));
    }
    Double_t diffstraw = Kal_Hit_Var(i,7)-dEta(viewID,Eta_Weighted[i]);
    fHStrawPos->Fill(diffstraw,Kal_Hit_Var(i,6));
    fHStrawPosVSAngle[viewplaneID]->Fill(diffstraw,angle);
    fHTimeCor->Fill(orthcoord,Kal_Hit_Var(i,8));
    fHTimeCorPerView[viewplaneID]->Fill(orthcoord,Kal_Hit_Var(i,8));

  }

  chi2 /= (fNResidualHits-5); //chi2 = (N-n-1)^(-1) * sum(Residue^2 / Sigma ^2) , with N = total hits, n = # parameters fitted
  return chi2;
}

// Compute track time based on the leading time hit information
Double_t Track::ComputeTrackTimeLeading()
{
  double time = 0;
  if (fNResidualHits == 0) return time;
  for(Int_t ihit = 0; ihit < fNResidualHits; ihit++) {
    Int_t viewID = Kal_Hit_Var(ihit, 0);
    Double_t radiusFit = TMath::Abs(dEta(viewID, Eta_Weighted[ihit]) - Kal_Hit_Var(ihit, 7));
    Double_t driftTime = Kal_Hit_Var(ihit, 8);
    time+= fRTdependence->GetX(radiusFit) - driftTime;
  }
  return (time / fNResidualHits);
}

Double_t Track::Resolution(TMatrixD covariance,Double_t view){
  if (view==1) {
    return  TMath::Sqrt(covariance(2,2)/2+covariance(3,3)/2);
  }else if(view==0){
    return TMath::Sqrt(covariance(2,2)/2+covariance(3,3)/2);
  }else if(view==2 || view==3){
    return TMath::Sqrt(covariance(view,view));
  }else{ return 0;}
}

Double_t Track::dEta(Int_t view,TMatrixD eta){
  if (view==1){
    return (eta(3,0)+eta(2,0))/TMath::Sqrt2();
  }else if(view==0){
    return (eta(2,0)-eta(3,0))/TMath::Sqrt2();
  }else if(view==2 || view==3){
    return eta(view,0);
  }else{ return 0;}
}

void Track::StoreTrack(Int_t type)
{
  if (type==1) return;

  if (type==-1){
    fCandidate->SetSlopeXBeforeFit(fFastThetaX);     
    fCandidate->SetSlopeYBeforeFit(fFastThetaY);     
    fCandidate->SetPositionBeforeFit(TVector3(fFastX0,fFastY0,fFastZ0));
    fCandidate->SetMomentumBeforeFit(fFastMomentum);
    fCandidate->SetTime(fTTime-155.5);
    fCandidate->SetCombinationTotalQuality(jCombination->GetQuality());
    fCandidate->SetCombinationHoughQuality(jCombination->GetHDelta());
    return;
  }

  // Define variables
  Double_t thetaX_up = Eta_Final[0](0,0);
  Double_t thetaY_up = Eta_Final[0](1,0);
  Double_t posX_up = Eta_Final[0](2,0);
  Double_t posY_up = Eta_Final[0](3,0);
  Double_t mom_up = !fIsMuonRun ? 1/Eta_Final[0](4,0) : 30000.;
  Int_t charge = !fIsMuonRun ? fabs(mom_up)/mom_up : 0.;
  Double_t thetaX_dw = Eta_Final[1](0,0);
  Double_t thetaY_dw = Eta_Final[1](1,0);
  Double_t posX_dw = Eta_Final[1](2,0);
  Double_t posY_dw = Eta_Final[1](3,0);
  //Double_t mom_dw = !fIsMuonRun ? 1/Eta_Final[1](4,0) : 30000.;

  // Store variables
  fCandidate->SetSlopeXBeforeMagnet(thetaX_up);
  fCandidate->SetSlopeYBeforeMagnet(thetaY_up);
  fCandidate->SetPositionBeforeMagnet(TVector3(posX_up,posY_up,fZRef_Upstream));
//  fCandidate->SetMomentum(fabs(mom_up));
  if (!fIsMuonRun) fCandidate->SetMomentum(fabs(fFinalMomentum/fNResidualHits));
  else fCandidate->SetMomentum(fabs(mom_up));
  fCandidate->SetSlopeXAfterMagnet(thetaX_dw);
  fCandidate->SetSlopeYAfterMagnet(thetaY_dw);
  fCandidate->SetPositionAfterMagnet(TVector3(posX_dw,posY_dw,fZRef_Downstream));
  fCandidate->SetCharge(charge);
  fCandidate->SetChi2(fChi2);
  fCandidate->SetLeadingTime(fLTime);
  fCandidate->SetNChambers(fNChambers);
  Int_t nTotalHits = 0;
  for (Int_t iCha=0; iCha<4; iCha++) {
    fCandidate->SetChamberId(iCha,fIdChamber[iCha]);
    fCandidate->SetNTotalHitPerChamber(iCha,fNTotalHitPerChamber[iCha]);
    fCandidate->SetNViewsPerChamber(iCha,fNViewsPerChamber[iCha]);
    fCandidate->SetN2HitClusterPerChamber(iCha,fN2HitClusterPerChamber[iCha]);
    fCandidate->SetN3HitClusterPerChamber(iCha,fN3HitClusterPerChamber[iCha]);
    nTotalHits += fNTotalHitPerChamber[iCha];
  }
  for (Int_t iTrackHit=0; iTrackHit<nTotalHits; iTrackHit++) fCandidate->AddHit(Kal_Hit_Var(iTrackHit,3));
  if (!fIsMuonRun) {
    for (Int_t jx=0; jx<5; jx++) {
      for (Int_t jy=0; jy<5; jy++) fCandidate->SetCovariance(jx,jy,V_Final[type](jx,jy));
    }
  }
}

void Track::ExtractCoordinate()
{
  // Extract coordinate
  Int_t jHitForFit = 0;

  // Chamber hit 
  for (Int_t jj=0; jj<4; jj++) {
    Int_t chamberHitId = jCombination->GetChamberHitId(jj); // Get the Id of the chamber-hit of chamber jj (1 chamber-hit per chamber in each combination) 
    if (chamberHitId==-1) continue;
    fIdChamber[jj] = jCombination->GetChamberId(jj);
    ChamberHitCollector *thisChamber = fChamber->at(jj); // Get the pointer to the chamber-hit-collector of chamber jj 
    std::vector<Intersection>::iterator thisChamberHit = thisChamber->GetHit(chamberHitId); // Get the pointer to the chamber-hit of chamber jj
 
    // A chamber hit is done by view-clusters. Therefore loop over the its view-clusters.
    // Extract coordinates (N.B. GetNCluster is the size of the ClusterId of an Intersection, therefore the idview = -1 are not considered by default)
    for (UInt_t jclust=0; jclust<thisChamberHit->GetNCluster(); jclust++) {
      Int_t idview = thisChamberHit->GetViewId(jclust); // View ID of a chamber hit cluster (set in ChamberHitCollector)
      Int_t idcluster = thisChamberHit->GetClusterId(jclust); // Cluster ID of a chamber hit cluster (set in ChamberHitCollector) 
      Cluster *thisCluster = thisChamber->GetView(idview)->Getcluster(idcluster); // Pointer to this cluster
      // A view-cluster is formed by straw hits. Therefore loop over the view hits.
      for (UInt_t jhit=0; jhit<thisCluster->GetNHit(); jhit++) {
        TRecoSpectrometerHit *thisHit = thisChamber->GetView(idview)->GetHit(thisCluster->GetHitId(jhit)); // Pointer to the straw hit forming the view-cluster
        Double_t zi = thisHit->GetPosition().Z();//ordering the views hit
        Double_t newxcoord = thisHit->GetLocalPosition().X(); 
        Double_t xstraw = fSpecGeo->GetStraw(thisHit->GetChannelID())->GetLocalPosition().X();
        // Time signal propagation correction
////        if (fDATA && fIteration==2) {
////          Double_t newradius = SpectrometerParameters::GetInstance()->GetRTDependenceData((thisHit->GetDriftTime()-fTimeCorX[jHitForFit])/1000.);
        if (fRTMode) {
            Double_t newradius = SpectrometerParameters::GetInstance()->GetRTDependenceDataFull(thisHit->GetPlaneID()*122+thisHit->GetStrawID()+1,(thisHit->GetDriftTime()-fTimeCorX[jHitForFit]),(thisHit->GetEnergy()));
            if (newradius<0)   newradius = 0;
            if (newradius>4.9) newradius = 4.9;
        }
        if (!fRTMode) {
            Double_t newradius = SpectrometerParameters::GetInstance()->GetRTDependenceData((thisHit->GetDriftTime()-fTimeCorX[jHitForFit])/1000.);
            if (newradius<0)   newradius = 0;
            if (newradius>4.9) newradius = 4.9;
        }
////    Double_t xstraw = fSpecGeo->GetStraw(thisHit->GetChannelID())->GetLocalPosition().X();
////    Double_t oldradius = thisHit->GetLocalPosition().X()-xstraw;
////    Int_t sign = oldradius != 0 ? oldradius/fabs(oldradius) : 1;
////    newxcoord = xstraw+sign*newradius; // Recompute the position (differences only in case of relevant time correction from signal propagation)
////        }

        Kal_Hit_Var(jHitForFit,0) = idview;
        Kal_Hit_Var(jHitForFit,1) = zi;
        Kal_Hit_Var(jHitForFit,2) = jj;
        Kal_Hit_Var(jHitForFit,3) = thisHit->GetID();
        Kal_Hit_Var(jHitForFit,4) = newxcoord;
        Kal_Hit_Var(jHitForFit,5) = thisHit->GetWireDistance();
        Kal_Hit_Var(jHitForFit,6) = 122*(16*thisHit->GetChamberID()+4*thisHit->GetViewID()+2*thisHit->GetHalfViewID()+thisHit->GetPlaneID())+thisHit->GetStrawID();
        Kal_Hit_Var(jHitForFit,7) = xstraw;
        Kal_Hit_Var(jHitForFit,8) = thisHit->GetDriftTime()-fTimeCorX[jHitForFit];
        Kal_Hit_Var(jHitForFit,9) = thisHit->GetWireDistance();
        jHitForFit++;
        fNTotalHitPerChamber[jj]++;
      } // end loop over straw hit per cluster
      if (thisCluster->GetNHit()==2) fN2HitClusterPerChamber[jj]++;
      if (thisCluster->GetNHit()==3) fN3HitClusterPerChamber[jj]++;
    } // end loop over cluster per chamber hit
    fNViewsPerChamber[jj] += thisChamberHit->GetNCluster();
  } // end loop over chamber hit per combination  


  // Sort the hits
  for (Int_t i=0; i<jHitForFit; i++){
    for (Int_t j=i+1; j<jHitForFit; j++){
      if ( Kal_Hit_Var(j,1) < Kal_Hit_Var(i,1) ){ 
        Int_t viewstore1 =     Kal_Hit_Var(i,0);
        Int_t viewstore2 =     Kal_Hit_Var(j,0);
        Double_t zstore1 =     Kal_Hit_Var(i,1);
        Double_t zstore2 =     Kal_Hit_Var(j,1);
	Int_t chamberstore1 =  Kal_Hit_Var(i,2);
	Int_t chamberstore2 =  Kal_Hit_Var(j,2);
        Int_t recostore1 =     Kal_Hit_Var(i,3);
        Int_t recostore2 =     Kal_Hit_Var(j,3);
	Double_t xstore1 =     Kal_Hit_Var(i,4);
	Double_t xstore2 =     Kal_Hit_Var(j,4);
	Double_t wirestore1 =  Kal_Hit_Var(i,5);
	Double_t wirestore2 =  Kal_Hit_Var(j,5);
        Int_t idstore1 =       Kal_Hit_Var(i,6);
        Int_t idstore2 =       Kal_Hit_Var(j,6);
	Double_t strawpos1 =   Kal_Hit_Var(i,7);
	Double_t strawpos2 =   Kal_Hit_Var(j,7);
	Double_t drifttime1 =  Kal_Hit_Var(i,8);
	Double_t drifttime2 =  Kal_Hit_Var(j,8);
	Double_t radius1 =     Kal_Hit_Var(i,9);
	Double_t radius2 =     Kal_Hit_Var(j,9);
	Kal_Hit_Var(i,0) = viewstore2;
        Kal_Hit_Var(j,0) = viewstore1;
        Kal_Hit_Var(i,1) = zstore2;
        Kal_Hit_Var(j,1) = zstore1;
        Kal_Hit_Var(i,2) = chamberstore2;
        Kal_Hit_Var(j,2) = chamberstore1;
        Kal_Hit_Var(i,3) = recostore2;
        Kal_Hit_Var(j,3) = recostore1;
        Kal_Hit_Var(i,4) = xstore2;
        Kal_Hit_Var(j,4) = xstore1;
	Kal_Hit_Var(i,5) = wirestore2;
        Kal_Hit_Var(j,5) = wirestore1;
        Kal_Hit_Var(i,6) = idstore2;
        Kal_Hit_Var(j,6) = idstore1;
	Kal_Hit_Var(i,7) = strawpos2;
	Kal_Hit_Var(j,7) = strawpos1;
	Kal_Hit_Var(i,8) = drifttime2;
	Kal_Hit_Var(j,8) = drifttime1;
	Kal_Hit_Var(i,9) = radius2;
	Kal_Hit_Var(j,9) = radius1;
      }
    }
  } 
  fNResidualHits = jHitForFit; 
}


Double_t Track::SigmaRadius(Double_t radius) {
  Double_t sigRad1 = 0.7-0.363*radius;
  Double_t sigRad2 = 0.476-0.147*radius+0.0092*radius*radius+0.00135*radius*radius*radius;
  Double_t sigRad3 = 0.1218;
  if (radius==0) return 5./sqrt(12.);
  if (radius<1 && radius>0) return sqrt(sigRad1*sigRad1);
  if (radius>=1 && radius<4) return sqrt(sigRad2*sigRad2);
  if (radius>=4) return sqrt(sigRad3*sigRad3);
  return 0.;
}

void Track::AddMultipleScattering(){
  Double_t delta = 0;
  if (!fIsMuonRun) {
    delta = 2*13.6*fabs(Kal_Eta_prime(4,0))*sqrt(0.0003)*(1+.038*log(0.0003)); // MS gaussian approx., not the best
  }
  else delta = 2*(13.6/30000.)*sqrt(0.0003)*(1+.038*log(0.0003)); 
  Kal_V_prime(0,0) += delta*delta;
  Kal_V_prime(1,1) += delta*delta;
}


void Track::InitHistograms() 
{
  fHResidual = new TH2F("HResidual","",100,-2.5,2.5,7808,0,7808); 
  fHResidualvsR = new TH2F("HResidualvsR","",100,-2.5,2.5,100,0,10.); 
  fHResidualvsR_304 = new TH2F("HResidualvsR_304","",100,-2.5,2.5,100,0,10.);
  fHResidualvsR_313 = new TH2F("HResidualvsR_313","",100,-2.5,2.5,100,0,10.);
  fHResidualvsR_2379 = new TH2F("HResidualvsR_2379","",100,-2.5,2.5,100,0,10.);
  fHResidualvsR_2388 = new TH2F("HResidualvsR_2388","",100,-2.5,2.5,100,0,10.);
  fHStrawPos = new TH2F("HStrawPos","",100,-10,10,7808,0,7808); 
  fHTimeCor = new TH2F("HTimeCor","",52,-1050,1050,200,0,200); 

  for (Int_t j=0; j<16; j++) {
    TString namehistoTimeCor;
    TString namehistoStrawPos;
    namehistoStrawPos.Form("HStrawPosVsAngle%d",j);
    fHStrawPosVSAngle[j]= new TH2F(namehistoStrawPos.Data(),"",200,-10,10,10,-0.01,0.01); 
    namehistoTimeCor.Form("HTimeCorPerPlane%d",j);
    fHTimeCorPerView[j] = new TH2F(namehistoTimeCor.Data(),"",52,-1050,1050,200,0,200);
  }
}

void Track::SaveHistograms()
{
  if (fHResidual) fHResidual->Write();
  if (fHResidualvsR) fHResidualvsR->Write();
  if (fHResidualvsR_304) fHResidualvsR_304->Write();
  if (fHResidualvsR_313) fHResidualvsR_313->Write();
  if (fHResidualvsR_2379) fHResidualvsR_2379->Write();
  if (fHResidualvsR_2388) fHResidualvsR_2388->Write();
  if (fHStrawPos) fHStrawPos->Write();
  if (fHTimeCor) fHTimeCor->Write();
  for (Int_t j=0; j<16; j++) {
    if (fHStrawPosVSAngle[j]) fHStrawPosVSAngle[j]->Write();
    if (fHTimeCorPerView[j])  fHTimeCorPerView[j]->Write();
  }
}


// MUON RUN ///////////////////////////////////////////////////////////////////////////////////////////////

void Track::ReconstructMuonRun(Combination *jc, ChamberCollector *c, TRecoSpectrometerCandidate *t) 
{
  jCombination = jc;
  fChamber = c;
  fCandidate = t; 

  InitMuonRun();
  ExtractCoordinate();

  MakeInitialFitMatricesMuonRun();
  FitMuonRun();
  fChi2 = chi2_calc();
  fLTime = ComputeTrackTimeLeading();
  Transport_To_ZRef_MuonRun();

  StoreTrack(0); 
}

void Track::InitMuonRun()
{
  // Initialize some variables
  fNResidualHits = jCombination->GetNTotalHits();
  fNChambers = jCombination->GetType();
  for (Int_t iCh=0; iCh<4; iCh++) {
    fNTotalHitPerChamber[iCh] = 0;
    fN2HitClusterPerChamber[iCh] = 0;
    fN3HitClusterPerChamber[iCh] = 0;
    fNViewsPerChamber[iCh] = 0;
    fIdChamber[iCh] = -1;
  }

  // Clean matrices
  Kal_Hit_Var.ResizeTo(fNResidualHits,10);
  Kal_Eta_EachHit_Forward.clear();
  Kal_Eta_EachHit_Backward.clear();
  Kal_V_EachHit_Forward.clear();
  Kal_V_EachHit_Backward.clear();
  V_Weighted.clear();
  Eta_Weighted.clear();
  V_Weighted.resize(fNResidualHits);
  Eta_Weighted.resize(fNResidualHits);
  for(Int_t i = 0; i < fNResidualHits; i++){
    V_Weighted[i].ResizeTo(4,4);
    Eta_Weighted[i].ResizeTo(4,1);
  }
}

void Track::MakeInitialFitMatricesMuonRun()
{
  // Initial values
  fFastThetaX = jCombination->GetThetaX();
  fFastThetaY = jCombination->GetThetaY();
  fFastX0 = jCombination->GetX0();
  fFastY0 = jCombination->GetY0();
  fFastZ0 = jCombination->GetZ0();
  fTTime = jCombination->GetTrailingTime();
  StoreTrack(-1); 

  // Eta
  Kal_Eta(0,0)=fFastThetaX;
  Kal_Eta(1,0)=fFastThetaY;
  Kal_Eta(2,0)=fFastX0+fFastThetaX*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);
  Kal_Eta(3,0)=fFastY0+fFastThetaY*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);

  // Covariance matrix
  Kal_V.Zero();
  Kal_V(0,0)=.001*.001;
  Kal_V(1,1)=.001*.001;
  Kal_V(2,2)=20.*20.;
  Kal_V(3,3)=20.*20.;
  Kal_V *= 200;
}

void Track::FitMuonRun()
{
  UpStream_ChamberID = 0; //chamber 1
  DownStream_ChamberID = 3; //chamber 4

  // BACKWARDS FIT
  Fit_Direction = 0;
  Hit_Count = fNResidualHits;
  Hit_Added = 0;
  badfit = Kalman_Algorithm_MuonRun();
  if(badfit==true) std::cout << "<<<<<<<FIT ERROR>>>>>>> @Event: " << std::endl;
  badfit = true;

  // FORWARD FIT
  Fit_Direction = 1;
  Hit_Count = 1;
  Hit_Added = 0;
  badfit = Kalman_Algorithm_MuonRun();
  if(badfit == true) std::cout << "<<<<<<<FIT ERROR>>>>>>> @Event: " << std::endl;
  badfit = true;
}

Bool_t Track::Kalman_Algorithm_MuonRun()
{
  // ADD HIT
  viewID = Kal_Hit_Var(Hit_Count-1,0);
  Measured_Hit_Position_X = Kal_Hit_Var(Hit_Count-1,4);
  Kal_Sigma = SigmaRadius(Kal_Hit_Var(Hit_Count-1,5));

  //Using the initial matrices for the first hit added
  //and using the calculated matrices to add on the hit recursively
  TMatrixD Kal_V_tmp, Kal_Eta_tmp;
  Kal_V_tmp.ResizeTo(Kal_V);
  Kal_Eta_tmp.ResizeTo(Kal_Eta);
  if(Hit_Added == 0 && Hit_Count == fNResidualHits){ 
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_prime.Zero(); 
  }else if(Hit_Added == 0 && Hit_Count == 1){ //needed to alter the initial conditions for the forward fit. 
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_tmp(2,0) = fFastX0+fFastThetaX*(Kal_Hit_Var(Hit_Count-1,1)-fFastZ0);
    Kal_Eta_tmp(3,0) = fFastY0+fFastThetaY*(Kal_Hit_Var(Hit_Count-1,1)-fFastZ0);
    Kal_Eta_prime.Zero();
  }else{
    Kal_V_tmp = Kal_V_prime;
    Kal_Eta_tmp = Kal_Eta_prime;
  }

  // WEIGHING MATRICES
  if (Fit_Direction==0){
    Kal_Eta_EachHit_Backward.push_back(Kal_Eta_prime);
    Kal_V_EachHit_Backward.push_back(Kal_V_prime);
  }
  if (Fit_Direction==1){
    Kal_Eta_EachHit_Forward.push_back(Kal_Eta_prime);
    Kal_V_EachHit_Forward.push_back(Kal_V_prime);
    Weigh_Matrices_MuonRun();
  }
  
  // Adding the hit to the covariance matrix
  TMatrixD num1(Kal_D[viewID],TMatrixD::kTransposeMult,Kal_V_tmp);
  TMatrixD num2(Kal_D[viewID],TMatrixD::kMult,num1);
  Kal_V_prime.Mult(Kal_V_tmp,num2);
  TMatrixD deno1(Kal_V_tmp,TMatrixD::kMult,Kal_D[viewID]);
  TMatrixD deno2(Kal_D[viewID],TMatrixD::kTransposeMult,deno1);
  deno2(0,0) += Kal_Sigma*Kal_Sigma;
  Kal_V_prime *= -1/deno2(0,0);
  Kal_V_prime += Kal_V_tmp;

  // Adding the hit to the parameter matrix
  Kal_Eta_prime.Mult(Kal_V_prime,Kal_D[viewID]);
  Kal_Eta_prime *= (Measured_Hit_Position_X - dEta(viewID,Kal_Eta_tmp))/(Kal_Sigma*Kal_Sigma);
  Kal_Eta_prime += Kal_Eta_tmp;
  
  // MULTIPLE SCATTERING
  if(Hit_Added>0) AddMultipleScattering();

  //counting the number of hits added to the track
  Hit_Added++;

  // TRANSPORTING
  if ((Hit_Count>1 && Fit_Direction==0) && Kal_Hit_Var(Hit_Count-2,2)>=UpStream_ChamberID && Kal_Hit_Var(Hit_Count-2,2)<=DownStream_ChamberID) Transport_MuonRun();
  if ((Hit_Count<fNResidualHits && Fit_Direction==1) && Kal_Hit_Var(Hit_Count,2)>=UpStream_ChamberID && Kal_Hit_Var(Hit_Count,2)<=DownStream_ChamberID) Transport_MuonRun();
  
  return false;
}

void Track::Transport_MuonRun()
{
  if(Fit_Direction == 0) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count-2,1);
  if(Fit_Direction == 1) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count,1);

  // Translating Eta_prime
  Kal_Eta_prime(2,0) = Kal_Eta_prime(2,0)-Kal_Eta_prime(0,0)*Distance_ToNext_Hit; 
  Kal_Eta_prime(3,0) = Kal_Eta_prime(3,0)-Kal_Eta_prime(1,0)*Distance_ToNext_Hit;

  // Transforming the Kal_V (the covariance matrix)
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -Distance_ToNext_Hit;
  Kal_A(3,1) = -Distance_ToNext_Hit; 
  TMatrixD transp1(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A);
  Kal_V_prime.Mult(Kal_A,transp1);
  
  //Decrementing our hit_counter for the backward fit
  if(Fit_Direction == 0) Hit_Count--; 
    
  // Incrementing our hit_counter for the forward fit
  if(Fit_Direction == 1) Hit_Count++; 

  // Add hit  
  Kalman_Algorithm_MuonRun();
}

void Track::Weigh_Matrices_MuonRun()
{
  if(Hit_Added==0){
    V_Weighted[Hit_Added] = Kal_V_EachHit_Backward[fNResidualHits-1];
    Eta_Weighted[Hit_Added] = Kal_Eta_EachHit_Backward[fNResidualHits-1];
  }
  
  if(Hit_Added>0 && Hit_Added<fNResidualHits-1){
    TMatrixD V_inverse_b(TMatrixD::kInverted,Kal_V_EachHit_Backward[(fNResidualHits)-(Hit_Added+1)]);    
    TMatrixD V_inverse_f(TMatrixD::kInverted,Kal_V_EachHit_Forward[Hit_Added]);  
    V_Weighted[Hit_Added].Plus(V_inverse_b,V_inverse_f);
    V_Weighted[Hit_Added].InvertFast(); 
    TMatrixD VEta_b(V_inverse_b,TMatrixD::kMult,Kal_Eta_EachHit_Backward[(fNResidualHits)-(Hit_Added+1)]);
    TMatrixD VEta_f(V_inverse_f,TMatrixD::kMult,Kal_Eta_EachHit_Forward[Hit_Added]);
    TMatrixD EtaSum(VEta_b,TMatrixD::kPlus,VEta_f);
    Eta_Weighted[Hit_Added].Mult(V_Weighted[Hit_Added],EtaSum);
  }
  
  if(Hit_Added == fNResidualHits-1){
     V_Weighted[Hit_Added] = Kal_V_EachHit_Forward[fNResidualHits-1];
     Eta_Weighted[Hit_Added] = Kal_Eta_EachHit_Forward[fNResidualHits-1];
  }
}

void Track::Transport_To_ZRef_MuonRun()
{
  
  for(Int_t iDirection = 0; iDirection<2; iDirection++){
    Int_t POI;
    Double_t ZReference;
    iDirection == 0 ? POI = 0 : POI = fNResidualHits-1;
    iDirection == 0 ? ZReference = fZRef_Upstream : ZReference = fZRef_Downstream;
    Distance_ToNext_Hit =   Kal_Hit_Var(POI,1) - ZReference;
    Eta_Final[iDirection] = Eta_Weighted[POI];    
    Eta_Final[iDirection](2,0) = Eta_Final[iDirection](2,0) - Eta_Final[iDirection](0,0)*Distance_ToNext_Hit;
    Eta_Final[iDirection](3,0) = Eta_Final[iDirection](3,0) - Eta_Final[iDirection](1,0)*Distance_ToNext_Hit;
    
    V_Final[iDirection] = V_Weighted[POI];
    Kal_A.UnitMatrix();
    Kal_A(2,0) = -Distance_ToNext_Hit;
    Kal_A(3,1) = -Distance_ToNext_Hit; 
    TMatrixD transp1(V_Final[iDirection],TMatrixD::kMultTranspose,Kal_A);
    V_Final[iDirection].Mult(Kal_A,transp1); 
  }
}

// Analytic ///////////////////////////////////////////////////////////////////////////////////////////////

void Track::ReconstructSimple(Combination *jc, ChamberCollector *c, TRecoSpectrometerCandidate *t) 
{
  jCombination = jc;
  fChamber = c;
  fCandidate = t; 
  if (jCombination->GetType()<4) return;

  InitSimple();
  ExtractCoordinate();

  MakeInitialFitMatricesSimple();
  FitSimple();
////  chi2_calc();
////  Transport_To_ZRef_Simple();
////
////  StoreTrack(0); 
}

void Track::InitSimple()
{
  // Initialize some variables
  fNResidualHits = jCombination->GetNTotalHits();
  fNChambers = jCombination->GetType();
  for (Int_t iCh=0; iCh<4; iCh++) {
    fNTotalHitPerChamber[iCh] = 0;
    fN2HitClusterPerChamber[iCh] = 0;
    fN3HitClusterPerChamber[iCh] = 0;
    fNViewsPerChamber[iCh] = 0;
    fIdChamber[iCh] = -1;
  }

  if (fDATA) {
    Double_t bintmag[2];
    Double_t xpos = fFastX0+fFastThetaX*(fZMag-fFastZ0); 
    Double_t ypos = fFastY0+fFastThetaY*(fZMag-fFastZ0); 
    ExtractBMag(xpos,ypos,bintmag);
    Double_t bmag = 1000.*bintmag[0]/fDMag;
    Double_t bmagy = 1000.*bintmag[1]/fDMag;
    fPtKick = fEC*bmag*fDMag*1000*1.007; // Effective scaling to account for fringe field
    fPtKickY = -fEC*bmagy*fDMag*1000*1.01; // opposite direction of the ptkick along Y (F= q v x B). Scaling to account for fringe field 
  }

  // Clean matrices
  Kal_Hit_Var.ResizeTo(fNResidualHits,9);
  Kal_Eta_EachHit_Forward.clear();
  Kal_Eta_EachHit_Backward.clear();
  Kal_V_EachHit_Forward.clear();
  Kal_V_EachHit_Backward.clear();
  V_Weighted.clear();
  Eta_Weighted.clear();
  V_Weighted.resize(fNResidualHits);
  Eta_Weighted.resize(fNResidualHits);
  for(Int_t i = 0; i < fNResidualHits; i++){
    V_Weighted[i].ResizeTo(4,4);
    Eta_Weighted[i].ResizeTo(4,1);
  }
}

void Track::MakeInitialFitMatricesSimple()
{
  // Initial values
  fFastMomentum = jCombination->GetP();
  fFastThetaX = jCombination->GetThetaX();
  fFastThetaY = jCombination->GetThetaY();
  fFastX0 = jCombination->GetX0();
  fFastY0 = jCombination->GetY0();
  fFastZ0 = jCombination->GetZ0();
  fTTime = jCombination->GetTrailingTime();
  StoreTrack(-1); 

  // Eta
  Kal_Eta(0,0)=fFastThetaX;
  Kal_Eta(1,0)=fFastThetaY;
  Kal_Eta(2,0)=fFastX0+fFastThetaX*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);
  Kal_Eta(3,0)=fFastY0+fFastThetaY*(Kal_Hit_Var(fNResidualHits-1,1)-fFastZ0);

  // Covariance matrix
  Kal_V.Zero();
  Kal_V(0,0)=.001*.001;
  Kal_V(1,1)=.001*.001;
  Kal_V(2,2)=20.*20.;
  Kal_V(3,3)=20.*20.;
  Kal_V *= 200;
}

void Track::FitSimple()
{
  // ThetaY measurement
  MakeInitialFitMatricesSimple();
  UpStream_ChamberID = 0; //chamber 1
  DownStream_ChamberID = 3; //chamber 4
  Fit_Direction = 0;
  Hit_Count = fNResidualHits;
  Hit_Added = 0;
  Kalman_Algorithm_Simple();
  badfit = true;
  //Double_t thetaY_Before = Kal_Eta_prime(1,0);
  //Double_t posY_Before = Kal_Eta_prime(3,0);

  // BACKWARDS FIT: 4->3
  MakeInitialFitMatricesSimple();
  UpStream_ChamberID = 2;
  DownStream_ChamberID = 3;
  Fit_Direction = 0;
  Hit_Count = fNResidualHits;
  Hit_Added = 0;
  Kalman_Algorithm_Simple();
  badfit = true;
  Double_t thetaX_After = Kal_Eta_prime(0,0);
  Double_t posX_After = Kal_Eta_prime(2,0)+Kal_Eta_prime(0,0)*(fZMag+0.5*fDMag-fPositionZ);

  // FORWARD FIT 1->2
  MakeInitialFitMatricesSimple();
  UpStream_ChamberID = 0;
  DownStream_ChamberID = 1;
  Fit_Direction = 1;
  Hit_Count = 1;
  Hit_Added = 0;
  Kalman_Algorithm_Simple();
  badfit = true;
  Double_t thetaX_Before = Kal_Eta_prime(0,0);
  Double_t posX_Before = Kal_Eta_prime(2,0)+Kal_Eta_prime(0,0)*(fZMag-0.5*fDMag-fPositionZ);

  // Momentum measurement
  Bool_t loop = true;
  Int_t kIter = 0;
  Double_t pmom = fPtKick/(thetaX_Before-thetaX_After); 
  Double_t dx_after = 0;
  while (loop) {
    dx_after = posX_Before+thetaX_Before*fDMag-0.5*(fPtKick/pmom)*fDMag-posX_After;
    if (fabs(dx_after)>0.5&&kIter<4) {
      pmom = fPtKick/(thetaX_Before-thetaX_After+dx_after/fDMag);
      kIter++;
      continue;
    }
    break;
  }

  dx_after = posX_Before+thetaX_Before*fDMag-0.5*(fPtKick/pmom)*fDMag-posX_After; 
}

void Track::Kalman_Algorithm_Simple()
{
  // ADD HIT
  viewID = Kal_Hit_Var(Hit_Count-1,0);
  Measured_Hit_Position_X = Kal_Hit_Var(Hit_Count-1,4);
  Kal_Sigma = SigmaRadius(Kal_Hit_Var(Hit_Count-1,5));

  //Using the initial matrices for the first hit added
  //and using the calculated matrices to add on the hit recursively
  TMatrixD Kal_V_tmp, Kal_Eta_tmp;
  Kal_V_tmp.ResizeTo(Kal_V);
  Kal_Eta_tmp.ResizeTo(Kal_Eta);
  if(Hit_Added == 0 && Hit_Count == fNResidualHits){ 
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_prime.Zero(); 
  }else if(Hit_Added == 0 && Hit_Count == 1){ //needed to alter the initial conditions for the forward fit. 
    Kal_V_tmp = Kal_V;
    Kal_Eta_tmp = Kal_Eta;
    Kal_Eta_tmp(2,0) = fFastX0+fFastThetaX*(Kal_Hit_Var(Hit_Count-1,1)-fFastZ0);
    Kal_Eta_tmp(3,0) = fFastY0+fFastThetaY*(Kal_Hit_Var(Hit_Count-1,1)-fFastZ0);
    Kal_Eta_prime.Zero();
  }else{
    Kal_V_tmp = Kal_V_prime;
    Kal_Eta_tmp = Kal_Eta_prime;
  }
  
  // Adding the hit to the covariance matrix
  TMatrixD num1(Kal_D[viewID],TMatrixD::kTransposeMult,Kal_V_tmp);
  TMatrixD num2(Kal_D[viewID],TMatrixD::kMult,num1);
  Kal_V_prime.Mult(Kal_V_tmp,num2);
  TMatrixD deno1(Kal_V_tmp,TMatrixD::kMult,Kal_D[viewID]);
  TMatrixD deno2(Kal_D[viewID],TMatrixD::kTransposeMult,deno1);
  deno2(0,0) += Kal_Sigma*Kal_Sigma;
  Kal_V_prime *= -1/deno2(0,0);
  Kal_V_prime += Kal_V_tmp;

  // Adding the hit to the parameter matrix
  Kal_Eta_prime.Mult(Kal_V_prime,Kal_D[viewID]);
  Kal_Eta_prime *= (Measured_Hit_Position_X - dEta(viewID,Kal_Eta_tmp))/(Kal_Sigma*Kal_Sigma);
  Kal_Eta_prime += Kal_Eta_tmp;
  
  // MULTIPLE SCATTERING
  if(Hit_Added>0) AddMultipleScattering();

  //counting the number of hits added to the track
  Hit_Added++;

  // TRANSPORTING
  fPositionZ = Kal_Hit_Var(Hit_Count-1,1);
  if ((Hit_Count>1 && Fit_Direction==0) && Kal_Hit_Var(Hit_Count-2,2)>=UpStream_ChamberID && Kal_Hit_Var(Hit_Count-2,2)<=DownStream_ChamberID) Transport_Simple();
  if ((Hit_Count<fNResidualHits && Fit_Direction==1) && Kal_Hit_Var(Hit_Count,2)>=UpStream_ChamberID && Kal_Hit_Var(Hit_Count,2)<=DownStream_ChamberID) Transport_Simple();
}

void Track::Transport_Simple()
{
  if(Fit_Direction == 0) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count-2,1);
  if(Fit_Direction == 1) Distance_ToNext_Hit = Kal_Hit_Var(Hit_Count-1,1) - Kal_Hit_Var(Hit_Count,1);

  // Translating Eta_prime
  Kal_Eta_prime(2,0) = Kal_Eta_prime(2,0)-Kal_Eta_prime(0,0)*Distance_ToNext_Hit; 
  Kal_Eta_prime(3,0) = Kal_Eta_prime(3,0)-Kal_Eta_prime(1,0)*Distance_ToNext_Hit;

  // Transforming the Kal_V (the covariance matrix)
  Kal_A.UnitMatrix();
  Kal_A(2,0) = -Distance_ToNext_Hit;
  Kal_A(3,1) = -Distance_ToNext_Hit; 
  TMatrixD transp1(Kal_V_prime,TMatrixD::kMultTranspose,Kal_A);
  Kal_V_prime.Mult(Kal_A,transp1);
  
  //Decrementing our hit_counter for the backward fit
  if(Fit_Direction == 0) Hit_Count--; 
    
  // Incrementing our hit_counter for the forward fit
  if(Fit_Direction == 1) Hit_Count++; 

  // Add hit  
  Kalman_Algorithm_Simple();
}

void Track::ExtractBMag(Double_t xmag, Double_t ymag, Double_t *bmag) {
  Double_t pass = 80.;
  Int_t kx = (Int_t)(xmag+1000)/fabs(pass); 
  Int_t ky = (Int_t)(ymag+1000)/fabs(pass); 
  Int_t j = 27*kx+ky;
  Double_t xposgrid = -1000+kx*pass;
  Double_t yposgrid = -1000+ky*pass;
  Double_t dx = fabs(xmag-xposgrid)/fabs(pass);
  Double_t dy = fabs(ymag-yposgrid)/fabs(pass);
  Double_t bymag1 = SpectrometerParameters::GetInstance()->GetByIntegral(j);
  Double_t bymag2 = SpectrometerParameters::GetInstance()->GetByIntegral(j+27);
  Double_t bymag3 = SpectrometerParameters::GetInstance()->GetByIntegral(j+1);
  Double_t bymag4 = SpectrometerParameters::GetInstance()->GetByIntegral(j+27+1);
  Double_t by1 = bymag1;         
  Double_t by2 = kx<25 ? bymag2 : bymag1; 
  Double_t by3 = ky<26 ? bymag3 : bymag1; 
  Double_t by4 = kx<25&&ky<26 ? bymag4 : bymag1;
//  bmag[0] = (by1+by2+by3+by4)/4.;
  bmag[0] = (1-dx-dy+dx*dy)*by1+(dx-dx*dy)*by2+(dy-dx*dy)*by3+dx*dy*by4;
  Double_t bxmag1 = SpectrometerParameters::GetInstance()->GetBxIntegral(j);
  Double_t bxmag2 = SpectrometerParameters::GetInstance()->GetBxIntegral(j+27);
  Double_t bxmag3 = SpectrometerParameters::GetInstance()->GetBxIntegral(j+1);
  Double_t bxmag4 = SpectrometerParameters::GetInstance()->GetBxIntegral(j+27+1);
  Double_t bx1 = bxmag1;         
  Double_t bx2 = kx<25 ? bxmag2 : bxmag1; 
  Double_t bx3 = ky<26 ? bxmag3 : bxmag1; 
  Double_t bx4 = kx<25&&ky<26 ? bxmag4 : bxmag1;
//  bmag[1] = (bx1+bx2+bx3+bx4)/4.;
  bmag[1] = (1-dx-dy+dx*dy)*bx1+(dx-dx*dy)*bx2+(dy-dx*dy)*bx3+dx*dy*bx4;
}

