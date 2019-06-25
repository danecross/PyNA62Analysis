#ifndef Track_H
#define Track_H 1

#include "TLorentzVector.h"
#include "ChamberHitCollector.hh"
#include "Event.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TVector3.h"
#include <stdarg.h>
#include "Combination.hh"
#include <TMatrixD.h>
#include "TH2F.h"
#include "TF1.h"
#include "TSpline.h"
#include "TGraph.h"

typedef std::vector<ChamberHitCollector *> ChamberCollector;

class SpectrometerGeometry;

class Track 
{

public:
  Track();
  virtual ~Track();
  void Reconstruct(Combination *,ChamberCollector *,TRecoSpectrometerCandidate *, Int_t);
  void ReconstructMuonRun(Combination *,ChamberCollector *,TRecoSpectrometerCandidate *);
  void ReconstructSimple(Combination *,ChamberCollector *,TRecoSpectrometerCandidate *);

  // Histograms
  void InitHistograms();
  void SaveHistograms();

private:
  SpectrometerGeometry *fSpecGeo;
  Int_t fIteration;
  Combination *jCombination; ///< Pointer to a chamber-hits combination candidate. 
  ChamberCollector *fChamber; ///< Pointer to the ensemble of view-hits reconstructed in one chamber chamber. 
  TRecoSpectrometerCandidate *fCandidate; ///< Pointer to the class which stores the reconstructed tracks.
  SpectrometerParameters *fPar;
  Bool_t fDATA;
  Int_t fNChambers;
  Int_t fIdChamber[4]; ///< Id of the chambers of the combination.
  Int_t fNResidualHits; ///< Number of hits after the tight fit.
  Int_t fNTotalHitPerChamber[4]; ///< Number of total tube-hit per chamber.
  Int_t fNViewsPerChamber[4]; ///< Number of total views per chamber.
  Int_t fN2HitClusterPerChamber[4]; ///< Number of views per chamber with 2 straws.
  Int_t fN3HitClusterPerChamber[4]; ///< Number of views per chamber with 3 straws.
  Double_t fPMom;
  Double_t fFastMomentum; ///< Momentum of the combination estimated by fitting the clusters forming the combination (TrackCollector).
  Double_t fFastThetaX; ///< Slope in X of the combination estimated by fitting the clusters forming the combination (TrackCollector).
  Double_t fFastThetaY; ///< Slope in Y of the combination estimated by fitting the clusters forming the combination (TrackCollector).
  Double_t fFastX0; ///< X position at Z0 of the combination estimated by fitting the clusters forming the combination (TrackCollector). 
  Double_t fFastY0; ///< Y position at Z0 of the combination estimated by fitting the clusters forming the combination (TrackCollector).  
  Double_t fFastZ0; ///< Z0 of the combination estimated by fitting the clusters forming the combination (TrackCollector). 
  Double_t fFastCharge;///< Charge of combination track
  Double_t fTTime; ///< Time of the combination from the trailing time.
  Double_t fLTime; ///< Time of the combination of leading edge hit times.
  Double_t fPtrack; ///< Momentum of the fitted track. 
  Double_t fThetaX; ///< Slope in X of the fitted track.
  Double_t fThetaY; ///< Slope in Y of the fitted track.
  Double_t fPosX; ///< X position at the reference plane (Z selected in Spectrometer.conf) of the fitted track.
  Double_t fPosY; ///< Y position at the reference plane (Z selected in Spectrometer.conf) of the fitted track.
  Double_t fCharge; ///< Charge of the fitted track.
  Double_t fChi2; ///< Chi2 of the fitted track. 
  Double_t fDMag; ///< Thickness of the magnet.
  Double_t fZMag; ///< Position of the front face of the magnet.
  Double_t fEC; ///< e*c. 
  Double_t fBMag; ///< Mangetic field intensity.
  Double_t fZRef; ///< Longitudinal position of the reference plane for the final track fit.
  Double_t fXX0; ///< Estimated X/X0 per single plane of straws. 
  Double_t fGuessAverageSigma; ///< Estimated average sigma on radius measurement.
  Double_t fGuessSigmaNoSlope; ///< Estimated sigma on radius measurement in case of no slope measurement in the LR ambiguity algorithm.
  Bool_t fIsMuonRun;
  Bool_t fRTMode;
  Bool_t fXTMode;

  //Additional variables added for the Kalman fit
  Int_t type_of_data;
  Double_t fXMag;
  Double_t fYMag;
  Double_t fPtxKick[2]; //in MeV
  Double_t fPtyKick[2]; //in MeV
  Double_t deltaX; // Shift n X due to the magnet kick
  Double_t Distance_ToNext_Hit;// distance to the following hit
  Int_t Hit_Count;
  Int_t Hit_Added;
  Double_t fFinalMomentum;
  Int_t viewID;
  Double_t Measured_Hit_Position_X;
  Double_t Kal_Sigma;
  Int_t UpStream_ChamberID;
  Int_t DownStream_ChamberID;
  Double_t DistanceTo_Magnet; //from hit before the magnet to the middle of the magnet
  Double_t DistanceFrom_Magnet;
  Int_t Fit_Direction;
  Int_t NumberOfEvents;
  Double_t fZRef_Upstream;//m
  Double_t fZRef_Downstream;//m
  Bool_t badfit;
  Bool_t trans_mag;
   

  //KALMAN FIT MATRICES
  TMatrixD Kal_D[4]; // derivative matrix
  TMatrixD V_Final[2]; 
  TMatrixD Eta_Final[2]; 
  TMatrixD Kal_Eta_prime; // kalman calculation of next parameter
  TMatrixD Kal_V_prime; // kalman calculation of next covariance matrix
  TMatrixD Kal_Eta; // kalman calculation of next parameter
  TMatrixD Kal_V; // kalman calculation of next covariance matrix
  TMatrixD Kal_A; // kalman jacobian matrix between parameters of two adjacent hits
  TMatrixD Kal_Hit_Var; //matrix containing the hits
  Int_t KickDirection_Charge; //direction of the kick due to the charge of the particle
  Int_t KickDirection_Fit; // to keep the kick transport in the right direction when transporting forward or backward

  std::vector<TMatrixD> V_Weighted;
  std::vector<TMatrixD> Eta_Weighted;  
  std::vector<TMatrixD> Kal_Eta_EachHit_Forward;
  std::vector<TMatrixD> Kal_Eta_EachHit_Backward;
  std::vector<TMatrixD> Kal_V_EachHit_Forward;
  std::vector<TMatrixD> Kal_V_EachHit_Backward;


  Double_t fPtrack_original;
  Double_t fThetaX_original;
  Double_t fThetaY_original;
  Double_t fPosX_original;  
  Double_t fPosY_original;  
  Double_t fCharge_original;
  Double_t chi2_original;  
  
  Double_t fTimeCorX[100];

  //FUNCTIONS
  void Init_FitMatrices();

  void Init(); // new version
  void ExtractCoordinate(); // new version    
  void Fit(); 
  void StoreTrack(Int_t);
  void StoreTrack();
  Bool_t Kalman_Algorithm(Int_t);
  void MakeInitialFitMatrices();
  Double_t dEta(Int_t,TMatrixD);
  void Transport();
  Double_t Resolution(TMatrixD, Double_t);
//  Bool_t Magnet_Transport();
  void AddMultipleScattering();  
  Double_t chi2_calc();
  Double_t ComputeTrackTimeLeading();
  void FinalForwardMatrices();
  void FinalBackwardMatrices();
  void Weigh_Matrices();
  void Transport_To_ZRef();
  Double_t SigmaRadius(Double_t); 

  Double_t fByMagField[702];
  Double_t fBxMagField[702];
//  Double_t fBxMNP33[26][26][169];
//  Double_t fByMNP33[26][26][169];
//  Double_t fBzMNP33[26][26][169];
  Double_t fBxMNP33[169][26][26];
  Double_t fByMNP33[169][26][26];
  Double_t fBzMNP33[169][26][26];
  Double_t fZcoorMNP33[169];
//  Double_t fBxFringe[17][17][30];
//  Double_t fByFringe[17][17][30];
//  Double_t fBzFringe[17][17][30];
  Double_t fBxFringe[30][17][17];
  Double_t fByFringe[30][17][17];
  Double_t fBzFringe[30][17][17];
  Double_t fZcoorFringe[30];
  void ExtractBMag(Double_t, Double_t, Double_t*);

  // Full fit
  void FitFullMNP33Forward(); 
  void FitFullMNP33Backward(); 
  void Transport_free(Double_t,Double_t);
  void Transport_magnet(Int_t,Double_t,Double_t,Bool_t);
  Int_t fHitCountStartBackward;
  Int_t fHitAddedStartBackward;
  Int_t fHitCountStartForward;
  Int_t fHitAddedStartForward;
  Double_t fScale;
 
  TVector3 ComputeBFieldAtXYLinear(Int_t,Double_t*,Int_t);
  Double_t *ComputeBFieldAtZLinear(Int_t,Double_t*,TVector3 *,Int_t); 

  // Muon RUN
  void InitMuonRun();
  void MakeInitialFitMatricesMuonRun();
  void FitMuonRun(); 
  Bool_t Kalman_Algorithm_MuonRun();
  void Transport_MuonRun(); 
  void Weigh_Matrices_MuonRun();
  void Transport_To_ZRef_MuonRun();

  // MC or Simplified
  Double_t fPtKick;
  Double_t fPtKickY;

  // Analytical
  Double_t fPositionZ;
  void InitSimple();
  void MakeInitialFitMatricesSimple();
  void FitSimple(); 
  void Kalman_Algorithm_Simple();
  void Transport_Simple(); 

  // Functions
  TF1 *fRTdependence;

  // Histograms
  TH2F *fHResidual;
  TH2F *fHResidualvsR;
  TH2F *fHResidualvsR_304;
  TH2F *fHResidualvsR_313;
  TH2F *fHResidualvsR_2379;
  TH2F *fHResidualvsR_2388;
  TH2F *fHStrawPos;
  TH2F *fHTimeCor;
  TH2F *fHStrawPosVSAngle[16];
  TH2F *fHTimeCorPerView[16];
};
#endif
