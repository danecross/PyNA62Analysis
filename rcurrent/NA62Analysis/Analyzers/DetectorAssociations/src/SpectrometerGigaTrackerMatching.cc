// ----------------------------------------------------------------------
//
// History:
//
// Created by Joel Swallow (joel.christopher.swallow@cern.ch) 24/03/17
// Modified by Joel Swallow 21/06/17 and 6/7/17
// Modified by Joel Swallow 14/09/17
// ----------------------------------------------------------------------

/// \class SpectrometerGigaTrackerMatching
/// \Brief
/// Matching an upstream GTK candidate to a DownstreamTrack object.
/// \EndBrief
/// \Detailed
/// The analyser is set up to run in two different modes,
/// 0. (default) General GTK-Spectrometer matching mode. In this mode the analyser loops over all 
/// DownstreamTracks in an event and for each one assesses the matching to each GTK Candidate in the
/// event. For each DownstreamTrack object an output container (SpectrometerGigaTrackerMatching) is 
/// filled with a set of vectors (of size equal to the number of GTK candidates in the event) which
/// charecterise the GTK candidate, the DownstreamTrack and the quality of the matching between them
/// determined using a matching discriminant. 
/// 1. (special option) testing and Validation mode : applying the procedure on a per-downstream track
/// basis using a pure K3pi sample (isolated using the K3piStrictSelection analyser). Operation in
/// this mode includes evaluation of the performance of the matching proceudre, claculating the
/// mismatch and no-match probabilities.
/// For more details please see internal note NA62-17-08: "K-pi Association for Single Track Event 
/// Reconstruction at the NA62 Experiment"
/// 	Example of use:
/// \code
///	std::vector<SpectrometerGigaTrackerMatchingOutput> SpecGTK = *(std::vector<SpectrometerGigaTrackerMatchingOutput>*)GetOutput("SpectrometerGigaTrackerMatching.Output");
///        for (UInt_t iTrack=0; iTrack<SpecGTK.size(); iTrack++) { //loop over SpecGTK output containers (one for each DownstreamTrack)
///                //SpecGTK[iTrack]  is the SpectrometerGigaTrackerMatchingOutput object for the iTrack^th DownstreamTrack
///                for (Int_t i=0; i<SpecGTK[iTrack].GetNGTKCandidates(); i++) { //loop over the entries for each GTK candidate. 
///                        SpecGTK[iTrack].Print(i); //Print results for GTK Candidate i and DownstreaTrack iTrack         
///                }
///		   std::cout<<"Event No.:"<<iEvent<<": DownstreamTrack:"<<iTrack<<": Best matched GTK candidate Index = "<<SpecGTK[iTrack].GetBestIndex()<<": Best Discriminant=M_D="<<SpecGTK[iTrack].GetBestDiscriminant()<<std::endl; //Print the best matching GTK candidate index and corresponding Discriminant value. 
///		   //One can also access other properties of the record corresponding to the best matching GTK candidate : e.g SpecGTK[iTrack].GetBestCorrectedBeamParticleMomentum(); .
///        }
/// \endcode
/// Note that if one is trying to perform a matching using this procedure there are cases where the 
/// output vector is not filled for every DownstreamTrack because it has not been possible to apply the matching 
/// procedure (for example an event may fail the requierment that exactly 1 CHOD and NewCHOD association exists - 
/// which is needed for timing information of the DownstreamTrack used in the matching). One may therefore 
/// check the DownstreamTrackID [using : SpecGTK->GetTrackID()] to see if an association output exists 
/// for the DownstreamTrack of interest for your analysis.
/// \author Joel Swallow (joel.christopher.swallow@cern.ch)
/// \EndDetailed
// ---------------------------------------------------------------

#include "SpectrometerGigaTrackerMatching.hh"
#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include <TF1.h>
#include <TCanvas.h>
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TProfile.h"
#include "L0TPData.hh"
#include "TwoLinesCDA.hh"
#include <cmath>
#include "TMath.h"
#include "Math/WrappedTF1.h"
#include "Math/GaussIntegrator.h"
#include <algorithm>	//for sorting arrays
#include "DownstreamTrack.hh" //for dealing with downstream track as an input....
#include "SpectrometerGigaTrackerMatchingOutput.hh" 
#include "BaseAnalysis.hh"
#include "BlueTubeTracker.hh"
#include "SpectrometerTrackVertex.hh"

using namespace NA62Analysis;
using namespace NA62Constants;
using namespace std;

Double_t fitf_cdaSig(Double_t *x, Double_t *par);
Double_t NumericalIntegrationCDASig();
Double_t NumericalIntegrationCDASig_VAL(Double_t cda);
Double_t fitf_cdaBkg(Double_t *x, Double_t *par);
Double_t NumericalIntegrationCDABkg();
Double_t NumericalIntegrationCDABkg_VAL(Double_t cda);
Double_t fitf_dtSig(Double_t *x, Double_t *par);
Double_t NumericalIntegrationdtSig();
Double_t NumericalIntegrationdtSig_VAL(Double_t dt);
Double_t fitf_dtBkg(Double_t *x, Double_t *par);
Double_t NumericalIntegrationdtBkg();
Double_t NumericalIntegrationdtBkg_VAL(Double_t dt);
// :::: new pdfs :::: 4.2.17 ::::
Double_t fitf_cdaSig_new(Double_t *x, Double_t *par);
Double_t NumericalIntegrationCDASig_new();
Double_t NumericalIntegrationCDASig_VAL_new(Double_t cda);
Double_t fitf_cdaBkg_new(Double_t *x, Double_t *par);
Double_t NumericalIntegrationCDABkg_new();
Double_t NumericalIntegrationCDABkg_VAL_new(Double_t cda);
Double_t fitf_dtSig_new(Double_t *x, Double_t *par);
Double_t NumericalIntegrationdtSig_new();
Double_t NumericalIntegrationdtSig_VAL_new(Double_t dt);
Double_t fitf_dtBkg_new(Double_t *x, Double_t *par);
Double_t NumericalIntegrationdtBkg_new();
Double_t NumericalIntegrationdtBkg_VAL_new(Double_t dt);

//constructor--------------------------------------------------------------------------------------
SpectrometerGigaTrackerMatching::SpectrometerGigaTrackerMatching(Core::BaseAnalysis *ba) : Analyzer(ba, "SpectrometerGigaTrackerMatching"){

  Double_t reftime=0;	//default 0
  fRefTime = reftime;	//set reference time

  //fnRecoCandUpperLim = 250; // Upper Limit set on the number of GTK candidates considered (sets size of arrays). : Search for fnRecoCandUpperLim to find all instances of this (must be hard-coded to 250 to prevent warnings).

  // Geometry parameters (GTK)
  fGTKOffset = 0.;
  fZGTK[0] = 79600.; //Z position of GTK1
  fZGTK[1] = 92800.; //Z position of GTK2
  fZGTK[2] = 102400.;//Z position of GTK3

  RequestTree("GigaTracker",new TRecoGigaTrackerEvent,"Reco");
  fGTKevt = GetEvent<TRecoGigaTrackerEvent>();

  RequestL0Data();	
  // Algorithm Paramters (From GigaTrackerRecoEvt (9/11/16)
  AddParam("TimeWindow"                 , &fTimeWindow                      ,  2        ); //ns
  AddParam("TimeWindowTrigger"          , &fTimeWindowTrigger               ,  5        ); //ns
  AddParam("XWindow"                    , &fXWindow                         ,  10       ); //mm
  AddParam("YWindow"                    , &fYWindow                         ,  10       ); //mm
  AddParam("Dt"                         , &fDt                              ,  0        ); //if need offset
  AddParam("Chi2X"                      , &fChi2X                           ,  20       ); //chi2 cut
  AddParam("Chi2T"                      , &fChi2T                           ,  20       ); //chi2 cut
  AddParam("RefineTimeStation"          , &fRefineTimeStation               ,  1        );
  AddParam("RefineTimeChip"             , &fRefineTimeChip                  ,  0        );
  AddParam("RemoveHit"                  , &fRemoveHit                       ,  1        );

  // Reference detector
  fReferenceDetector = 0;

  //Set variables as sent by Giussepe
  fOffset_X_GTK[0]=0.44; 
  fOffset_X_GTK[1]=-0.07;
  fOffset_X_GTK[2]=0.13;
  fOffset_Y_GTK[0]=0.04;
  fOffset_Y_GTK[1]=0.11;
  fOffset_Y_GTK[2]=0.07;

  fNK3piCandidatesPassingDiscCut = 0;
  fNK3piCandidatesNotPassingDiscCut = 0;

  //Initialize matching variables to 0 
  for(Int_t j=0; j<3 ;j++){
    fNoCandidate=0;
    fDmatchMatch_NMatch[j]=0;
    fDmatchMatch_NMismatch[j]=0;
    fDmatchMatch_NNoMatch[j]=0;
    fMDMatch_NMatch[j]=0;
    fMDMatch_NMismatch[j]=0;
    fMDMatch_NNoMatch[j]=0;
    fDmatchMatchUNR_NMatch[j]=0;
    fDmatchMatchUNR_NMismatch[j]=0;
    fDmatchMatchUNR_NNoMatch[j]=0;
  }

  fEfficiency = 0;
  fNRecoNoCand = 0;
  fNK3piCandidates = 0;
  fNCandidateEvents =0;
  fNRecoMoreThanOne = 0;

}

//--------------------------------------------------------------------------------------------------
SpectrometerGigaTrackerMatching::~SpectrometerGigaTrackerMatching(){}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::InitOutput(){

  //////////////Outputs///////////////
  //Dmatch Procedure
  RegisterOutput("PiIndex",&fMatchedpiIndex);
  RegisterOutput("KIndex",&fMatchedKIndex);
  RegisterOutput("MatchingQuality",&fMatchingQuality);
  //MD Procedure
  RegisterOutput("PiIndex_MD",&fMatchedpiIndex_MD);
  RegisterOutput("KIndex_MD",&fMatchedKIndex_MD);
  RegisterOutput("MatchingQuality_MD",&fMatchingQuality_MD);
  //Dmatch Unrestricted Procedure
  RegisterOutput("PiIndex_DmUNR",&fMatchedpiIndex_DmUNR);
  RegisterOutput("KIndex_DmUNR",&fMatchedKIndex_DmUNR);
  RegisterOutput("MatchingQuality_DmUNR",&fMatchingQuality_DmUNR);

  RegisterOutput("Output", &fContainer);

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::InitHist(){

  //GTK Candidates Histos
  BookHisto("GTKCand/hNReconstructedCandidates", new TH1F("NReconstructedCandidates","Number of Reconstructed Candidates",31,-0.5,30.5));
  BookHisto("GTKCand/hpCand", new TH1F("pCand","Momentum of Reconstructed Candidate; |p_{K}| [GeV]",160,71,79));
  BookHisto("GTKCand/hCandidateHitPosGTK1", new TH2F("CandidateHitPosGTK1","Hit position of candidate in GTK1; x [mm] ; y [mm]",210,-35,35,100,-20,20));
  BookHisto("GTKCand/hCandidateHitPosGTK2", new TH2F("CandidateHitPosGTK2","Hit position of candidate in GTK2; x [mm] ; y [mm]",210,-35,35,100,-20,20));
  BookHisto("GTKCand/hCandidateHitPosGTK3", new TH2F("CandidateHitPosGTK3","Hit position of candidate in GTK3; x [mm] ; y [mm]",120,-35,35,100,-20,20));
  BookHisto("GTKCand/hCandAngleX", new TH1F("CandAngleX","Candidate Angle #Theta_{x} = #frac{dx}{dz} ; dx/dz ",100,0.0009,0.0016));
  BookHisto("GTKCand/hCandAngleY", new TH1F("CandAngleY","Candidate Angle #Theta_{y} = #frac{dy}{dz} ; dy/dz ",100,-0.0005,0.0005));
  BookHisto("GTKCand/pCAND_XvsAngleX_GTK3", new TProfile("CAND_XvsAngleX_GTK3_Profile", "dx/dz vs x at GTK3 for candidate; x [mm]; dx/dz", 210, -35, 35));
  BookHisto("GTKCand/pCAND_YvsAngleY_GTK3", new TProfile("CAND_YvsAngleY_GTK3_Profile", "dy/dz vs y at GTK3 for candidate; y [mm]; dy/dz", 240,-20,20)); //210, -35, 35));
  BookHisto("GTKCand/hdtCandidate", new TH1F("dtCandidate","Time diference between GTK candidate time and KTAG reference time; t_{GTK}-t_{KTAG}",300,-1.2,1.2));

  BookHisto("NGTKErrorMask",new TH1F("NGTKErrorMask","Number of times the GTK error mask is/is not present; 0=No error mask , 1=Error mask",2,-0.5,1.5));
  BookHisto("nRecoCandExeeds250",new TH1F("nRecoCandExeeds250","Number of times there are nore than 250 reconstructed GTK candidates",1,0.5,1.5));	
  BookHisto("nRecoCandExeeds250_GTKerrorMask",new TH1F("nRecoCandExeeds250_GTKerrorMask","Number of times there are more than 250 reconstructed GTK candidates with/without GTK error mask; 0=No error mask, 1=Error mask",2,-0.5,1.5));
  BookHisto("nRecoCand_ErrorMaskOff",new TH1F("nRecoCand_ErrorMaskOff","Number of reconstructed GTK candidates (GTK error mask OFF)",10001,-0.5,10000.5));
  BookHisto("nRecoCand_ErrorMaskActivated",new TH1F("nRecoCand_ErrorMaskActivated","Number of reconstructed GTK candidates (GTK error mask ACTIVATED)",10001,-0.5,10000.5));

  //Histos for Spectrometer-GTK matching 
  BookHisto("CDA_/hCDA", new TH1F("CDA","CDA of #pi and K tracks (Disc20); CDA [mm]",500,0,80));
  BookHisto("CDA_/hCDAdt", new TH2F("CDAdt","CDA vs t_{GTKcand}-t_{KTAG} (Disc<20); t_{GTKcand}-t_{KTAG} [ns] ; CDA [mm] ",300,-1.2,1.2,250,0,50));


  BookHisto("DmatchMatchingProcedure/NInsideOutsidedtWindow",new TH1F("NInsideOutsidedtWindow","No. of candidates inside dt window [-0.6,0.6] ns ; 0 = outside window [-1,-0.6] or [0.6,1] , 1 = inside window [-0.6,0.6]",2,0,2));
  for(Int_t j=1; j<4 ; j++){
    BookHisto(Form("DmatchMatchingProcedure/NInsideOutsidedtWindowPi%d",j),new TH1F(Form("NInsideOutsidedtWindowPi%d",j),Form("No. of candidates inside dt window [-0.6,0.6] ns (using #pi_{%d} so once per K3pi!); 0 = outside window [-1,-0.6] or [0.6,1] , 1 = inside window [-0.6,0.6]",j),2,0,2));
  }	

  for(Int_t j = 1 ; j<4 ; j++){
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hPsig",j),new TH1F(Form("PsigPi%d",j),Form("P_{Sig} (#pi_{%d})",j),200,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hPbkg",j),new TH1F(Form("PbkgPi%d",j),Form("P_{Bkg} (#pi_{%d})",j),200,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hPsig_Pbkg",j),new TH1F(Form("Psig_PbkgPi%d",j),Form("P_{Sig}+P_{Bkg} (#pi_{%d})",j),200,0,2));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hRsig",j),new TH1F(Form("RsigPi%d",j),Form("P_{Sig}/(P_{Sig}+P_{Bkg}) (#pi_{%d})",j),200,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hRbkg",j),new TH1F(Form("RbkgPi%d",j),Form("P_{Bkg}/(P_{Sig}+P_{Bkg}) (#pi_{%d})",j),200,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hFnew",j),new TH1F(Form("FnewPi%d",j),Form("P_{Sig}/P_{Bkg} (#pi_{%d})",j),400,0,2));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/NewDmatch",j), new TH1F(Form("NewDmatchPi%d",j),Form("Dmatch using new procedure (#pi_{%d})",j),500,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hPsigPbkg2D",j),new TH2F(Form("PsigPbkg2DPi%d",j),Form("P_{Sig} vs P_{Bkg} #pi_{%d} ; P_{Bkg} ; P_{Sig}",j),100,0,1,100,0,1));
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/NewDmatchPassedNotPassed",j), new TH1F(Form("NewDmatchPassedNotPassedPi%d",j),Form("Dmatch Passed / Not passed (#pi_{%d}); 0 = Not passed , 1 = Passed",j),2,-0.5,1.5));
    ///////Matching evaulation
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/MatchingResults",j), new TH1F(Form("DmatchMatchingResultsPi%d",j),Form("Final Matching Results of Dmatch Matching Procedure (D<20,Dmatch<0.001) (#pi_{%d})",j),13,-2.5,10.5));	
    BookHisto(Form("MDMatchingProcedure/Pi%d/MatchingResults",j), new TH1F(Form("MDMatchingResultsPi%d",j),Form("Final Matching Results of MD Matching Procedure (D<20, MD unconstrained) (#pi_{%d})",j),13,-2.5,10.5));
    BookHisto(Form("DmatchMatchingProcedureUNR/Pi%d/MatchingResults",j), new TH1F(Form("DmatchUNRMatchingResultsPi%d",j),Form("Final Matching Results of Dmatch Unrestricted Matching Procedure (D<20) (#pi_{%d})",j),13,-2.5,10.5));
  }
  BookHisto("DmatchMatchingProcedure/NewDmatch", new TH1F("NewDmatch","Dmatch using new procedure",500,0,1));

  BookHisto("MDMatchingProcedure/hX_VertexPosK", new TH1F("X_VertexPosK","X_{Vertex} for K",200,-50,50));
  BookHisto("MDMatchingProcedure/hX_VertexPosPi", new TH1F("X_VertexPosPi","X_{Vertex} for #pi",200,-50,50));
  BookHisto("MDMatchingProcedure/hY_VertexPosK", new TH1F("Y_VertexPosK","Y_{Vertex} for K",200,-50,50));
  BookHisto("MDMatchingProcedure/hY_VertexPosPi", new TH1F("Y_VertexPosPi","Y_{Vertex} for #pi",200,-50,50));

  BookHisto("DmatchAltMatchingProcedure/hdeltat", new TH1F("deltat","#Delta t_{K#pi} = t_{GTK,cand} - t_{#pi} ; #Delta t_{K#pi} = t_{GTK,cand} - t_{#pi} [ns]",500,-5,5));
  BookHisto("DmatchAltMatchingProcedure/hdeltatCORR", new TH1F("deltatCORR","#Delta t_{K#pi} = t_{GTK,cand} - t_{#pi} (CORRECTED) ; #Delta t_{K#pi} = t_{GTK,cand} - t_{#pi} [ns]",500,-5,5));

  BookHisto("CDA_/hcda_All", new TH1F("cda_ALL","K-#pi CDA (all pairs); CDA [mm]",500,0,50));

  BookHisto("MDMatchingProcedure/hdeltaX_KpiVertex", new TH1F("deltaX_KpiVertex","K-#pi #Delta x (at vertex) ; #Delta x = x_{K} - x_{pi} [mm]",500,-30,30));
  BookHisto("MDMatchingProcedure/hdeltaY_KpiVertex", new TH1F("deltaY_KpiVertex","K-#pi #Delta y (at vertex) ; #Delta y = y_{K} - y_{pi} [mm]",500,-30,30));

  BookHisto("DmatchAltMatchingProcedure/hTcheck1", new TH1F("NPTC_TimeCheck1","Time Check 1 : t_{GTK,cand} - t_{KTAG} ; t_{GTK,cand} - t_{KTAG} [ns]",500,-5,5));
  BookHisto("DmatchAltMatchingProcedure/hTcheck2", new TH1F("NPTC_TimeCheck2","Time Check 2 : t_{pi} - t_{KTAG} ; t_{pi} - t_{KTAG} [ns]",500,-5,5));

  BookHisto("MDMatchingProcedure/MD", new TH1F("MD","M_{D} - Linear Matching Discriminant",200,0,100));
  BookHisto("DmatchAltMatchingProcedure/Dm", new TH1F("Dm","D_{match}^{'} - Matching Discriminant using #Delta t_{K-#pi}",200,0,1)); 

  for(Int_t j=1 ; j<4 ; j++){
    BookHisto(Form("MDMatchingProcedure/MD_Pi%d",j), new TH1F(Form("MD_Pi%d",j),Form("M_{D} - Linear matching Discriminant - #pi_{%d}",j),200,0,100));
    BookHisto(Form("MDMatchingProcedure/MDbestPi%d",j), new TH1F(Form("MDBestPi%d",j),Form("M_{D}^{Best=min} (#pi_{%d})- Linear Matching Discriminant",j),200,0,100));
  }
  BookHisto("MDMatchingProcedure/MDbestAll", new TH1F("MDBestAll","M_{D}^{Best=min} (#pi_{1,2,3})- Linear Matching Discriminant",200,0,100));

  BookHisto("DmatchAltMatchingProcedure/NumIntegration_DTKpi_Sig",new TH1F("NumIntegration_DTKpi_Sig_TEST","NumIntegration_DTKpi_Sig_TEST",100,0,205));
  BookHisto("DmatchAltMatchingProcedure/NumIntegration_DTKpi_Bkg",new TH1F("NumIntegration_DTKpi_Bkg_TEST","NumIntegration_DTKpi_Bkg_TEST",100,0,25));
  BookHisto("DmatchAltMatchingProcedure/NumIntegration_DTKpi_SigVAL", new TH1F("NumIntegration_DTKpi_SigVAL_TEST","NumIntegration_DTKpi_SigVAL_TEST",100,-5,5));
  BookHisto("DmatchAltMatchingProcedure/NumIntegration_DTKpi_BkgVAL", new TH1F("NumIntegration_DTKpi_BkgVAL_TEST","NumIntegration_DTKpi_BkgVAL_TEST",100,-5,5));

  BookHisto("DmatchAltMatchingProcedure/hDm_Psig",new TH1F("Dm_Psig","P_{sig}",100,0,1));
  BookHisto("DmatchAltMatchingProcedure/hDm_Pbkg",new TH1F("Dm_Pbkg","P_{bkg}",100,0,1));

  BookHisto("DmatchAltMatchingProcedure/Pi1/hMDmatchingResult", new TH1F("MDMatchingResult","M_{D} Matching Results; 0 = no match, 7 = mismatch, otherwise = match",12,-1.5,10.5));
  BookHisto("DmatchAltMatchingProcedure/Pi1/hDmmatchingResult", new TH1F("DmMatchingResult","D_{m} Matching Results",12,-1.5,10.5));


  for(Int_t j=1; j<4 ; j++){
    BookHisto(Form("MatchingResults/DmatchMatching/pi%d/P0",j), new TH1F(Form("DmatchMatchingPi%dP0",j),Form("Dmatch Matching : No Match Probability (P_{0}) , #pi_{%d}",j),500,0,0.50));
    BookHisto(Form("MatchingResults/MDMatching/pi%d/P0",j), new TH1F(Form("MDMatchingPi%dP0",j),Form("MD Matching : No Match Probability (P_{0}) , #pi_{%d}",j),500,0,0.50));
    BookHisto(Form("MatchingResults/DmatchMatchingUNR/pi%d/P0",j), new TH1F(Form("DmatchMatchingUNRPi%dP0",j),Form("Dmatch Matching UNR : No Match Probability (P_{0}) , #pi_{%d}",j),500,0,0.50));
    BookHisto(Form("MatchingResults/DmatchMatching/pi%d/Poc",j), new TH1F(Form("DmatchMatchingPi%dPoc",j),Form("Dmatch Matching : No Match Probability (P_{0}) Given >0 GTK Candidates, #pi_{%d}",j),500,0,0.50));		
    BookHisto(Form("MatchingResults/MDMatching/pi%d/Poc",j), new TH1F(Form("MDMatchingPi%dPoc",j),Form("MD Matching : No Match Probability (P_{0}) Given >0 GTK Candidates, #pi_{%d}",j),500,0,0.50));		
    BookHisto(Form("MatchingResults/DmatchMatchingUNR/pi%d/Poc",j), new TH1F(Form("DmatchMatchingUNRPi%dPoc",j),Form("Dmatch Matching UNR : No Match Probability (P_{0}) Given >0 GTK Candidates, #pi_{%d}",j),500,0,0.50));
    BookHisto(Form("MatchingResults/DmatchMatching/pi%d/Pmm",j), new TH1F(Form("DmatchMatchingPi%dPmm",j),Form("Dmatch Matching : Mismatch Probability (P_{mm}) , #pi_{%d}",j),500,0,0.1));
    BookHisto(Form("MatchingResults/MDMatching/pi%d/Pmm",j), new TH1F(Form("MDMatchingPi%dPmm",j),Form("MD Matching : Mismatch Probability (P_{mm}) , #pi_{%d}",j),500,0,0.1));
    BookHisto(Form("MatchingResults/DmatchMatchingUNR/pi%d/Pmm",j), new TH1F(Form("DmatchMatchingUNRPi%dPmm",j),Form("Dmatch Matching UNR : Mismatch Probability (P_{mm}) , #pi_{%d}",j),500,0,0.1));			
  }

  for(Int_t j=1; j<4 ; j++){
    BookHisto(Form("DmatchMatchingProcedure/Pi%d/hDmatch_Best_Pi%d",j,j), new TH1F(Form("Dmatch_Best_Pi%d",j),Form("D^{max}_{match} #pi_{%d}",j),200,0,1));
    BookHisto(Form("MDMatchingProcedure/Pi%d/hMD_Best_Pi%d",j,j), new TH1F(Form("MD_Best_Pi%d",j),Form("M^{min}_{D} #pi_{%d}",j),200,0,100));
    BookHisto(Form("DmatchAltMatchingProcedure/Pi%d/hDmatch_Best_UNR_Pi%d",j,j), new TH1F(Form("Dmatch_Best_UNR_Pi%d",j),Form("D^{max}_{match} #pi_{%d}",j),200,0,1));
  }

  //Extra Histos for K3pi testing mode (mode 1)
  BookHisto("SpectrometerGigaTrackerMatchingBase/hIsValidK3piEvent", new TH1F("IsValidK3piEvent","Valid K3piEvents",100,0,1e6));
  BookHisto("hGTK3PosXY", new TH2F("GTK3PosXY","Hit position in GTK3; X [mm]; Y [mm]",240,-36,36,120,-18,18));

  BookHisto("Pi1/CDA_/hCDA", new TH1F("CDA_Pi1","CDA of #pi_{1} and K tracks (Disc20); CDA [mm]",500,0,80));
  BookHisto("Pi2/CDA_/hCDA", new TH1F("CDA_Pi2","CDA of #pi_{2} and K tracks (Disc20); CDA [mm]",500,0,80));
  BookHisto("Pi3/CDA_/hCDA", new TH1F("CDA_Pi3","CDA of #pi_{3} and K tracks (Disc20); CDA [mm]",500,0,80));	
  BookHisto("Pi1/CDA_/hCDAdt", new TH2F("CDAdtPi1","CDA vs t_{GTKcand}-t_{KTAG} (Disc<20) #pi_{1}; t_{GTKcand}-t_{KTAG} [ns] ; CDA [mm] ",300,-1.2,1.2,250,0,50));
  BookHisto("Pi2/CDA_/hCDAdt", new TH2F("CDAdtPi2","CDA vs t_{GTKcand}-t_{KTAG} (Disc<20) #pi_{2}; t_{GTKcand}-t_{KTAG} [ns] ; CDA [mm] ",300,-1.2,1.2,250,0,50));
  BookHisto("Pi3/CDA_/hCDAdt", new TH2F("CDAdtPi3","CDA vs t_{GTKcand}-t_{KTAG} (Disc<20) #pi_{3}; t_{GTKcand}-t_{KTAG} [ns] ; CDA [mm] ",300,-1.2,1.2,250,0,50));

  BookHisto("hDiscPrime", new TH1F("DiscPrime","Discriminant for potential GTK K prime candidates",35,0,7));
  BookHisto("hDiscsubPrime", new TH1F("DiscsubPrime","Discriminant for GTK K subprime candidates",100,0,20));

  BookHisto("hdtCandidateAfterDiscCut", new TH1F("dtCandidateAfterDiscCut","Time diference between GTK candidate time and KTAG reference time (After Disc Cut); t_{GTK}-t_{KTAG}",300,-1.2,1.2));
  BookHisto("hdtCandidateAfterDiscCut_BigScale", new TH1F("dtCandidateAfterDiscCut_BigScale","Time diference between GTK candidate time and KTAG reference time (After Disc Cut); t_{GTK}-t_{KTAG}",1000,-22,22));

  BookHisto("hdtCandidateAfterDiscCutDisc40", new TH1F("dtCandidateAfterDiscCutDisc40","Time diference between GTK candidate time and KTAG reference time (After Disc Cut D<40); t_{GTK}-t_{KTAG}",300,-1.2,1.2));

  BookHisto("hdtCandidateAfterDiscCut_Background", new TH1F("dtCandidateAfterDiscCut_Background","Time diference between GTK candidate time and KTAG reference time (After Disc Cut Not passed); t_{GTK}-t_{KTAG}",300,-1.2,1.2));
  BookHisto("hdtCandidateAfterDiscCut_BigScale_Background", new TH1F("dtCandidateAfterDiscCut_BigScale_Background","Time diference between GTK candidate time and KTAG reference time (After Disc Cut - Not passed); t_{GTK}-t_{KTAG}",1000,-22,22));

  BookHisto("hNNoK3piCandidates", new TH1F("NNoK3piCandidates","Number of cases with NO GTK candidates",1,0,1));

  //Correlation studies ----
  BookHisto("CorrelationStudy/P_ThetaX", new TProfile("P_ThetaX","Correlation Test (GTK Candidate): |p| & #theta_{x} ; |p| [GeV] ; #theta_{x}",100,70,80)); //,100,0.008,0.0016));
  BookHisto("CorrelationStudy/P_ThetaY", new TProfile("P_ThetaY","Correlation Test (GTK Candidate): |p| & #theta_{y} ; |p| [GeV] ; #theta_{y}",100,70,80)); //,100,0.008,0.0016));
  BookHisto("CorrelationStudy/P_X", new TProfile("P_X","Correlation Test (GTK Candidate): |p| & X ; |p| [GeV] ; X [mm]",100,70,80));
  BookHisto("CorrelationStudy/P_Y", new TProfile("P_Y","Correlation Test (GTK Candidate): |p| & Y ; |p| [GeV] ; Y [mm]",100,70,80));

  BookHisto("CorrelationStudy/ThetaX_ThetaY", new TProfile("ThetaX_ThetaY","Correlation Test (GTK Candidate): #theta_{x} & #theta_{y} ; #theta_{x} ; #theta_{y}",100,0.0008,0.0016));
  BookHisto("CorrelationStudy/ThetaX_X", new TProfile("ThetaX_X","Correlation Test (GTK Candidate): #theta_{x} & X ; #theta_{x} ; X [mm] ",100,0.0008,0.0016));
  BookHisto("CorrelationStudy/ThetaX_Y", new TProfile("ThetaX_Y","Correlation Test (GTK Candidate): #theta_{x} & Y ; #theta_{x} ; Y [mm] ",100,0.0008,0.0016));

  BookHisto("CorrelationStudy/ThetaY_X", new TProfile("ThetaY_X","Correlation Test (GTK Candidate): #theta_{y} & X ; #theta_{y} ; X [mm] ",100,-0.0005,0.0005));	
  BookHisto("CorrelationStudy/ThetaY_Y", new TProfile("ThetaY_Y","Correlation Test (GTK Candidate): #theta_{y} & Y ; #theta_{y} ; Y [mm] ",100,-0.0005,0.0005)); 

  BookHisto("CorrelationStudy/X_Y", new TProfile("X_Y","Correlation Test (GTK Candidate): X & Y ; X [mm] ; Y [mm] ",140,-35,35));	

  BookHisto("pi1Momentum_DownstreamTrackChecker",new TH1F("pi1Mom_DownstreamTrackChecker","#pi_{1} momentum (Testing use of Track input); p_{x} GeV",150,0,75));

  //Discriminant plots----
  BookHisto("DiscriminantPlots/hDiscMom", new TH1F("DiscMom","Discriminant variable 1. |p_{GTK}|-|p_{K3pi}|;|p_{GTK}|-|p_{K3pi}| [GeV]",500,-10,10));
  BookHisto("DiscriminantPlots/hDiscThetax", new TH1F("DiscThetax","Discriminant variable 2. #theta_{X:GTK} - #theta_{X:K3pi} ; #theta_{X:GTK} - #theta_{X:K3pi}",500,-0.0005,0.0005));
  BookHisto("DiscriminantPlots/hDiscThetay", new TH1F("DiscThetay","Discriminant variable 3. #theta_{Y:GTK} - #theta_{Y:K3pi} ; #theta_{Y:GTK} - #theta_{Y:K3pi}",500,-0.0005,0.0005));
  BookHisto("DiscriminantPlots/hDiscXGTK3", new TH1F("DiscX","Discriminant variable 4. X_{GTK3}-X_{K3pi} ; X_{GTK3}-X_{K3pi}",500,-50,50));
  BookHisto("DiscriminantPlots/hDiscYGTK3", new TH1F("DiscY","Discriminant variable 5. Y_{GTK3}-Y_{K3pi} ; Y_{GTK3}-Y_{K3pi}",500,-50,50));

  BookHisto("DiscriminantPlots/hDiscMomX", new TH1F("DiscMomX","Discriminant variable study p_{GTK_X}-p_{K3pi_X};p_{GTK_X}-p_{K3pi_X} [GeV]",200,-0.1,0.1));
  BookHisto("DiscriminantPlots/hDiscMomY", new TH1F("DiscMomY","Discriminant variable study p_{GTK_Y}-p_{K3pi_Y};p_{GTK_Y}-p_{K3pi_X} [GeV]",200,-0.1,0.1));
  BookHisto("DiscriminantPlots/hDiscMomZ", new TH1F("DiscMomZ","Discriminant variable study p_{GTK_Z}-p_{K3pi_Z};p_{GTK_Z}-p_{K3pi_X} [GeV]",200,-10,10));

  BookHisto("DiscriminantPlots/hDiscriminant",new TH1F("Discriminant","Discriminant",450,0,4500));
  BookHisto("DiscriminantPlots/hDiscriminantLimRange",new TH1F("DiscriminantLimRange","Discriminant",150,0,75));

  BookHisto("DiscriminantPlots/hBestCandDiscriminant", new TH1F("BestCandDiscriminant","Discriminant for Best Candidate",150,0,75));
  BookHisto("DiscriminantPlots/hBestCandDiscriminant_Full", new TH1F("BestCandDiscriminant_Full","Discriminant for Best Candidate",450,0,4500));
  BookHisto("DiscriminantPlots/hBestCandDiscriminant_dtCand",new TH2F("BestCandDiscriminant_dtCand","Discriminant for best candidate vs #Delta t_{Candidate}",100,-1.2,1.2,150,0,70));

  BookHisto("hDeltaSlopeXVsXGTK", new TH2F("DeltaSlopeXVsXGTK","#Delta(#Theta_{X}) vs x_{GTK3} (Using GTK K candidate X) ; x^{GTK}|_{z=104.2 m} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}} ",300,-30,30,300,-2e-3,2e-3)); 
  BookHisto("hDeltaSlopeXVsXK3pi", new TH2F("DeltaSlopeXVsXK3pi","#Delta(#Theta_{X}) vs x_{GTK3} (Using K3pi K candidate X) ; x^{K3#pi}|_{z=104.2 m} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}}",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeXVsDXGTK", new TH2F("DeltaSlopeXVsDXGTK","#Delta(#Theta_{X}) vs #Delta x ; x^{GTK3}_{GTK}-x^{GTK3}_{K3#pi} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsYGTK", new TH2F("DeltaSlopeYVsYGTK","#Delta(#Theta_{Y}) vs y_{GTK3} (Using GTK K candidate Y) ; y^{GTK}|_{z=104.2 m} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsYK3pi", new TH2F("DeltaSlopeYVsYK3pi","#Delta(#Theta_{Y}) vs y_{GTK3} (Using K3pi K candidate Y) ; y^{K3#pi}|_{z=104.2 m} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsDYGTK", new TH2F("DeltaSlopeYVsDYGTK","#Delta(#Theta_{Y}) vs #Delta y ; y^{GTK3}_{GTK}-y^{GTK3}_{K3#pi} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));

  BookHisto("hDeltaSlopeXVsXGTK_Disc20", new TH2F("DeltaSlopeXVsXGTK_Disc20","#Delta(#Theta_{X}) vs x_{GTK3} (Using GTK K candidate X) [D<20] ; x^{GTK}|_{z=104.2 m} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}} ",300,-30,30,300,-2e-3,2e-3));
  BookHisto("hDeltaSlopeXVsXK3pi_Disc20", new TH2F("DeltaSlopeXVsXK3pi_Disc20","#Delta(#Theta_{X}) vs x_{GTK3} (Using K3pi K candidate X) [D<20]; x^{K3#pi}|_{z=104.2 m} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}}",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeXVsDXGTK_Disc20", new TH2F("DeltaSlopeXVsDXGTK_Disc20","#Delta(#Theta_{X}) vs #Delta x [D<20]; x^{GTK3}_{GTK}-x^{GTK3}_{K3#pi} [mm] ; #theta_{x_{GTK}} - #theta_{x_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsYGTK_Disc20", new TH2F("DeltaSlopeYVsYGTK_Disc20","#Delta(#Theta_{Y}) vs y_{GTK3} (Using GTK K candidate Y) [D<20] ;  y^{GTK}|_{z=104.2 m} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsYK3pi_Disc20", new TH2F("DeltaSlopeYVsYK3pi_Disc20","#Delta(#Theta_{Y}) vs y_{GTK3} (Using K3pi K candidate Y) [D<20] ; y^{K3#pi}|_{z=104.2 m} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));
  BookHisto("hDeltaSlopeYVsDYGTK_Disc20", new TH2F("DeltaSlopeYVsDYGTK_Disc20","#Delta(#Theta_{Y}) vs #Delta y [D<20] ; y^{GTK3}_{GTK}-y^{GTK3}_{K3#pi} [mm] ; #theta_{y_{GTK}} - #theta_{y_{K3#pi}} ",300,-30,30,300,-0.5e-3,0.5e-3));

  BookHisto("h2DAngularDistX", new TH2F("2DAngularDistX","2D distibution showing slope dx/dz vs x(atGTK3) ; x|_{z=104.2 m} [mm] ; dx/dz",100,-30,30,100,-2e-3,2e-3));
  BookHisto("h2DAngularDistY", new TH2F("2DAngularDistY","2D distibution showing slope dy/dz vs y(atGTK3) ; y|_{z=104.2 m} [mm] ; dy/dz",100,-17,17,100,-1e-3,1e-3));

  BookHisto("h2DAngularDistX_Disc20", new TH2F("2DAngularDistX_Disc20","2D distibution showing slope dx/dz vs x(atGTK3) (Disc<20); x|_{z=104.2 m} [mm] ; dx/dz",100,-30,30,100,-2e-3,2e-3));
  BookHisto("h2DAngularDistY_Disc20", new TH2F("2DAngularDistY_Disc20","2D distibution showing slope dy/dz vs y(atGTK3) (Disc<20); y|_{z=104.2 m} [mm] ; dy/dz",100,-17,17,100,-1e-3,1e-3));

  BookHisto("h2DAngularDistX_Disc40", new TH2F("2DAngularDistX_Disc40","2D distibution showing slope dx/dz vs x(atGTK3) (Disc<40); x|_{z=104.2 m} [mm] ; dx/dz",100,-30,30,100,-2e-3,2e-3));
  BookHisto("h2DAngularDistY_Disc40", new TH2F("2DAngularDistY_Disc40","2D distibution showing slope dy/dz vs y(atGTK3) (Disc<40); y|_{z=104.2 m} [mm] ; dy/dz",100,-17,17,100,-1e-3,1e-3));

  BookHisto("hPxDist", new TH1F("PxDist","Candidate p_{x} ; p_{x} [GeV]",200,-0.25,0.25));
  BookHisto("hPyDist", new TH1F("PyDist","Candidate p_{y} ; p_{y} [GeV]",200,-0.25,0.25));
  BookHisto("hPzDist", new TH1F("PzDist","Candidate p_{z} ; p_{z} [GeV]",200,65,85));
  BookHisto("hSlopeX", new TH1F("SlopeX","Candidate dp_{x}/dp_{z} (Slope X) ; dp_{x}/dp_{z} [rad]",200,-0.0025,0.0025));
  BookHisto("hDELTA", new TH1F("DELTA","Rotation Angle For Beam ; [rad] ",200,-0.005,0.005));

  BookHisto("hBestCandp", new TH1F("BestCandp","Momentum of Candidate with Lowest Discriminant; |p_{K}| GeV",80,71,79));

  BookHisto("hGTKmomDiscCutPassed", new TH1F("GTKmomDiscCutPassed","Momentum of Candidate Passing Discriminant cut; |p_{K}| GeV",80,71,79));
  BookHisto("hGTKmomDiscCutNOTPassed", new TH1F("GTKmomDiscCutNOTPassed","Momentum of Candidate NOT Passing Discriminant cut; |p_{K}| GeV",80,71,79));
  BookHisto("hCorrespondingiK3piMom", new TH1F("CorrespondingiK3piMom","Momentum of K3pi candidate corresponding to good GTK canidate; |p_{K}| [GeV]",80,71,79));
  BookHisto("hNGoodGTKK3piCands", new TH1F("NGoodGTKK3piCands","Number of Events with a GTK candidate associated to a K3#pi candidate",30,0,30));

  BookHisto("hGTKmomDiscCutPassedDisc40", new TH1F("GTKmomDiscCutPassedDisc40","Momentum of Candidate Passing Discriminant cut D<40; |p_{K}| GeV",80,71,79));
  BookHisto("hCorrespondingiK3piMomDisc40", new TH1F("CorrespondingiK3piMomDisc40","Momentum of K3pi candidate corresponding to good GTK canidate D<40; |p_{K}| [GeV]",80,71,79));
  BookHisto("hNGoodGTKK3piCandsDisc40", new TH1F("NGoodGTKK3piCandsDisc40","Number of Events with a GTK candidate associated to a K3#pi candidate D<40",30,0,30));

  BookHisto("DiscTest/hGTKmomDiscCut5Passed", new TH1F("GTKmomDiscCut5Passed","Momentum of K Candidates passing Dics<5; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut10Passed", new TH1F("GTKmomDiscCut10Passed","Momentum of K Candidates passing Dics<10; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut15Passed", new TH1F("GTKmomDiscCut15Passed","Momentum of K Candidates passing Dics<15; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut20Passed", new TH1F("GTKmomDiscCut20Passed","Momentum of K Candidates passing Dics<20; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut25Passed", new TH1F("GTKmomDiscCut25Passed","Momentum of K Candidates passing Dics<25; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut30Passed", new TH1F("GTKmomDiscCut30Passed","Momentum of K Candidates passing Dics<30; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut35Passed", new TH1F("GTKmomDiscCut35Passed","Momentum of K Candidates passing Dics<35; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut40Passed", new TH1F("GTKmomDiscCut40Passed","Momentum of K Candidates passing Dics<40; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut45Passed", new TH1F("GTKmomDiscCut45Passed","Momentum of K Candidates passing Dics<45; |p_{K}| GeV",80,71,79));
  BookHisto("DiscTest/hGTKmomDiscCut50Passed", new TH1F("GTKmomDiscCut50Passed","Momentum of K Candidates passing Dics<50; |p_{K}| GeV",80,71,79));

  BookHisto("DiscTest/hNDiscPassed5", new TH1F("NDiscPassed5","No. of Reconstructed GTK Candidates for Disc<5",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed10", new TH1F("NDiscPassed10","No. of Reconstructed GTK Candidates for Disc<10",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed15", new TH1F("NDiscPassed15","No. of Reconstructed GTK Candidates for Disc<15",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed20", new TH1F("NDiscPassed20","No. of Reconstructed GTK Candidates for Disc<20",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed25", new TH1F("NDiscPassed25","No. of Reconstructed GTK Candidates for Disc<25",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed30", new TH1F("NDiscPassed30","No. of Reconstructed GTK Candidates for Disc<30",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed35", new TH1F("NDiscPassed35","No. of Reconstructed GTK Candidates for Disc<35",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed40", new TH1F("NDiscPassed40","No. of Reconstructed GTK Candidates for Disc<40",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed45", new TH1F("NDiscPassed45","No. of Reconstructed GTK Candidates for Disc<45",31,-0.5,30.5));
  BookHisto("DiscTest/hNDiscPassed50", new TH1F("NDiscPassed50","No. of Reconstructed GTK Candidates for Disc<50",31,-0.5,30.5));

  BookHisto("DiscTest/hDeltaTCut5", new TH1F("DeltaTCut5","#Delta t_{GTK,Cand}-t_{KTAG} for D<5 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut10", new TH1F("DeltaTCut10","#Delta t_{GTK,Cand}-t_{KTAG} for D<10 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut15", new TH1F("DeltaTCut15","#Delta t_{GTK,Cand}-t_{KTAG} for D<15 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut20", new TH1F("DeltaTCut20","#Delta t_{GTK,Cand}-t_{KTAG} for D<20 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut25", new TH1F("DeltaTCut25","#Delta t_{GTK,Cand}-t_{KTAG} for D<25 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut30", new TH1F("DeltaTCut30","#Delta t_{GTK,Cand}-t_{KTAG} for D<30 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut35", new TH1F("DeltaTCut35","#Delta t_{GTK,Cand}-t_{KTAG} for D<35 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut40", new TH1F("DeltaTCut40","#Delta t_{GTK,Cand}-t_{KTAG} for D<40 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut45", new TH1F("DeltaTCut45","#Delta t_{GTK,Cand}-t_{KTAG} for D<45 Candidates",250,-1.2,1.2));
  BookHisto("DiscTest/hDeltaTCut50", new TH1F("DeltaTCut50","#Delta t_{GTK,Cand}-t_{KTAG} for D<50 Candidates",250,-1.2,1.2));

  BookHisto("hDmatchCorrectMatch_K3piTest", new TH1F("DmatchCorrectMatch_K3piTest","K3pi test : Dmatch Correct Match Made",2,-0.5,1.5));
  BookHisto("hDmatchUNRCorrectMatch_K3piTest", new TH1F("DmatchUNRCorrectMatch_K3piTest","K3pi test : DmatchUNR Correct Match Made",2,-0.5,1.5));
  BookHisto("hMDCorrectMatch_K3piTest", new TH1F("MDCorrectMatch_K3piTest","K3pi test : MD Correct Match Made",2,-0.5,1.5));

}

//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::DefineMCSimple(){

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::StartOfBurstUser(){


}

void SpectrometerGigaTrackerMatching::Process(int iEvent){

  //std::cout<<"K-Pi Matching Begin...Event No.:"<<iEvent<<std::endl;
  TRecoGigaTrackerEvent* GTKEvent  = GetEvent<TRecoGigaTrackerEvent>();
  fGTKevt = GTKEvent;	
  Int_t nRecoCand = fGTKevt->GetNCandidates(); 

  // Options : 
  // (0) [Default] General GTK-Spectrometer matching mode
  // (1) K3pi mode (for testing and evaluation of matching procedure(s)) 
  AddParam("MatchingMode",&fMatchingMode,0); //standard is General GTK-Spectrometer matching mode

  fKTAGTime = -100000.0;
  fK3piTime = -100001.0;

  Int_t Nloop = -1;
  if(fMatchingMode == 1) Nloop=1;
  else if(fMatchingMode == 0){
    std::vector<DownstreamTrack> SpectrometerTracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");	
    Double_t NSpectrometerTracks = SpectrometerTracks.size();
    Nloop=NSpectrometerTracks;
    fContainer.clear(); //For a new event clear the Output vector.
  }

  for(Int_t I=0;I<Nloop;I++){ //loop over spectrometer tracks

    ResetOutputs(); //.clear() all output vectors. --- See .hh file for this method.

    if(fMatchingMode == 1){

      //*************Get Reconstructed GTK Candidates
      nRecoCand = fGTKevt->GetNCandidates(); 

      if(GetL0Data()->GetDataType() !=0x1 ) return;	//Selecting physics trigger....
      if(fGTKevt==NULL) return;   //if there is no GTK even then we cant do GTK-Spectrometer matching
      //*************Get output from K3piStrictSelection***************************************
      fIsK3pi = *(Bool_t*)GetOutput("K3piStrictSelection.EventSelected");	//Get valid K3pi events
      if(fIsK3pi == 0) return;	//We only want to reconstruct events with valid K3pi events	//For pileup we only want to reconstruct things without valid K3pi!
      if(fIsK3pi == 1){
        FillHisto("SpectrometerGigaTrackerMatchingBase/hIsValidK3piEvent",iEvent);
        FillHisto("hNK3piCandidates",0);	
        fNK3piCandidates+=1;	//Count number of K3pi candidates
        fNCandidateEvents+=1;
      }
      SetK3piEventVariables(); 
      //***************************************************************************************

    }
    else if(fMatchingMode == 0){
      fNCandidateEvents+=1;
      //**********Take downstream track(s) input******************************************
      //get info direct from tracks
      std::vector<DownstreamTrack> SpectrometerTracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");	
      //if(SpectrometerTracks.size() == 0) return;
      fTrackID = SpectrometerTracks[I].GetTrackID();	
      if(fGTKevt->GetErrorMask()==0){ 
        FillHisto("NGTKErrorMask",0); 	//count how many events do NOT have an associated GTK error mask
        FillHisto("nRecoCand_ErrorMaskOff",nRecoCand);
      }
      else{
        FillHisto("nRecoCand_ErrorMaskActivated",nRecoCand);
        FillHisto("NGTKErrorMask",1); 				//count how many events DO have an associated GTK error mask
      }

      if(nRecoCand>250){
        FillHisto("nRecoCandExeeds250",1);
        if(fGTKevt->GetErrorMask()==0) FillHisto("nRecoCandExeeds250_GTKerrorMask",0);
        else FillHisto("nRecoCandExeeds250_GTKerrorMask",1);
        return; //reject events with more than 250 GTK candidates reconstructed.				
      }
      //CHODs time.............................................................................................................
      //################################################################################
      if(SpectrometerTracks[I].GetNCHODAssociationRecords()!=1) return; //require exactly 1 associated candidate
      //################################################################################
      TRecoCHODCandidate *CHODCand = SpectrometerTracks[I].GetCHODCandidate(0); //there is only 1 candidate!!        
      Double_t CHODCandTime = CHODCand->GetTime();    //get time for associated CHOD candidate
      //###################################################################################################
      if(SpectrometerTracks[I].GetNNewCHODAssociationRecords()!=1) return; //require exactly 1 associated candidate
      //###################################################################################################
      TRecoNewCHODHit *NewCHODCand = SpectrometerTracks[I].GetNewCHODCandidate(0); //there is only 1 candidate
      Double_t NewCHODCandTime = NewCHODCand->GetTime();    //get time for associated NewCHOD candidate

      Double_t sigma_CHOD=0.3;
      Double_t sigma_NewCHOD=1.0;     
      //Double_t CHODOffset=0.36;       //calculated from fits to histograms
      //Double_t NewCHODOffset=-0.16;   //calculated from fits to histograms
      fK3piTime =( (CHODCandTime/(sigma_CHOD*sigma_CHOD)) + (NewCHODCandTime/(sigma_NewCHOD*sigma_NewCHOD)) ) / ( (1/(sigma_CHOD*sigma_CHOD)) + (1/(sigma_NewCHOD*sigma_NewCHOD))  );

      //Cedar/KTAg time.........................................................................................................
      TRecoCedarEvent* CedarEvent  = GetEvent<TRecoCedarEvent>();
      Int_t NCedarCand = CedarEvent->GetNCandidates();
      Int_t NGoodCEDARCand = 0;
      for (Int_t i=0; i<NCedarCand; i++){
        TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(i));
        if (Ccand->GetNSectors()>4) NGoodCEDARCand++;
      }
      //require a good candidate
      if (!NGoodCEDARCand) return;

      Double_t BestTimeDiff=100000000.0;
      Double_t CedarCand_Best_Time = 0;

      for(Int_t i=0; i<NCedarCand; i++){
        TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(i)); //loping over all cedar candidates including bad ones

        if(Ccand->GetNSectors()<5) continue; //look only at good cedar candidates (Extra careful)

        Double_t CedarCandTime = Ccand->GetTime();
        Double_t deltaT_CedarCandtime_piTime=CedarCandTime-fK3piTime;

        if(i==0){
          BestTimeDiff=deltaT_CedarCandtime_piTime;
          CedarCand_Best_Time=CedarCandTime;
        }

        if( abs(deltaT_CedarCandtime_piTime) < abs(BestTimeDiff)){    //if absolute time difference is smallar for the current candidate...
          BestTimeDiff=deltaT_CedarCandtime_piTime;
          CedarCand_Best_Time=CedarCandTime;

        }

      }

      fKTAGTime = CedarCand_Best_Time; // Recalculated using weighted mean times...
      //**************************************************************************************
    }

    //For GTK Candidates...
    if(I==0){ //Only need to do the following once regardless of the number of dowsntream tracks in the event 
      fNRecoCand += nRecoCand; //count up total number of GTK candidates reconstructed 
      if(nRecoCand == 0){
        fNRecoNoCand +=1; //count up number of times no candidates are reconstructed
        FillHisto("hNNoK3piCandidates",0);
      }
      if(nRecoCand >1){
        fNRecoMoreThanOne +=1;
      }
      fEfficiency = (Double_t)fNRecoCand/(Double_t)(fNRecoCand+fNRecoNoCand); //efficiency calculated for Reconstruction with current values
      FillHisto("GTKCand/hNReconstructedCandidates",nRecoCand);

      //Histograms for GTK candidates
      for(Int_t i=0 ; i<nRecoCand; i++){
        TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(fGTKevt->GetCandidate(i));
        TVector3 p_cand = GTKCand->GetMomentum();
        Double_t dxdz_cand = p_cand.X()/p_cand.Z();
        Double_t dydz_cand = p_cand.Y()/p_cand.Z();
        FillHisto("GTKCand/hpCand",p_cand.Mag()*0.001);
        FillHisto("GTKCand/hCandidateHitPosGTK1",GTKCand->GetPosition(0).X(),GTKCand->GetPosition(0).Y());
        FillHisto("GTKCand/hCandidateHitPosGTK2",GTKCand->GetPosition(1).X(),GTKCand->GetPosition(1).Y());
        FillHisto("GTKCand/hCandidateHitPosGTK3",GTKCand->GetPosition(2).X(),GTKCand->GetPosition(2).Y()); 
        FillHisto("GTKCand/hCandAngleX",dxdz_cand);
        FillHisto("GTKCand/hCandAngleY",dydz_cand);
        FillHisto("GTKCand/pCAND_XvsAngleX_GTK3",GTKCand->GetPosition(2).X(),dxdz_cand);
        FillHisto("GTKCand/pCAND_YvsAngleY_GTK3",GTKCand->GetPosition(2).Y(),dydz_cand);
        FillHisto("GTKCand/hdtCandidate",GTKCand->GetTime()-fKTAGTime);
      }	
    }

    //initialize variables for K3pi testing...............................
    Double_t D[250]; //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Double_t BestDisc=1000000.0;
    Int_t BestIndex=-1;
    //////////////////////////////////////////////////////////
    //for cases of several potential prime GTK K candidates
    Int_t PotentialGTKKPrimeIndex[25] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}; ///stores indexes of good matches for D<5 
    Double_t Dprime[25] = {100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0}; //initialize to a really high number! - assume you cant get any nore than 25 possible Prime candidates
    Int_t NPrimeGTKCandidates = 0; //count the number of potential GTK K prime candidates
    //////////////////////////////////////////////////////////
    ///subprime GTK K candidates (5<D<20) -- potential mismatch candidates
    Int_t PotentialGTKKsubPrimeIndex[25] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}; ///stores indexes of subprime candidates 5<D<20
    Double_t Dsubprime[25] = {100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0,100000.0}; //initialize to a really high number! - assume you cant get any nore than 25 possible subPrime candidates
    Int_t NsubPrimeGTKCandidates = 0; //count the number of potential GTK K subprime candidates
    //......................................................................

    if(fMatchingMode == 1){ //for K3pi (Testing Mode)
      //Build Discriminant for K3pi sample isolation===========================
      //..................GTK - K3pi Discriminant variables....................
      ////////////////////
      //1. |Momentum| [GeV] 
      //2. Theta_{x}  (dxdz)
      //3. Theta_{y}  (dydz)
      //4. X_{GTK3}	[mm]
      //5. Y_{GTK3}	[mm]
      ////////////////////
      //K3pi  : K3piStictSelection (See : SetK3piEventVariables() function)
      //....................
      //K     : SpectrometerGigaTrackerMatching
      //1. GTKCand->GetMomentum().Mag();
      //2. GTKCand->GetMomentum().X()/GTKCand->GetMomentum().Z()
      //3. GTKCand->GetMomentum().Y()/GTKCand->GetMomentum().Z()
      //4. GTKCand->GetPosition.X();
      //5. GTKCand->GetPosition.Y();
      //.......................................................................

      Int_t NGTKCandidates = fGTKevt->GetNCandidates(); 	// nRecoCand = NGTKCandidates
      Double_t Bestp=0;
      Double_t GTKCandMom[250]; //NGTKCandidates -> fnRecoCandUpperLim=250 to set array that does not vary in size 
      Double_t BestGTKCandTime = 10000000.0;	
      //Double_t SecondBestDisc = 1000001.0; //commented out to avoid [-Wunused-but-set-variable] warning
      //Int_t SecondBestIndex=-1; //commented out to avoid [-Wunused-but-set-variable] warning

      if(NGTKCandidates!=0){   //this requirement should be superfluous given that if NRecoCand==0 then the following for loop is never entered!
        for(Int_t i=0; i<NGTKCandidates; i++){
          TRecoGigaTrackerCandidate* GTKCand = static_cast<TRecoGigaTrackerCandidate*>(fGTKevt->GetCandidate(i));
          TVector3 GTK3Pos = GTKCand->GetPosition(2);
          TVector3 GTKMom = GTKCand->GetMomentum();
          Double_t GTKCandTime = GTKCand->GetTime();
          FillHisto("hGTK3PosXY",GTK3Pos.X(),GTK3Pos.Y());

          //Discriminant plots (always go SpectrometerGigaTrackerMatching-K3piStrictSelection)
          FillHisto("DiscriminantPlots/hDiscMom",GTKMom.Mag()*0.001 - fp_K3pi);
          FillHisto("DiscriminantPlots/hDiscThetax",GTKMom.X()/GTKMom.Z() - fdxdz_K3pi);
          FillHisto("DiscriminantPlots/hDiscThetay",GTKMom.Y()/GTKMom.Z() - fdydz_K3pi);
          FillHisto("DiscriminantPlots/hDiscXGTK3",GTK3Pos.X() - fXGTK3_K3pi);
          FillHisto("DiscriminantPlots/hDiscYGTK3",GTK3Pos.Y() - fYGTK3_K3pi); 

          //////////////------slope difference --following 18/1/17 meeting-------------///////////////////////////
          FillHisto("hDeltaSlopeXVsXGTK",GTK3Pos.X() ,  GTKMom.X()/GTKMom.Z() - fdxdz_K3pi ); //using GTK x pos
          FillHisto("hDeltaSlopeXVsXK3pi",fXGTK3_K3pi,GTKMom.X()/GTKMom.Z() - fdxdz_K3pi  ); //using K3pi x pos 
          FillHisto("hDeltaSlopeXVsDXGTK",GTK3Pos.X() - fXGTK3_K3pi ,  GTKMom.X()/GTKMom.Z() - fdxdz_K3pi ); //delta x and delta theta x

          FillHisto("hDeltaSlopeYVsYGTK",GTK3Pos.Y() ,  GTKMom.Y()/GTKMom.Z() - fdydz_K3pi ); //using GTK y pos
          FillHisto("hDeltaSlopeYVsYK3pi",fYGTK3_K3pi , GTKMom.Y()/GTKMom.Z() - fdydz_K3pi  ); //using K3pi y pos
          FillHisto("hDeltaSlopeYVsDYGTK",GTK3Pos.Y() - fYGTK3_K3pi ,  GTKMom.Y()/GTKMom.Z() - fdydz_K3pi ); //delta y and delta theta y
          ////////////////////////////////////////////////////////////////////////////////////////////////

          //Study correlations between the 5 variables
          //For GTK Candidates
          //|p| and ThetaX
          FillHisto("CorrelationStudy/P_ThetaX",GTKMom.Mag()*0.001,GTKMom.X()/GTKMom.Z());
          //|p| and ThetaY
          FillHisto("CorrelationStudy/P_ThetaY",GTKMom.Mag()*0.001,GTKMom.Y()/GTKMom.Z());
          //|p| and X
          FillHisto("CorrelationStudy/P_X",GTKMom.Mag()*0.001,GTK3Pos.X());			
          //|p| and Y
          FillHisto("CorrelationStudy/P_Y",GTKMom.Mag()*0.001,GTK3Pos.Y());	
          //ThetaX and thetaY
          FillHisto("CorrelationStudy/ThetaX_ThetaY", GTKMom.X()/GTKMom.Z(),GTKMom.Y()/GTKMom.Z());
          //heta X and X
          FillHisto("CorrelationStudy/ThetaX_X", GTKMom.X()/GTKMom.Z(),GTK3Pos.X());
          //Theta X and Y
          FillHisto("CorrelationStudy/ThetaX_Y", GTKMom.X()/GTKMom.Z(),GTK3Pos.Y());
          //Theta Y and X
          FillHisto("CorrelationStudy/ThetaY_X",GTKMom.Y()/GTKMom.Z(),GTK3Pos.X());
          //Theta Y and Y
          FillHisto("CorrelationStudy/ThetaY_Y",GTKMom.Y()/GTKMom.Z(),GTK3Pos.Y());
          //X and Y
          FillHisto("CorrelationStudy/X_Y",GTK3Pos.X(),GTK3Pos.Y());

          //Momentum Histos ##################################################################
          FillHisto("DiscriminantPlots/hDiscMomX",GTKMom.X()*0.001 - fp_K3piX);
          FillHisto("DiscriminantPlots/hDiscMomY",GTKMom.Y()*0.001 - fp_K3piY);
          FillHisto("DiscriminantPlots/hDiscMomZ",GTKMom.Z()*0.001 - fp_K3piZ);

          std::vector<DownstreamTrack> SpectrometerTracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
          Int_t NegativePioniTrackIndex = fNegativePionIndex; 
          TRecoSpectrometerCandidate* Scand_piminus = SpectrometerTracks[NegativePioniTrackIndex].GetSpectrometerCandidate();
          Double_t p_pi1 = Scand_piminus->GetThreeMomentumBeforeMagnet().Mag();
          FillHisto("pi1Momentum_DownstreamTrackChecker",p_pi1*0.001);
          //######################################################################################

          //###############
          //look at the dx vs slope X 2D plot
          FillHisto("h2DAngularDistX",GTK3Pos.X(),GTKMom.X()/GTKMom.Z());
          FillHisto("h2DAngularDistY",GTK3Pos.Y(),GTKMom.Y()/GTKMom.Z());
          FillHisto("hPxDist",GTKMom.X()*0.001);
          FillHisto("hPyDist",GTKMom.Y()*0.001);
          FillHisto("hPzDist",GTKMom.Z()*0.001);
          FillHisto("hSlopeX",GTKMom.X()/GTKMom.Z());

          Double_t DELTA = acos(GTKMom.Z()/GTKMom.Mag());
          FillHisto("hDELTA",DELTA);
          //##############

          //Discriminant
          //	values from fits:
          Double_t Sigma_p = 0.288; // <- 19.01.17 | origional -> 0.295;
          Double_t Sigma_thetax = 2.68e-5; 
          Double_t Sigma_thetay = 2.64e-5; //<- 19.01.17 | Origional -> 2.69e-5;
          Double_t Sigma_x = 1.47; //-<19.01.17 | Origional -> 1.50;
          Double_t Sigma_y = 1.44; //<- 19.01.17 | Origional -> 1.47;
          D[i] = ((GTKMom.Mag()*0.001 - fp_K3pi)/ Sigma_p)*((GTKMom.Mag()*0.001 - fp_K3pi)/ Sigma_p)+(((GTKMom.X()/GTKMom.Z()) - fdxdz_K3pi)/Sigma_thetax)*(((GTKMom.X()/GTKMom.Z()) - fdxdz_K3pi)/Sigma_thetax) + (((GTKMom.Y()/GTKMom.Z()) - fdydz_K3pi)/Sigma_thetay)*(((GTKMom.Y()/GTKMom.Z()) - fdydz_K3pi)/Sigma_thetay) + (((GTK3Pos.X() - fXGTK3_K3pi)/Sigma_x))*(((GTK3Pos.X() - fXGTK3_K3pi)/Sigma_x)) + ((GTK3Pos.Y() - fYGTK3_K3pi)/Sigma_y)*((GTK3Pos.Y() - fYGTK3_K3pi)/Sigma_y) ;
          FillHisto("DiscriminantPlots/hDiscriminant",D[i]);	
          FillHisto("DiscriminantPlots/hDiscriminantLimRange",D[i]);
          Double_t Disc = D[i];
          GTKCandMom[i] = GTKMom.Mag()*0.001; //GeV

          if(i==0){
            BestDisc = Disc;
            BestIndex = i;
            Bestp=GTKMom.Mag()*0.001;
            BestGTKCandTime = GTKCandTime;
          }
          else if(i>0 && Disc < BestDisc){
            //new second best
            //SecondBestIndex = BestIndex; //commented out to avoid [-Wunused-but-set-variable] warning
            //SecondBestDisc = BestDisc;   //commented out to avoid [-Wunused-but-set-variable] warning
            //new best 
            BestDisc = Disc;
            BestIndex = i;
            Bestp=GTKMom.Mag()*0.001;
            BestGTKCandTime = GTKCandTime;
          }
          else{

          }			
        }
      }

      if(BestDisc !=	1000000.0 ) FillHisto("DiscriminantPlots/hBestCandDiscriminant",BestDisc); //The if statement ensures hist is not filled if there is no GTK candidate
      if(BestDisc !=  1000000.0 ) FillHisto("DiscriminantPlots/hBestCandDiscriminant_Full",BestDisc);
      if(Bestp != 0)	FillHisto("hBestCandp",Bestp); //If statement ensures it is only filled if there is at least 1 GTK Candidate
      if(BestDisc !=  1000000.0 && BestGTKCandTime != 10000000.0) FillHisto("DiscriminantPlots/hBestCandDiscriminant_dtCand",BestGTKCandTime - fKTAGTime,BestDisc);

      Double_t NGoodGTKK3piCandidates=0;
      Double_t NGoodGTKK3piCandidatesDisc40=0;
      for(Int_t i=0 ; i<NGTKCandidates ; i++){

        TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(fGTKevt->GetCandidate(i));
        if(D[i]<20){  
          FillHisto("hGTKmomDiscCutPassed",GTKCandMom[i]);
          FillHisto("hCorrespondingiK3piMom",fp_K3pi);
          FillHisto("hdtCandidateAfterDiscCut",GTKCand->GetTime()-fKTAGTime);
          FillHisto("hdtCandidateAfterDiscCut_BigScale",GTKCand->GetTime()-fKTAGTime);
          NGoodGTKK3piCandidates++;	
          fNK3piCandidatesPassingDiscCut++;

          //////////////------slope difference-AFTER D<20 cut---------///////////////////////////
          FillHisto("h2DAngularDistX_Disc20",GTKCand->GetPosition(2).X(),GTKCand->GetMomentum().X()/GTKCand->GetMomentum().Z());
          FillHisto("hDeltaSlopeXVsXGTK_Disc20",GTKCand->GetPosition(2).X() , ((GTKCand->GetMomentum()).X())/((GTKCand->GetMomentum()).Z()) - fdxdz_K3pi ); //using GTK x pos
          FillHisto("hDeltaSlopeXVsXK3pi_Disc20",fXGTK3_K3pi, ((GTKCand->GetMomentum()).X())/((GTKCand->GetMomentum()).Z()) - fdxdz_K3pi  ); //using K3pi x pos 
          FillHisto("hDeltaSlopeXVsDXGTK_Disc20",GTKCand->GetPosition(2).X() - fXGTK3_K3pi ,  ((GTKCand->GetMomentum()).X())/((GTKCand->GetMomentum()).Z()) - fdxdz_K3pi ); //delta x and delta theta x

          FillHisto("h2DAngularDistY_Disc20",GTKCand->GetPosition(2).Y(),GTKCand->GetMomentum().Y()/GTKCand->GetMomentum().Z());	
          FillHisto("hDeltaSlopeYVsYGTK_Disc20",GTKCand->GetPosition(2).Y() ,  ((GTKCand->GetMomentum()).Y())/((GTKCand->GetMomentum()).Z()) - fdydz_K3pi ); //using GTK y pos
          FillHisto("hDeltaSlopeYVsYK3pi_Disc20",fYGTK3_K3pi , ((GTKCand->GetMomentum()).Y())/((GTKCand->GetMomentum()).Z()) - fdydz_K3pi  ); //using K3pi y pos
          FillHisto("hDeltaSlopeYVsDYGTK_Disc20",GTKCand->GetPosition(2).Y() - fYGTK3_K3pi ,  ((GTKCand->GetMomentum()).Y())/((GTKCand->GetMomentum()).Z()) - fdydz_K3pi ); //delta y and delta theta y
          ////////////////////////////////////////////////////////////////////////////////////////////////	

          ////if D<5 the we have a potentially prime GTK candidate..............
          if(D[i]<=5.){
            PotentialGTKKPrimeIndex[NPrimeGTKCandidates] = i; //fill the element of the array which is equal to the No of Prime GTK K Candidates counted so far. 

            Dprime[NPrimeGTKCandidates] = D[i];
            FillHisto("hDiscPrime",Dprime[NPrimeGTKCandidates]);
            NPrimeGTKCandidates++;
          }
          else if(D[i]>5. && D[i]<20.){
            PotentialGTKKsubPrimeIndex[NsubPrimeGTKCandidates] = i; //fill the element of the array which is equal to the No of Prime GTK K Candidates counted so far. 

            Dsubprime[NsubPrimeGTKCandidates] = D[i];
            FillHisto("hDiscsubPrime",Dsubprime[NsubPrimeGTKCandidates]);
            NsubPrimeGTKCandidates++;
          }

        }
        else{
          fNK3piCandidatesNotPassingDiscCut++;
          FillHisto("hGTKmomDiscCutNOTPassed",GTKCandMom[i]);
          FillHisto("hdtCandidateAfterDiscCut_Background",GTKCand->GetTime()-fKTAGTime);
          FillHisto("hdtCandidateAfterDiscCut_BigScale_Background",GTKCand->GetTime()-fKTAGTime);
        }

        if(D[i]<40){
          FillHisto("hGTKmomDiscCutPassedDisc40",GTKCandMom[i]);
          FillHisto("hCorrespondingiK3piMomDisc40",fp_K3pi);
          FillHisto("hdtCandidateAfterDiscCutDisc40",GTKCand->GetTime()-fKTAGTime);
          NGoodGTKK3piCandidatesDisc40++;

          FillHisto("h2DAngularDistX_Disc40",GTKCand->GetPosition(2).X(),GTKCand->GetMomentum().X()/GTKCand->GetMomentum().Z());
          FillHisto("h2DAngularDistY_Disc40",GTKCand->GetPosition(2).Y(),GTKCand->GetMomentum().Y()/GTKCand->GetMomentum().Z());

        }

      }
      FillHisto("hNGoodGTKK3piCands",NGoodGTKK3piCandidates);
      FillHisto("hNGoodGTKK3piCandsDisc40",NGoodGTKK3piCandidatesDisc40);

      //save the unsoted array
      Double_t DprimeUnsorted[25];
      Double_t PotentialGTKKPrimeIndexUnsorted[25];
      for(Int_t i = 0 ; i<25 ; i++) DprimeUnsorted[i] = Dprime[i]; //fill array
      for(Int_t i = 0 ; i<25 ; i++) PotentialGTKKPrimeIndexUnsorted[i] = PotentialGTKKPrimeIndex[i]; //fill array

      //D<5 GTK K prime candidates :: ORDERING
      sort(	Dprime , Dprime + 25);	//sorting
      //the printouts show it works!!!
      /*
         std::cout << "Sorted Array looks like :";
         for (Int_t k = 0; k<25; k++){
         std::cout<<","<<Dprime[k]<<",";
         }
         std::cout<<"."<<std::endl;	

         std::cout<<"Sorted array of size "<<NPrimeGTKCandidates<<"looks like :";
         for(Int_t l = 0; l<NPrimeGTKCandidates ; l++){
         std::cout<<","<<Dprime[l]<<",";
         }
         std::cout<<"."<<std::endl;
         */
      //now we have to sort the indicies in the same way!
      //Need to search the sorted Dprime and find out which index goes where
      for(Int_t a = 0 ; a<25 ; a++){
        for(Int_t b = 0 ; b<25 ; b++){
          Int_t PotentialGTKKPrimeIndexValue = PotentialGTKKPrimeIndexUnsorted[b];
          if(Dprime[a] == DprimeUnsorted[b]) PotentialGTKKPrimeIndex[a] = PotentialGTKKPrimeIndexValue; //if the entries of the two Dprime arrays agree then set the new value of the PotentialGTKKPrimeIndex array to be the same as the index in the unsorted array which corresponds to this Dprime entry
        }
      }
      //TESTS 	show proper sorting.....
      /*
         std::cout<<"Unsorted array DprimeUnsorted =";
         for(Int_t k =0 ; k<25 ; k++){
         std::cout<<" "<<DprimeUnsorted[k]<<",";
         }
         std::cout<<"."<<std::endl;
         std::cout<<"Unsorted array Index = ";
         for(Int_t k =0 ; k<25 ; k++){
         std::cout<<" "<<PotentialGTKKPrimeIndexUnsorted[k]<<",";
         }
         std::cout<<"."<<std::endl;
         std::cout << "Sorted Array Dprime =";
         for (Int_t k = 0; k<25; k++){
         std::cout<<" "<<Dprime[k]<<",";
         }
         std::cout<<"."<<std::endl;
         std::cout<<"Sorted array Index = ";
         for(Int_t k =0 ; k<25 ; k++){
         std::cout<<" "<<PotentialGTKKPrimeIndex[k]<<",";
         }
         std::cout<<"."<<std::endl;
         std::cout<<".................................****************************************..............................."<<std::endl;
         */

      //Discriminant cut testing.....
      Int_t NPassed5 = 0;
      Int_t NPassed10 = 0;
      Int_t NPassed15 = 0;
      Int_t NPassed20 = 0;
      Int_t NPassed25 = 0;
      Int_t NPassed30 = 0;
      Int_t NPassed35 = 0;
      Int_t NPassed40 = 0;
      Int_t NPassed45 = 0;
      Int_t NPassed50 = 0;

      for(Int_t i=0 ; i<NGTKCandidates ; i++){
        TRecoGigaTrackerCandidate* GTKCand = static_cast<TRecoGigaTrackerCandidate*>(fGTKevt->GetCandidate(i));
        Double_t GTKCandTime = GTKCand->GetTime();

        if(D[i]<5){
          FillHisto("DiscTest/hGTKmomDiscCut5Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut5",GTKCandTime-fKTAGTime);
          NPassed5++;			
        }

        if(D[i]<10){
          FillHisto("DiscTest/hGTKmomDiscCut10Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut10",GTKCandTime-fKTAGTime);
          NPassed10++;
        }

        if(D[i]<15){
          FillHisto("DiscTest/hGTKmomDiscCut15Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut15",GTKCandTime-fKTAGTime);
          NPassed15++;
        }

        if(D[i]<20){
          FillHisto("DiscTest/hGTKmomDiscCut20Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut20",GTKCandTime-fKTAGTime);
          NPassed20++;
        }

        if(D[i]<25){
          FillHisto("DiscTest/hGTKmomDiscCut25Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut25",GTKCandTime-fKTAGTime);
          NPassed25++;
        }

        if(D[i]<30){
          FillHisto("DiscTest/hGTKmomDiscCut30Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut30",GTKCandTime-fKTAGTime);
          NPassed30++;
        }

        if(D[i]<35){
          FillHisto("DiscTest/hGTKmomDiscCut35Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut35",GTKCandTime-fKTAGTime);
          NPassed35++;
        }

        if(D[i]<40){
          FillHisto("DiscTest/hGTKmomDiscCut40Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut40",GTKCandTime-fKTAGTime);
          NPassed40++;
        }

        if(D[i]<45){
          FillHisto("DiscTest/hGTKmomDiscCut45Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut45",GTKCandTime-fKTAGTime);
          NPassed45++;
        }

        if(D[i]<50){
          FillHisto("DiscTest/hGTKmomDiscCut50Passed",GTKCandMom[i]);
          FillHisto("DiscTest/hDeltaTCut50",GTKCandTime-fKTAGTime);
          NPassed50++;
        }

      }
      FillHisto("DiscTest/hNDiscPassed5",NPassed5);
      FillHisto("DiscTest/hNDiscPassed10",NPassed10);
      FillHisto("DiscTest/hNDiscPassed15",NPassed15);
      FillHisto("DiscTest/hNDiscPassed20",NPassed20);
      FillHisto("DiscTest/hNDiscPassed25",NPassed25);
      FillHisto("DiscTest/hNDiscPassed30",NPassed30);
      FillHisto("DiscTest/hNDiscPassed35",NPassed35);
      FillHisto("DiscTest/hNDiscPassed40",NPassed40);
      FillHisto("DiscTest/hNDiscPassed45",NPassed45);
      FillHisto("DiscTest/hNDiscPassed50",NPassed50);
    } //end of K3pi testing analysis to isolate a suitable scenario for K-pi matching

    //////////////////////////////////
    Double_t PiTime[3] = {0.,0.,0.}; 

    //variables for matching
    Int_t KBestMatchedIndex[3] = {-1,-1,-1};
    Double_t KBestDmatch[3] = {1000000.0,1000000.0,1000000.0};

    //for unconstrained procedure 
    Int_t KBestMatchedIndexUNR[3] = {-1,-1,-1};
    Double_t KBestDmatchUNR[3] = {1000000.0,1000000.0,1000000.0};

    //matching procedure variables 
    Double_t MDbest[3] = {100000.0,100000.0,100000.0}; 
    Int_t MDbestIndex[3] = {-1,-1,-1}; 
    /////////////////////////////////////

    TRecoSpectrometerCandidate* Scand_pi1;// = new TRecoSpectrometerCandidate();
    TRecoSpectrometerCandidate* Scand_pi2;// = new TRecoSpectrometerCandidate();
    TRecoSpectrometerCandidate* Scand_pi3;// = new TRecoSpectrometerCandidate();

    //----------------------------
    // For Results Output
    Int_t PositiveMatch_DmatchProcedure[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Double_t PositiveMatchQuality_DmatchProcedure[250][3];	//nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size 
    Int_t PositiveMatch_DmatchProcedure_Nmatches[3] = {0,0,0}; 
    Int_t PositiveMatch_K3pi_CorrectMatch[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    //MD procedure
    Int_t PositiveMatch_MDProcedure[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Double_t PositiveMatchQuality_MDProcedure[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Int_t PositiveMatch_MDProcedure_Nmatches[3] = {0,0,0};
    Int_t PositiveMatch_K3pi_CorrectMatch_MD[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    //Dmatch Unrestricted Procedure
    Int_t PositiveMatch_DmUNRProcedure[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Double_t PositiveMatchQuality_DmUNRProcedure[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size
    Int_t PositiveMatch_DmUNRProcedure_Nmatches[3] = {0,0,0};
    Int_t PositiveMatch_K3pi_CorrectMatch_DmUNR[250][3];   //nRecoCand -> fnRecoCandUpperLim=250 to set array that does not vary in size

    for(Int_t i = 0 ; i<nRecoCand ; i++){ //initialize all elements to crazy values
      for(Int_t j = 0 ; j<3 ; j++){
        PositiveMatch_DmatchProcedure[i][j] = -99;  
        PositiveMatchQuality_DmatchProcedure[i][j] = -99.9;
        PositiveMatch_K3pi_CorrectMatch[i][j] = -2;
        //MD Procedure
        PositiveMatch_MDProcedure[i][j] = -99;
        PositiveMatchQuality_MDProcedure[i][j] = -99.9;
        PositiveMatch_K3pi_CorrectMatch_MD[i][j] = -2;
        //Dmatch Unrestricted
        PositiveMatch_DmUNRProcedure[i][j] = -99;
        PositiveMatchQuality_DmUNRProcedure[i][j] = -99.9;
        PositiveMatch_K3pi_CorrectMatch_DmUNR[i][j] = -2;
      }
    }
    //----------------------------
    Int_t NTracksToMatch = -1; //number of downstream track to match to 
    if(fMatchingMode == 0){
      NTracksToMatch = 1; //General case of matching 1 DownstreamTrack to GTK candidates (e.g Kpinunu) 
      std::vector<DownstreamTrack> SpectrometerTracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
      Scand_pi1 =SpectrometerTracks[I].GetSpectrometerCandidate();	//Single DownstreamTrack of interest
      Scand_pi2 = Scand_pi1; //just use a duplicate for now...
      Scand_pi3 = Scand_pi1; //just use a duplicate for now...
      //get a single DownstreamTrack (naming assumes it is a pion but it could be something else)....
    }
    else if(fMatchingMode == 1){
      NTracksToMatch = 3; //K3pi
      //using track info instead of directly passed
      std::vector<DownstreamTrack> SpectrometerTracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
      //pi1 = pi-
      Int_t NegativePionIndex = fNegativePionIndex; 
      Scand_pi1 =SpectrometerTracks[NegativePionIndex].GetSpectrometerCandidate();
      //pi2 = pi+ (1)         
      Int_t PositivePionIndex1 = fPositivePionIndex1; 
      Scand_pi2 =SpectrometerTracks[PositivePionIndex1].GetSpectrometerCandidate();
      //pi2 = pi+ (2)         
      Int_t PositivePionIndex2 = fPositivePionIndex2; 
      Scand_pi3 =SpectrometerTracks[PositivePionIndex2].GetSpectrometerCandidate();
    }
    else std::cout<<user_normal()<<"...***___ERROR___***..."<<std::endl;

    if(nRecoCand==0) fNoCandidate+=1;

    ///Matching Procedures................................................
    for(Int_t j = 0 ; j<NTracksToMatch ; j++){ //loop over the number of downstream tracks
      //for CDA calculation
      TVector3 B0(0.,0.,0.);
      TVector3 PpiTrack(0.,0.,0.);		
      if(fMatchingMode == 1){ //K3pi
        //Get the position and momentum for the three different pion candidates
        if(j==0){
          B0 = Scand_pi1->GetPositionBeforeMagnet(); //Reference position for pion track
          PpiTrack = Scand_pi1->GetThreeMomentumBeforeMagnet(); //Momentum for pion track for CDA
          PiTime[j] = fNegativePionCHODsTime; 
        }
        else if(j==1){
          B0 = Scand_pi2->GetPositionBeforeMagnet();
          PpiTrack = Scand_pi2->GetThreeMomentumBeforeMagnet();
          PiTime[j] = fPositivePion1CHODsTime; 
        }
        else if(j==2){
          B0 = Scand_pi3->GetPositionBeforeMagnet();
          PpiTrack = Scand_pi3->GetThreeMomentumBeforeMagnet();
          PiTime[j] = fPositivePion2CHODsTime; 
        }
        else{
          std::cout<<user_normal()<<"ERROR!!!"<<std::endl;
        }
      }
      else if(fMatchingMode == 0){ //general GTK-Spectrometer Matching
        //get the position and momentum of the downstream track 
        B0 = Scand_pi1->GetPositionBeforeMagnet();
        PpiTrack = Scand_pi1->GetThreeMomentumBeforeMagnet(); //(uncorrected) track momentum
        PiTime[j] = fK3piTime ;
      }
      TVector3 b = PpiTrack*(1/PpiTrack.Mag());       //unit vector for direction of pi track                 

      for(Int_t i = 0 ; i<nRecoCand ; i++){ //loop over GTK Candidate
        TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(fGTKevt->GetCandidate(i)); //Get GTK candidate

        //for CDA calculation
        TVector3 A0 = GTKCand->GetPosition(2);  //position vector at GTK3 - reference K track position
        TVector3 Pktrack = GTKCand->GetMomentum(); //get (uncorrected) momentum of the GTK candidate
        TVector3 a = Pktrack*(1/Pktrack.Mag()); //unit vector for direction of K track (GTK candidate)
        //CDA   //using official tool
        TwoLinesCDA *CDAToolA = new TwoLinesCDA();
        //straw track, then gtk track
        CDAToolA->SetLine1Point1(A0.X(),A0.Y(),A0.Z()); //Position of K GTKCand at GTK3
        CDAToolA->SetDir1(a); //unit vector for GTK K cand direction 
        CDAToolA->SetLine2Point1(B0.X(),B0.Y(),B0.Z());  //position of pi candidate before magnet
        CDAToolA->SetDir2(b); //unit vector for pi direction
        CDAToolA->ComputeVertexCDA();
        Double_t cda = CDAToolA->GetCDA();	

        //Prepare for MD matching....................................................
        TVector3 KpiVertex = CDAToolA->GetVertex();
        delete CDAToolA; 
        //Initialize 3vectors
        TVector3 VertexPosK(0.0,0.0,0.0);
        TVector3 VertexPosPi(0.0,0.0,0.0);
        TVector3 VertexMomK(0.0,0.0,0.0);
        TVector3 VertexMomPi(0.0,0.0,0.0);
        if(KpiVertex.Z() > 183311.0 || KpiVertex.Z() < 100000.0){
          //If ZVertex lies outside of the range of BlueTubeTracker set variables to "out of range" estimate values
          VertexPosK = KpiVertex;
          VertexPosPi = KpiVertex;
          VertexMomK = GTKCand->GetMomentum();
          VertexMomPi = PpiTrack;	
        }
        else{ //Use BlueTubeTracker to find exact values
          //extrapolate pion track to Zvertex
          if(fMatchingMode == 1){ //K3pi
            if (j==0) BlueTubeTracker::GetInstance()->SetCharge(-1); //pi - is track 1
            if (j==1) BlueTubeTracker::GetInstance()->SetCharge(+1); //pi + is track 2
            if (j==2) BlueTubeTracker::GetInstance()->SetCharge(+1); //pi + is track 3
          }
          else{ //Kpinunu
            BlueTubeTracker::GetInstance()->SetCharge(+1); //pi + (assume positive tracks (here simply labeled as pions BUT no assumption about the mass is made))
          }
          BlueTubeTracker::GetInstance()->SetInitialPosition(B0.X(), B0.Y(), B0.Z()); // Unit: mm (x, y, z) //position of pion before magnet
          BlueTubeTracker::GetInstance()->SetInitialMomentum(PpiTrack.X() , PpiTrack.Y(), PpiTrack.Z());  // Unit: MeV/c //momentum of pion before magnet
          BlueTubeTracker::GetInstance()->SetZFinal(KpiVertex.Z()); // Unit: mm //K-pi vertex
          BlueTubeTracker::GetInstance()->TrackParticle();
          VertexPosPi = BlueTubeTracker::GetInstance()->GetFinalPosition(); // Unit: mm
          VertexMomPi = BlueTubeTracker::GetInstance()->GetFinalMomentum(); // Unit: MeV/c

          //extrapolate GTK Kaon track to Zvetrex
          BlueTubeTracker::GetInstance()->SetCharge(+1); //K+
          BlueTubeTracker::GetInstance()->SetInitialPosition((GTKCand->GetPosition(2)).X(), (GTKCand->GetPosition(2)).Y(), (GTKCand->GetPosition(2)).Z());  // Unit: mm (x, y, z) //GTK K candidate GTK3 position
          BlueTubeTracker::GetInstance()->SetInitialMomentum((GTKCand->GetMomentum()).X() , (GTKCand->GetMomentum()).Y(), (GTKCand->GetMomentum()).Z()) ;  // Unit: MeV/c //GTK K candidate momentum
          BlueTubeTracker::GetInstance()->SetZFinal(KpiVertex.Z()); // Unit: mm //K-pi vertex
          BlueTubeTracker::GetInstance()->TrackParticle();
          VertexPosK = BlueTubeTracker::GetInstance()->GetFinalPosition(); // Unit: mm
          VertexMomK = BlueTubeTracker::GetInstance()->GetFinalMomentum(); // Unit: MeV/c
        }
        FillHisto("MDMatchingProcedure/hX_VertexPosK",VertexPosK.X());
        FillHisto("MDMatchingProcedure/hX_VertexPosPi",VertexPosPi.X());
        FillHisto("MDMatchingProcedure/hY_VertexPosK",VertexPosK.Y());
        FillHisto("MDMatchingProcedure/hY_VertexPosPi",VertexPosPi.Y());

        //Set outputs
        //Estimate of Vertex position for output.
        Double_t xvtx = VertexPosK.X() + sqrt( ((VertexPosK.X()-VertexPosPi.X())/2)*((VertexPosK.X()-VertexPosPi.X())/2) ); //xvtx=x1-|(x1-x2)/2| 
        Double_t yvtx = VertexPosK.Y() + sqrt( ((VertexPosK.Y()-VertexPosPi.Y())/2)*((VertexPosK.Y()-VertexPosPi.Y())/2) ); //yvtx=y1-|(y1-y2)/2|
        TVector3 vtx(xvtx,yvtx,KpiVertex.Z());
        fVertexPosition.push_back(vtx);
        //Momenta for output
        fBeamParticleMomentum.push_back(Pktrack);
        fCorrectedBeamParticleMomentum.push_back(VertexMomK); //Blue field corrected
        fTrackMomentum.push_back(PpiTrack);
        fCorrectedTrackMomentum.push_back(VertexMomPi); //Blue field corrected
        //cda for output
        fcda.push_back(cda);
        //..........

        //for all pi...
        //dt using GTK cand time and pion time (from CHODS)
        FillHisto("DmatchAltMatchingProcedure/hdeltat",GTKCand->GetTime() - PiTime[j]);
        FillHisto("DmatchAltMatchingProcedure/hdeltatCORR",GTKCand->GetTime() - PiTime[j] - 0.260); //apply zero offset correction!
        FillHisto("CDA_/hcda_All",cda); //give cda for all K and all pi (D<20 cut not applied yet and plotted once for each pi)
        FillHisto("MDMatchingProcedure/hdeltaX_KpiVertex", VertexPosK.X() - VertexPosPi.X() ); //delta x for MD Matching Procedure (sigma_x from stdev)
        FillHisto("MDMatchingProcedure/hdeltaY_KpiVertex", VertexPosK.Y() - VertexPosPi.Y() ); //delta y for MD Matching Procedure (sigma y from stdev)

        //set variable for delta t 
        Double_t dt_GTKKTAG = -1000000.0; 
        if(fMatchingMode == 1){ //K3pi mode
          dt_GTKKTAG = GTKCand->GetTime()-fKTAGTime;
        }
        else{ //General GTK-Spectrometer Matching mode
          dt_GTKKTAG = GTKCand->GetTime()-fKTAGTime; //use KTAG reference time
        }

        if(fMatchingMode ==1){
          if(D[i]>20){ //Discriminant cut for K3pi~~~
            continue; //Continue onto the the next GTK candidate....
          }
        }

        FillHisto("CDA_/hCDA",cda);
        FillHisto("CDA_/hCDAdt",dt_GTKKTAG,cda);
        //FillHisto("hCDASignalPDF",CDASignalPDF(cda));
        //FillHisto("hdtSignalPDF",dtSignalPDF( GTKCand->GetTime()-K3piCedarTime ));
        //FillHisto("hCDABackgroundPDF",CDABackgroundPDF(cda));
        //FillHisto("hdtBackgroundPDF",dtBackgroundPDF( GTKCand->GetTime()-K3piCedarTime ));	
        FillHisto(Form("Pi%d/CDA_/hCDA",j+1),cda);
        FillHisto(Form("Pi%d/CDA_/hCDAdt",j+1),dt_GTKKTAG,cda);

        //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        //+++++++++=Matching Procedure 1 : Dmatch+++++++++++++++++++++++++++
        if(fabs(dt_GTKKTAG)<0.6){ //need to have D<20 (cut applied already)to have good GTK Candidates and restrict dt so the fitting pdfs are valid		

          Double_t P_Sig = (NumericalIntegrationCDASig_VAL_new(cda)*NumericalIntegrationdtSig_VAL_new( fabs(dt_GTKKTAG) ) ) / (NumericalIntegrationCDASig_new()*NumericalIntegrationdtSig_new() );
          Double_t P_Bkg = (NumericalIntegrationCDABkg_VAL_new(cda)*NumericalIntegrationdtBkg_VAL_new( fabs(dt_GTKKTAG) ) ) / (NumericalIntegrationCDABkg_new()*NumericalIntegrationdtBkg_new() );
          Double_t Dmatch = (P_Sig*P_Bkg)/(P_Sig+P_Bkg);
          FillHisto("DmatchMatchingProcedure/NInsideOutsidedtWindow",1);
          FillHisto("DmatchMatchingProcedure/NewDmatch",Dmatch);

          FillHisto(Form("DmatchMatchingProcedure/NInsideOutsidedtWindowPi%d",j+1),1);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hPsig",j+1),P_Sig);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hPbkg",j+1),P_Bkg);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hPsig_Pbkg",j+1),P_Sig+P_Bkg);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hRsig",j+1),P_Sig/(P_Sig+P_Bkg));
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hRbkg",j+1),P_Bkg/(P_Sig+P_Bkg));
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hFnew",j+1),P_Sig/P_Bkg);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/NewDmatch",j+1),Dmatch);
          FillHisto(Form("DmatchMatchingProcedure/Pi%d/hPsigPbkg2D",j+1),P_Bkg,P_Sig);

          //restricted matching
          if(Dmatch>0.001){ //potential match -- look for the best
            FillHisto(Form("DmatchMatchingProcedure/Pi%d/NewDmatchPassedNotPassed",j+1),1); //passed

            PositiveMatch_DmatchProcedure[i][j] = 1; //match
            PositiveMatch_DmatchProcedure_Nmatches[j]++; 
            PositiveMatchQuality_DmatchProcedure[i][j] = Dmatch; //quantify quality with Dmatch value

            if(KBestDmatch[j] == 1000000.0 && KBestMatchedIndex[j] == -1 ){ //not yet set
              KBestMatchedIndex[j] = i;
              KBestDmatch[j] = Dmatch;
            }
            else if(Dmatch>KBestDmatch[j]){ //found a better match
              KBestMatchedIndex[j] = i;
              KBestDmatch[j] = Dmatch;
            }
            else{ //worse match....
            }
          }
          else{
            FillHisto(Form("DmatchMatchingProcedure/Pi%d/NewDmatchPassedNotPassed",j+1),0); //not passe
          }

          //unrestricted matching
          PositiveMatch_DmUNRProcedure[i][j] = 1; //match
          PositiveMatch_DmUNRProcedure_Nmatches[j]++;
          PositiveMatchQuality_DmUNRProcedure[i][j] = Dmatch; //quantify quality with Dmatch value 

          if(KBestDmatchUNR[j] == 1000000.0 && KBestMatchedIndexUNR[j] == -1 ){ //not yet set
            KBestMatchedIndexUNR[j] = i;
            KBestDmatchUNR[j] = Dmatch;
          }
          else if(Dmatch>KBestDmatchUNR[j]){ //found a better match
            KBestMatchedIndexUNR[j] = i;
            KBestDmatchUNR[j] = Dmatch;
          }
          else{ //worse match....
          }
        }
        else{
          FillHisto("DmatchMatchingProcedure/NInsideOutsidedtWindow",0); //outside time window.
          FillHisto(Form("DmatchMatchingProcedure/NInsideOutsidedtWindowPi%d",j+1),0); //outside time winder per pion
        }
        //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        //~~~~~~~~~~Matching Procedure 2 : Linear Matching Discriminant~~~~~
        //Build linear discriminant 
        //--current : values from below for x and y and estimate for delta t based on previous knowledge.... 
        //      (values from 6610 full run (GS300117 list) 2225_2227 11.02.17 )  -- out of date
        Double_t MDsigmaDT = 0.138; // <--160217 full list run value //0.256; //0.3055;
        Double_t MDsigmaX = 1.897; //1.927;
        Double_t MDsigmaY = 1.629; //1.571;
        Double_t DeltaX = VertexPosK.X() - VertexPosPi.X();
        Double_t DeltaY = VertexPosK.Y() - VertexPosPi.Y();
        Double_t DeltaT = GTKCand->GetTime() - fKTAGTime; //GTK time - KTAG reference time.
        Double_t MD = (DeltaX/MDsigmaX)*(DeltaX/MDsigmaX) + (DeltaY/MDsigmaY)*(DeltaY/MDsigmaY) + (DeltaT/MDsigmaDT)*(DeltaT/MDsigmaDT); //linear matching discriminant with time correction applied
        FillHisto("MDMatchingProcedure/MD",MD);
        FillHisto(Form("MDMatchingProcedure/MD_Pi%d",j+1),MD);

        PositiveMatch_MDProcedure[i][j] = 1; //match
        PositiveMatch_MDProcedure_Nmatches[j]++;
        PositiveMatchQuality_MDProcedure[i][j] = MD; //quantify quality with MD value (*Smaller = better*) 

        ///find the GTK K Candidate with the lowest MD for pion j
        if(MDbestIndex[j] == -1){ //none yet set
          MDbestIndex[j] = i; //store Kaon Index
          MDbest[j] = MD; //store best value of DM
        }
        else if(MD<MDbest[j]){ //found a case with SMALLER MD
          MDbestIndex[j] = i; //store Kaon Index
          MDbest[j] = MD; //store best value of DM        
        }
        else{ //larger MD so worse candidate and therefore ignore...
        }
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      }//end of loop on GTK candidates

      //FillHistos for best discriminants
      //Dmatch Procedure
      FillHisto(Form("DmatchMatchingProcedure/Pi%d/hDmatch_Best_Pi%d",j+1,j+1),KBestDmatch[j]);
      //MD procedure
      FillHisto(Form("MDMatchingProcedure/Pi%d/hMD_Best_Pi%d",j+1,j+1),MDbest[j]);
      //Dmatch unrestricted Procedure
      FillHisto(Form("DmatchAltMatchingProcedure/Pi%d/hDmatch_Best_UNR_Pi%d",j+1,j+1),KBestDmatchUNR[j]);

    } //end of loop on pion tracks

    //Evaluate Matching performance (only possible for K3pi!)
    if(fMatchingMode == 1){ //K3pi mode
      //look at how the matching has performed for the three pion tracks Using linear matching discriminant
      for(Int_t j=0; j<3; j++){ 

        //////////////Final Matching Results using my standard Dmatch procedure (applying D<20 and Dmatch>0.001)
        if(nRecoCand == 0){
          if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",0); //no GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",0); //no GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",0); //no GTK candidates
          fDmatchMatch_NNoMatch[j] +=1;
        }
        else if(D[BestIndex]>20){ //no good candidates to match to
          fDmatchMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",1); //no GOOD GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",1); //no GOOD GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",1); //no GOOD GTK candidates
        }
        else if(KBestMatchedIndex[j] == -1){
          fDmatchMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",2); //no GOOD GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",2); //no GOOD GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",2); //no GOOD GTK candidates
        }
        else if(D[BestIndex]<20 &&  KBestMatchedIndex[j] == BestIndex){ //match to best Candidate
          fDmatchMatch_NMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",3); //matched to the best GTK candidate
          if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",3); //matched to the best GTK candidate
          if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",3); //matched to the best GTK candidate
        }
        else if(D[BestIndex]<20 &&  KBestMatchedIndex[j] != BestIndex){ //dont match to the best candidate

          if(D[BestIndex]<5){ //must try to match only ti prime candidates

            Bool_t Matched = kFALSE;
            Bool_t Mismatched = kFALSE;					

            for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
              if(KBestMatchedIndex[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                fDmatchMatch_NMatch[j] +=1;
                if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",5); //matched to a PRime GTK Candidate
                Matched = kTRUE;
              }	
            }

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(KBestMatchedIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",6); //mismatched       
                if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",6); //mismatched
                if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",6); //mismatched
                fDmatchMatch_NMismatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Matched == kFALSE && Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",7); //fail - no match
              if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",7); //fail - no match
              if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",7); //fail - no match
              fDmatchMatch_NNoMatch[j] +=1;
            }


          }
          else if(D[BestIndex]>5 && D[BestIndex]<20){ //look only at sub-prime candidates

            Bool_t Mismatched = kFALSE;

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(KBestMatchedIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",9); //matched       
                if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",9); //matched
                if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",9); //matched
                fDmatchMatch_NMatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",10); //fail - no match
              if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",10); //fail - no match
              if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",10); //fail - no match
              fDmatchMatch_NNoMatch[j] +=1;
            }

          }
          else{ //error
            fDmatchMatch_NNoMatch[j] +=1;
            if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",-1);
            if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",-1);
            if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",-1);
          }


        }
        else{
          fDmatchMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedure/Pi1/MatchingResults",-2);
          if(j==1) FillHisto("DmatchMatchingProcedure/Pi2/MatchingResults",-2);
          if(j==2) FillHisto("DmatchMatchingProcedure/Pi3/MatchingResults",-2);
        }               
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////


        //////////////Final Matching Results using Linear Discriminant (no constraint of MD but D<20 cut imposed)
        if(nRecoCand == 0){
          if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",0); //no GTK candidates
          if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",0); //no GTK candidates
          if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",0); //no GTK candidates
          fMDMatch_NNoMatch[j] +=1;
        }
        else if(D[BestIndex]>20){ //no good candidates to match to
          fMDMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",1); //no GOOD GTK candidates
          if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",1); //no GOOD GTK candidates
          if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",1); //no GOOD GTK candidates
        }
        else if(MDbestIndex[j] == -1){
          fMDMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",2); //no GOOD GTK candidates
          if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",2); //no GOOD GTK candidates
          if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",2); //no GOOD GTK candidates
        }
        else if(D[BestIndex]<20 &&  MDbestIndex[j] == BestIndex){ //match to best Candidate
          fMDMatch_NMatch[j] +=1;
          if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",3); //matched to the best GTK candidate
          if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",3); //matched to the best GTK candidate
          if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",3); //matched to the best GTK candidate
        }
        else if(D[BestIndex]<20 &&  MDbestIndex[j] != BestIndex){ //dont match to the best candidate

          if(D[BestIndex]<5){ //must try to match only ti prime candidates

            Bool_t Matched = kFALSE;
            Bool_t Mismatched = kFALSE;					

            for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
              if(MDbestIndex[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                fMDMatch_NMatch[j] +=1;
                if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",5); //matched to a PRime GTK Candidate
                Matched = kTRUE;
              }	
            }

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(MDbestIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",6); //mismatched       
                if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",6); //mismatched 
                if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",6); //mismatched 
                fMDMatch_NMismatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Matched == kFALSE && Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",7); //fail - no match
              if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",7); //fail - no match
              if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",7); //fail - no match
              fMDMatch_NNoMatch[j] +=1;
            }


          }
          else if(D[BestIndex]>5 && D[BestIndex]<20){ //look only at sub-prime candidates

            Bool_t Mismatched = kFALSE;

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(MDbestIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",9); //matched       
                if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",9); //matched
                if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",9); //matched
                fMDMatch_NMatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",10); //fail - no match
              if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",10); //fail - no match
              if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",10); //fail - no match
              fMDMatch_NNoMatch[j] +=1;
            }

          }
          else{ //error
            fMDMatch_NNoMatch[j] +=1;
            if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",-1);
            if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",-1);
            if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",-1);
          }


        }
        else{
          fMDMatch_NNoMatch[j] +=1;
          if(j==0) FillHisto("MDMatchingProcedure/Pi1/MatchingResults",-2);
          if(j==1) FillHisto("MDMatchingProcedure/Pi2/MatchingResults",-2);
          if(j==2) FillHisto("MDMatchingProcedure/Pi3/MatchingResults",-2);
        }               
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        //////////////Final Matching Results using my standard Dmatch procedure  but with no Dmatch cut!(applying D<20 and Dmatch -- no restriction)
        if(nRecoCand == 0){
          if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",0); //no GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",0); //no GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",0); //no GTK candidates
          fDmatchMatchUNR_NNoMatch[j] +=1;
        }
        else if(D[BestIndex]>20){ //no good candidates to match to
          fDmatchMatchUNR_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",1); //no GOOD GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",1); //no GOOD GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",1); //no GOOD GTK candidates
        }
        else if(KBestMatchedIndexUNR[j] == -1){
          fDmatchMatchUNR_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",2); //no GOOD GTK candidates
          if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",2); //no GOOD GTK candidates
          if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",2); //no GOOD GTK candidates
        }
        else if(D[BestIndex]<20 &&  KBestMatchedIndexUNR[j] == BestIndex){ //match to best Candidate
          fDmatchMatchUNR_NMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",3); //matched to the best GTK candidate
          if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",3); //matched to the best GTK candidate
          if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",3); //matched to the best GTK candidate
        }
        else if(D[BestIndex]<20 &&  KBestMatchedIndexUNR[j] != BestIndex){ //dont match to the best candidate

          if(D[BestIndex]<5){ //must try to match only ti prime candidates

            Bool_t Matched = kFALSE;
            Bool_t Mismatched = kFALSE;					

            for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
              if(KBestMatchedIndexUNR[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                fDmatchMatchUNR_NMatch[j] +=1;
                if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",5); //matched to a PRime GTK Candidate
                if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",5); //matched to a PRime GTK Candidate
                Matched = kTRUE;
              }	
            }

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(KBestMatchedIndexUNR[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",6); //mismatched       
                if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",6); //mismatched
                if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",6); //mismatched
                fDmatchMatchUNR_NMismatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Matched == kFALSE && Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",7); //fail - no match
              if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",7); //fail - no match
              if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",7); //fail - no match
              fDmatchMatchUNR_NNoMatch[j] +=1;
            }


          }
          else if(D[BestIndex]>5 && D[BestIndex]<20){ //look only at sub-prime candidates

            Bool_t Mismatched = kFALSE;

            for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over subPrime GTK K Candidates
              if(KBestMatchedIndexUNR[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K subprime candidates
                if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",9); //matched       
                if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",9); //matched
                if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",9); //matched
                fDmatchMatchUNR_NMatch[j] +=1;
                Mismatched = kTRUE;
              }
            }

            if(Mismatched == kFALSE){ //matching procedure has failed to find a match or mismatch!
              if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",10); //fail - no match
              if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",10); //fail - no match
              if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",10); //fail - no match
              fDmatchMatchUNR_NNoMatch[j] +=1;
            }

          }
          else{ //error
            fDmatchMatchUNR_NNoMatch[j] +=1;
            if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",-1);
            if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",-1);
            if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",-1);
          }

        }
        else{
          fDmatchMatchUNR_NNoMatch[j] +=1;
          if(j==0) FillHisto("DmatchMatchingProcedureUNR/Pi1/MatchingResults",-2);
          if(j==1) FillHisto("DmatchMatchingProcedureUNR/Pi2/MatchingResults",-2);
          if(j==2) FillHisto("DmatchMatchingProcedureUNR/Pi3/MatchingResults",-2);
        }               
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////		

      } //end of loop over pion tracks

    } //end of matching evaluation for K3pi
    else if(fMatchingMode == 0){

      if(PositiveMatch_DmatchProcedure_Nmatches[0] ==0){  //no match made for Kpinunu
        fDmatchMatch_NNoMatch[0] +=1;
      }

      if(PositiveMatch_DmUNRProcedure_Nmatches[0] ==0){  //no match made for Kpinunu
        fDmatchMatchUNR_NNoMatch[0] +=1;
      }

      if(PositiveMatch_MDProcedure_Nmatches[0] ==0){	//no match made for Kpinunu
        fMDMatch_NNoMatch[0] +=1;		
      }
    }

    //initialize output vectors...
    fMatchedpiIndex.clear();
    fMatchedKIndex.clear();
    fMatchingQuality.clear();
    //MD matching
    fMatchedpiIndex_MD.clear();
    fMatchedKIndex_MD.clear();
    fMatchingQuality_MD.clear();
    //Dmatch Unrestricted matching
    fMatchedpiIndex_DmUNR.clear();
    fMatchedKIndex_DmUNR.clear();
    fMatchingQuality_DmUNR.clear();

    for(Int_t j = 0 ; j<NTracksToMatch ; j++){

      for(Int_t i = 0; i<nRecoCand ; i++){

        if(PositiveMatch_DmatchProcedure[i][j] == 1){ //made a good match here
          //std::cout<<"Match : pi_{"<<j<<"} --- K_{"<<i<<"} | Quality = "<<PositiveMatchQuality_DmatchProcedure[i][j];
          if(fMatchingMode == 1){ //K3pi
            if(D[BestIndex]<20 && i == KBestMatchedIndex[j] && i == BestIndex){
              PositiveMatch_K3pi_CorrectMatch[i][j] = 1;
              //std::cout<<" << correct match!";
            }
            else if(D[BestIndex]<20 && i == KBestMatchedIndex[j] && KBestMatchedIndex[j] != BestIndex){
              if(D[BestIndex]<5){ //only match to prime candidates can be good
                for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(KBestMatchedIndex[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch[i][j] = 1;
                    //std::cout<<" << correct match!";		
                  }
                }
              }
              else{ //5<D<20
                for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(KBestMatchedIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch[i][j] = 1;
                    //std::cout<<" << correct match!";
                  }
                }
              }
            }
          }
          //std::cout<<" ."<<std::endl;
          FillHisto("hDmatchCorrectMatch_K3piTest",PositiveMatch_K3pi_CorrectMatch[i][j]); //Fills with 1 if correct match made and 0 otherwise
          //fill output vectors...
          fMatchedpiIndex.push_back(j);
          fMatchedKIndex.push_back(i);
          fMatchingQuality.push_back(PositiveMatchQuality_DmatchProcedure[i][j]);
          //	fMatchingResults.push_back(j*1000 + i*100+PositiveMatchQuality_DmatchProcedure[i][j]);

        } //good match made (Dmatch standard procedure)^^^

        if(PositiveMatch_MDProcedure[i][j] == 1){ //made a good match here
          fMDMatchMade.push_back(true);
          //std::cout<<"Match MD : pi_{"<<j<<"} --- K_{"<<i<<"} | Quality = "<<PositiveMatchQuality_MDProcedure[i][j];
          if(fMatchingMode == 1){ //K3pi
            if(D[BestIndex]<20 && i == MDbestIndex[j] && i == BestIndex){
              PositiveMatch_K3pi_CorrectMatch_MD[i][j] = 1;
              //std::cout<<" << correct match!";
            }
            else if(D[BestIndex]<20 && i == MDbestIndex[j] && MDbestIndex[j] != BestIndex){
              if(D[BestIndex]<5){ //only match to prime candidates can be good
                for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(MDbestIndex[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch_MD[i][j] = 1;
                    //std::cout<<" << correct match!";		
                  }
                }
              }
              else{ //5<D<20
                for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(MDbestIndex[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch_MD[i][j] = 1;
                    //std::cout<<" << correct match!";
                  }
                }
              }
            }
          }
          //std::cout<<" ."<<std::endl;
          FillHisto("hMDCorrectMatch_K3piTest",PositiveMatch_K3pi_CorrectMatch_MD[i][j]); //Fills with 1 if correct match made and 0 otherwise	
          //fill output vectors...
          fMatchedpiIndex_MD.push_back(j);
          fMatchedKIndex_MD.push_back(i);
          fMatchingQuality_MD.push_back(PositiveMatchQuality_MDProcedure[i][j]);
        } // Good match made with MD procedure ^^^	
        else fMDMatchMade.push_back(false); //No good matches made with MD procedure

        if(PositiveMatch_DmUNRProcedure[i][j] == 1){ //made a good match here
          //std::cout<<"Match Dm_UNR : pi_{"<<j<<"} --- K_{"<<i<<"} | Quality = "<<PositiveMatchQuality_DmUNRProcedure[i][j];
          if(fMatchingMode == 1){ //K3pi
            if(D[BestIndex]<20 && i == KBestMatchedIndexUNR[j] && i == BestIndex){
              PositiveMatch_K3pi_CorrectMatch_DmUNR[i][j] = 1;
              //std::cout<<" << correct match!";
            }
            else if(D[BestIndex]<20 && i == KBestMatchedIndexUNR[j] && KBestMatchedIndexUNR[j] != BestIndex){
              if(D[BestIndex]<5){ //only match to prime candidates can be good
                for(Int_t k=0 ; k<NPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(KBestMatchedIndexUNR[j] == PotentialGTKKPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch_DmUNR[i][j] = 1;
                    //std::cout<<" << correct match!";		
                  }
                }
              }
              else{ //5<D<20
                for(Int_t k=0 ; k<NsubPrimeGTKCandidates ; k++){ //loop over Prime GTK K Candidates
                  if(KBestMatchedIndexUNR[j] == PotentialGTKKsubPrimeIndex[k]){ //matching to any of the possible GTK K prime candidates
                    PositiveMatch_K3pi_CorrectMatch_DmUNR[i][j] = 1;
                    //std::cout<<" << correct match!";
                  }
                }
              }
            }
            //PositiveMatch_K3pi_CorrectMatch_DmUNR --> Set output vector for fMatchingMode==1 only which is correct match or not
          }
          //std::cout<<" ."<<std::endl;
          FillHisto("hDmatchUNRCorrectMatch_K3piTest",PositiveMatch_K3pi_CorrectMatch_DmUNR[i][j]); //Fills with 1 if correct match made and 0 otherwise	
          //fill output vectors...
          fMatchedpiIndex_DmUNR.push_back(j);
          fMatchedKIndex_DmUNR.push_back(i);
          fMatchingQuality_DmUNR.push_back(PositiveMatchQuality_DmUNRProcedure[i][j]);
        } // Good Match made with Dmatch Unrestricted ^^^ 

      } //end of loop over GTK K candidates

    } //end of loop over pion candidates

    /*
       std::cout<<"MatchingResults = ";
       std::cout<<" piIndex : ";	
       for(auto k = fMatchedpiIndex.begin() ; k!=fMatchedpiIndex.end() ; k++){
       std::cout<<*k<<" , ";
       }
       std::cout<<"| KIndex : ";
       for(auto k = fMatchedKIndex.begin() ; k!=fMatchedKIndex.end() ; k++){
       std::cout<<*k<<" , ";
       }
       std::cout<<"| MatchQuality : ";
       for(auto k = fMatchingQuality.begin() ; k!=fMatchingQuality.end() ; k++){
       std::cout<<*k<<" , ";
       }
       std::cout<<" ."<<std::endl;
       */

    if(fMatchingMode == 0){
      //output K indices for each downstream track 
      //		std::cout<<"Best Matched kaon Index = "<<KBestMatchedIndex[0]<<std::endl;	
      //###########Using the MD matching procedure ONLY !!!! ##########
      fMDDiscriminant = fMatchingQuality_MD;
      SpectrometerGigaTrackerMatchingOutput Asso; 
      Asso.SetTrackID(fTrackID);//iEvent);
      for(Int_t i=0; i<nRecoCand ;i++){ //fill output vectors with ith element corresponding to the ith Reconstructed GTK candidate and the Ith DownstreamTrack
        //SpectrometerGigaTrackerMatchingOutput Asso(i);
        Asso.AddRecord(i,fMDMatchMade[i],fVertexPosition[i],fBeamParticleMomentum[i],fCorrectedBeamParticleMomentum[i],fTrackMomentum[i],fCorrectedTrackMomentum[i],fcda[i],fMDDiscriminant[i]);
        //AddRecord(GTKCandidateIndex,MatchMade,Vertex,BeamParticleMomentum,CorrectedBeamParticleMomentum,TrackMomentum,CorrectedTrackMomentum,CDA,Discriminant); 
      }

      //find best matching GTK candidate for a given DownstreamTrack object
      Double_t BestMDDiscriminant = 1000000.0;
      Int_t BestGTKCandIndex = -1;
      for(Int_t i=0; i<nRecoCand ;i++){
        if(i == 0){
          BestMDDiscriminant = fMDDiscriminant[i];
          BestGTKCandIndex = 0;
        }
        else{
          if(fMDDiscriminant[i]<BestMDDiscriminant){ //smaller value of MD Discriminant indicates a beter match
            BestMDDiscriminant = fMDDiscriminant[i];
            BestGTKCandIndex = i;
          }
        }
      }
      //set the outputs for best candidate...
      Asso.SetBestMatch(BestGTKCandIndex,BestMDDiscriminant);

      fContainer.push_back(Asso); //push_back output vector of SpectrometerGigaTrackerOutput. 	
    }

  } //end of loop over Spectormeter Tracks (move on to DownstreamTrack I+1)...

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::EndOfBurstUser(){

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::StartOfRunUser(){

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::EndOfRunUser(){

  Double_t GTKRecoEfficiency = 1 - ((Double_t)fNRecoNoCand/((Double_t)fNCandidateEvents)); 
  std::cout<<user_standard()<<"GTKRecoEfficiency="<<GTKRecoEfficiency<<std::endl;

  if(fMatchingMode == 1){
    //Print out matchng results
    std::cout<<user_standard()<<"**************************Matching Results**************************"<<std::endl;
    for(Int_t j=0; j<3 ; j++){ //loop over 3 pion condidates
      Double_t Pmm = (fDmatchMatch_NMismatch[j]/((Double_t)((fDmatchMatch_NMatch[j]+fDmatchMatch_NMismatch[j]))));
      Double_t n = fDmatchMatch_NMatch[j] + fDmatchMatch_NMismatch[j] + fDmatchMatch_NNoMatch[j];
      Double_t Po = fDmatchMatch_NNoMatch[j]/((Double_t)((fDmatchMatch_NMatch[j]+fDmatchMatch_NMismatch[j]+fDmatchMatch_NNoMatch[j])));
      Double_t N = fDmatchMatch_NMatch[j] + fDmatchMatch_NMismatch[j];
      //remove GTK inefficiency from the equation
      Double_t Poc = (fDmatchMatch_NNoMatch[j] -fNoCandidate)/((Double_t)((fDmatchMatch_NMatch[j]+fDmatchMatch_NMismatch[j]+fDmatchMatch_NNoMatch[j]))-fNoCandidate);//probability of none of the canidates being matched, given that there is at least 1 candidate 
      Double_t n_ = fDmatchMatch_NMatch[j] + fDmatchMatch_NMismatch[j] + fDmatchMatch_NNoMatch[j] - fNoCandidate; 
      std::cout<<user_standard()<<"Pmm_{pi"<<j<<"}="<<Pmm<<" +/- "<< (sqrt( (Pmm*(1-Pmm))/N  ) ) <<" | Pom_{pi"<<j<<"}="<<Po<<" +/- "<<(sqrt( (Po*(1-Po))/n  ) )<<" | Poc{pi"<<j<<"}="<<Poc<<" +/- "<< (sqrt( (Poc*(1-Poc))/n_  ) ) <<std::endl;
    }
    //////////////
    std::cout<<user_standard()<<"Linear Matching Discriminant MD"<<std::endl;
    for(Int_t j=0; j<3 ; j++){ //loop over 3 pion condidates
      Double_t Pmm = (fMDMatch_NMismatch[j]/((Double_t)((fMDMatch_NMatch[j]+fMDMatch_NMismatch[j]))));
      Double_t n = fMDMatch_NMatch[j] + fMDMatch_NMismatch[j] + fMDMatch_NNoMatch[j];
      Double_t Po = fMDMatch_NNoMatch[j]/((Double_t)((fMDMatch_NMatch[j]+fMDMatch_NMismatch[j]+fMDMatch_NNoMatch[j])));
      Double_t N = fMDMatch_NMatch[j] + fMDMatch_NMismatch[j];
      Double_t Poc = (fMDMatch_NNoMatch[j] -fNoCandidate)/((Double_t)((fMDMatch_NMatch[j]+fMDMatch_NMismatch[j]+fMDMatch_NNoMatch[j]))-fNoCandidate);//probability of none of the canidates being matched, given that there is at least 1 candidate 
      Double_t n_ = fMDMatch_NMatch[j] + fMDMatch_NMismatch[j] + fMDMatch_NNoMatch[j] - fNoCandidate; 
      std::cout<<user_standard()<<"Pmm_{pi"<<j<<"}="<<Pmm<<" +/- "<< (sqrt( (Pmm*(1-Pmm))/N  ) ) <<" | Pom_{pi"<<j<<"}="<<Po<<" +/- "<<(sqrt( (Po*(1-Po))/n  ) )<<" | Poc{pi"<<j<<"}="<<Poc<<" +/- "<< (sqrt( (Poc*(1-Poc))/n_  ) ) <<std::endl;
    }

    std::cout<<user_standard()<<"Unrestricted Dmatch"<<std::endl;
    for(Int_t j=0; j<3 ; j++){ //loop over 3 pion condidates
      Double_t Pmm = (fDmatchMatchUNR_NMismatch[j]/((Double_t)((fDmatchMatchUNR_NMatch[j]+fDmatchMatchUNR_NMismatch[j]))));
      Double_t n = fDmatchMatchUNR_NMatch[j] + fDmatchMatchUNR_NMismatch[j] + fDmatchMatchUNR_NNoMatch[j];
      Double_t Po = fDmatchMatchUNR_NNoMatch[j]/((Double_t)((fDmatchMatchUNR_NMatch[j]+fDmatchMatchUNR_NMismatch[j]+fDmatchMatchUNR_NNoMatch[j])));
      Double_t N = fDmatchMatchUNR_NMatch[j] + fDmatchMatchUNR_NMismatch[j];
      Double_t Poc = (fDmatchMatchUNR_NNoMatch[j] -fNoCandidate)/((Double_t)((fDmatchMatchUNR_NMatch[j]+fDmatchMatchUNR_NMismatch[j]+fDmatchMatchUNR_NNoMatch[j]))-fNoCandidate);//probability of none of the canidates being matched, given that there is at least 1 candidate 
      Double_t n_ = fDmatchMatchUNR_NMatch[j] + fDmatchMatchUNR_NMismatch[j] + fDmatchMatchUNR_NNoMatch[j] - fNoCandidate; 
      std::cout<<user_standard()<<"Pmm_{pi"<<j<<"}="<<Pmm<<" +/- "<< (sqrt( (Pmm*(1-Pmm))/N  ) ) <<" | Pom_{pi"<<j<<"}="<<Po<<" +/- "<<(sqrt( (Po*(1-Po))/n  ) )<<" | Poc{pi"<<j<<"}="<<Poc<<" +/- "<< (sqrt( (Poc*(1-Poc))/n_  ) ) <<std::endl;
    }
    //////////////

    //fill histos with matching results
    FillHisto("MatchingResults/DmatchMatching/pi1/P0",fDmatchMatch_NNoMatch[0]/((Double_t)((fDmatchMatch_NMatch[0]+fDmatchMatch_NMismatch[0]+fDmatchMatch_NNoMatch[0]))) );
    FillHisto("MatchingResults/DmatchMatching/pi1/Poc",(fDmatchMatch_NNoMatch[0]-fNoCandidate)/((Double_t)((fDmatchMatch_NMatch[0]+fDmatchMatch_NMismatch[0]+fDmatchMatch_NNoMatch[0] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatching/pi1/Pmm",(fDmatchMatch_NMismatch[0]/((Double_t)((fDmatchMatch_NMatch[0]+fDmatchMatch_NMismatch[0])))) );
    FillHisto("MatchingResults/DmatchMatching/pi2/P0",fDmatchMatch_NNoMatch[1]/((Double_t)((fDmatchMatch_NMatch[1]+fDmatchMatch_NMismatch[1]+fDmatchMatch_NNoMatch[1]))) );
    FillHisto("MatchingResults/DmatchMatching/pi2/Poc",(fDmatchMatch_NNoMatch[1]-fNoCandidate)/((Double_t)((fDmatchMatch_NMatch[1]+fDmatchMatch_NMismatch[1]+fDmatchMatch_NNoMatch[1] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatching/pi2/Pmm",(fDmatchMatch_NMismatch[1]/((Double_t)((fDmatchMatch_NMatch[1]+fDmatchMatch_NMismatch[1])))) );
    FillHisto("MatchingResults/DmatchMatching/pi3/P0",fDmatchMatch_NNoMatch[2]/((Double_t)((fDmatchMatch_NMatch[2]+fDmatchMatch_NMismatch[2]+fDmatchMatch_NNoMatch[2]))) );
    FillHisto("MatchingResults/DmatchMatching/pi3/Poc",(fDmatchMatch_NNoMatch[2]-fNoCandidate)/((Double_t)((fDmatchMatch_NMatch[2]+fDmatchMatch_NMismatch[2]+fDmatchMatch_NNoMatch[2] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatching/pi3/Pmm",(fDmatchMatch_NMismatch[2]/((Double_t)((fDmatchMatch_NMatch[2]+fDmatchMatch_NMismatch[2])))) );

    FillHisto("MatchingResults/MDMatching/pi1/P0",fMDMatch_NNoMatch[0]/((Double_t)((fMDMatch_NMatch[0]+fMDMatch_NMismatch[0]+fMDMatch_NNoMatch[0]))) );
    FillHisto("MatchingResults/MDMatching/pi1/Poc",(fMDMatch_NNoMatch[0]-fNoCandidate)/((Double_t)((fMDMatch_NMatch[0]+fMDMatch_NMismatch[0]+fMDMatch_NNoMatch[0] -fNoCandidate ))) );
    FillHisto("MatchingResults/MDMatching/pi1/Pmm",(fMDMatch_NMismatch[0]/((Double_t)((fMDMatch_NMatch[0]+fMDMatch_NMismatch[0])))) );
    FillHisto("MatchingResults/MDMatching/pi2/P0",fMDMatch_NNoMatch[1]/((Double_t)((fMDMatch_NMatch[1]+fMDMatch_NMismatch[1]+fMDMatch_NNoMatch[1]))) );
    FillHisto("MatchingResults/MDMatching/pi2/Poc",(fMDMatch_NNoMatch[1]-fNoCandidate)/((Double_t)((fMDMatch_NMatch[1]+fMDMatch_NMismatch[1]+fMDMatch_NNoMatch[1] -fNoCandidate ))) );
    FillHisto("MatchingResults/MDMatching/pi2/Pmm",(fMDMatch_NMismatch[1]/((Double_t)((fMDMatch_NMatch[1]+fMDMatch_NMismatch[1])))) );
    FillHisto("MatchingResults/MDMatching/pi3/P0",fMDMatch_NNoMatch[2]/((Double_t)((fMDMatch_NMatch[2]+fMDMatch_NMismatch[2]+fMDMatch_NNoMatch[2]))) );
    FillHisto("MatchingResults/MDMatching/pi3/Poc",(fMDMatch_NNoMatch[2]-fNoCandidate)/((Double_t)((fMDMatch_NMatch[2]+fMDMatch_NMismatch[2]+fMDMatch_NNoMatch[2] -fNoCandidate ))) );
    FillHisto("MatchingResults/MDMatching/pi3/Pmm",(fMDMatch_NMismatch[2]/((Double_t)((fMDMatch_NMatch[2]+fMDMatch_NMismatch[2])))) );

    FillHisto("MatchingResults/DmatchMatchingUNR/pi1/P0",fDmatchMatchUNR_NNoMatch[0]/((Double_t)((fDmatchMatchUNR_NMatch[0]+fDmatchMatchUNR_NMismatch[0]+fDmatchMatchUNR_NNoMatch[0]))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi1/Poc",(fDmatchMatchUNR_NNoMatch[0]-fNoCandidate)/((Double_t)((fDmatchMatchUNR_NMatch[0]+fDmatchMatchUNR_NMismatch[0]+fDmatchMatchUNR_NNoMatch[0] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi1/Pmm",(fDmatchMatchUNR_NMismatch[0]/((Double_t)((fDmatchMatchUNR_NMatch[0]+fDmatchMatchUNR_NMismatch[0])))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi2/P0",fDmatchMatchUNR_NNoMatch[1]/((Double_t)((fDmatchMatchUNR_NMatch[1]+fDmatchMatchUNR_NMismatch[1]+fDmatchMatchUNR_NNoMatch[1]))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi2/Poc",(fDmatchMatchUNR_NNoMatch[1]-fNoCandidate)/((Double_t)((fDmatchMatchUNR_NMatch[1]+fDmatchMatchUNR_NMismatch[1]+fDmatchMatchUNR_NNoMatch[1] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi2/Pmm",(fDmatchMatchUNR_NMismatch[1]/((Double_t)((fDmatchMatchUNR_NMatch[1]+fDmatchMatchUNR_NMismatch[1])))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi3/P0",fDmatchMatchUNR_NNoMatch[2]/((Double_t)((fDmatchMatchUNR_NMatch[2]+fDmatchMatchUNR_NMismatch[2]+fDmatchMatchUNR_NNoMatch[2]))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi3/Poc",(fDmatchMatchUNR_NNoMatch[2]-fNoCandidate)/((Double_t)((fDmatchMatchUNR_NMatch[2]+fDmatchMatchUNR_NMismatch[2]+fDmatchMatchUNR_NNoMatch[2] -fNoCandidate ))) );
    FillHisto("MatchingResults/DmatchMatchingUNR/pi3/Pmm",(fDmatchMatchUNR_NMismatch[2]/((Double_t)((fDmatchMatchUNR_NMatch[2]+fDmatchMatchUNR_NMismatch[2])))) );

  }
  else if( fMatchingMode ==0){

    std::cout<<user_standard()<<" GTK inefficiency ::: Events with No GTK candidates reconstructed = "<<fNRecoNoCand<<" ::: Events with >1 GTK Candidates reconstructed = "<<fNRecoMoreThanOne<<std::endl;

    std::cout<<user_standard()<<" ::::: Matching Results ::::: "<<std::endl;
    std::cout<<user_standard()<<"   N No matches (DmatchUNR) = "<<fDmatchMatchUNR_NNoMatch[0]<<std::endl;
    std::cout<<user_standard()<<"   N No matches (Dmatch) = "<<fDmatchMatch_NNoMatch[0]<<std::endl;
    std::cout<<user_standard()<<"   N No Matches (MD) = "<<fMDMatch_NNoMatch[0]<<std::endl;
    std::cout<<user_standard()<<"::::::::::::::::::::::::::::::"<<std::endl;
  }


  SaveAllPlots(); //save the plots to the root file.
}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::PostProcess(){

}
//--------------------------------------------------------------------------------------------------
void SpectrometerGigaTrackerMatching::DrawPlot(){

}
//--------------------------------------------------------------------------------------------------

/// PDF functions //////////////////////////////////////////////////////////////
//CDA Signal pdf-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::CDASignalPDF(Double_t CDA){
  //parameter values 25.1.17
  Double_t a = 0.87;
  Double_t b = 0.22;
  Double_t c = 0.91;
  Double_t alphaCB = -59.2;
  Double_t mean1 = -0.21;
  Double_t mean2 = 1.7;
  Double_t mean3 = -4.8;
  Double_t meanCB = 0.34;
  Double_t nCB = 10;
  Double_t sigma1 = 1.67;
  Double_t sigma2 = 2.5;
  Double_t sigma3 = 5.5;
  Double_t sigmaCB = 6.8;

  Double_t PDF = 0;

  if(CDA<-fabs(alphaCB)){
    PDF = c * ( b * ( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*( (CDA-mean2) /sigma2  )*((CDA-mean2) /sigma2)  ) )) + (1-a)*(( (pow((nCB/fabs(alphaCB)),nCB ))*TMath::Exp(-0.5*alphaCB*alphaCB) ) / ( pow( ( (nCB /fabs(alphaCB)) - fabs(alphaCB) - CDA  ) ,nCB )  )  )) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1))) ) ) + (1-c)*( (1/(sigma3*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((CDA-mean3)/sigma3)*((CDA-mean3)/sigma3))) ) ;
  }
  else{
    PDF = c * ( b * ( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*( (CDA-mean2) /sigma2  )*((CDA-mean2) /sigma2)  ) )) + (1-a)*( TMath::Exp(-0.5*( ((CDA-meanCB)/sigmaCB)*((CDA-meanCB)/sigmaCB) ))  )) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1))) ) ) + (1-c)*( (1/(sigma3*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((CDA-mean3)/sigma3)*((CDA-mean3)/sigma3))) ) ;
  }

  return PDF;
}

//--------------------------------------------------------------------------------------------
//Fitting function
Double_t fitf_cdaSig(Double_t *x, Double_t *par){
  Double_t PDF=0;
  // Double_t a = par[0];
  // Double_t b = par[1];
  // Double_t c = par[2];
  Double_t alphaCB = par[3];
  // Double_t mean1 = par[4];
  // Double_t mean2 = par[5];
  // Double_t mean3 = par[6];
  // Double_t nCB = par[7];
  // Double_t sigma1 = par[8];
  // Double_t sigma2 = par[9];
  // Double_t sigma3 = par[10];
  // Double_t sigmaCB = par[11];
  // Double_t meanCB = par[12];

  Double_t CDA = x[0];

  if(CDA<-fabs(alphaCB)){
    PDF = par[2] * ( par[1] * ( par[2]*( (1/(par[9]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*( (x[0]-par[5]) /par[9]  )*((x[0]-par[5]) /par[9])  ) )) + (1-par[0])*(( (pow((par[7]/fabs(par[3])),par[7] ))*TMath::Exp(-0.5*par[3]*par[3]) ) / ( pow( ( (par[7] /fabs(par[3])) - fabs(par[3]) - x[0]  ) ,par[7] )  )  )) + (1-par[1])*( (1/(par[8]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((x[0]-par[4])/par[8])*((x[0]-par[4])/par[8]))) ) ) + (1-par[2])*( (1/(par[10]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((x[0]-par[6])/par[10])*((x[0]-par[6])/par[10]))) ) ;
  }
  else if(CDA>-fabs(alphaCB)){
    PDF = par[2] * ( par[1] * ( par[0]*( (1/(par[9]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*( (x[0]-par[5]) /par[9]  )*((x[0]-par[5]) /par[9])  ) )) + (1-par[0])*( TMath::Exp(-0.5*( ((x[0]-par[12])/par[11])*((x[0]-par[12])/par[11]) ))  )) + (1-par[1])*( (1/(par[8]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((x[0]-par[4])/par[8])*((x[0]-par[4])/par[8]))) ) ) + (1-par[2])*( (1/(par[10]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((x[0]-par[6])/par[10])*((x[0]-par[6])/par[10]))) ) ;
  }

  return PDF;
}


Double_t NumericalIntegrationCDASig(){
  //TF1 f("CDASigPDF", fitf_cdaSig, 0, 80);
  TF1 FitFuncCDASig("FitFuncCDASig",fitf_cdaSig,1,0,13);
  //        25.1.17 values
  FitFuncCDASig.SetParameter(0,0.87);
  FitFuncCDASig.SetParameter(1,0.22);
  FitFuncCDASig.SetParameter(2,0.91);
  FitFuncCDASig.SetParameter(3,-59.2);
  FitFuncCDASig.SetParameter(4,-0.21);
  FitFuncCDASig.SetParameter(5,1.7);
  FitFuncCDASig.SetParameter(6,-4.8);
  FitFuncCDASig.SetParameter(7,0.34);
  FitFuncCDASig.SetParameter(8,10.0);
  FitFuncCDASig.SetParameter(9,1.67);
  FitFuncCDASig.SetParameter(10,2.5);
  FitFuncCDASig.SetParameter(11,5.5);
  FitFuncCDASig.SetParameter(12,6.8);

  ROOT::Math::WrappedTF1 wf1(FitFuncCDASig);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, 80) << endl;
  return ig.Integral(0, 80);
}
//----------------------------------------------------------------------------------------------------
Double_t NumericalIntegrationCDASig_VAL(Double_t cda){
  //TF1 f("CDASigPDF", fitf_cdaSig, 0, 80);
  TF1 FitFuncCDASig("FitFuncCDASig",fitf_cdaSig,1,0,13);
  //        25.1.17 values
  FitFuncCDASig.SetParameter(0,0.87);
  FitFuncCDASig.SetParameter(1,0.22);
  FitFuncCDASig.SetParameter(2,0.91);
  FitFuncCDASig.SetParameter(3,-59.2);
  FitFuncCDASig.SetParameter(4,-0.21);
  FitFuncCDASig.SetParameter(5,1.7);
  FitFuncCDASig.SetParameter(6,-4.8);
  FitFuncCDASig.SetParameter(7,0.34);
  FitFuncCDASig.SetParameter(8,10.0);
  FitFuncCDASig.SetParameter(9,1.67);
  FitFuncCDASig.SetParameter(10,2.5);
  FitFuncCDASig.SetParameter(11,5.5);
  FitFuncCDASig.SetParameter(12,6.8);
  ROOT::Math::WrappedTF1 wf1(FitFuncCDASig);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, cda) << endl;
  return ig.Integral(0, cda);
}


//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

//------------------CDA Background----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::CDABackgroundPDF(Double_t CDA){
  //25.1.17 update
  Double_t alphaCBb = -59.2;
  Double_t f1 = 0.94;
  Double_t f2 = 0.9998;
  Double_t mean1b = -0.75;
  Double_t mean2b = 17;
  Double_t meanCBb = 0.41;
  Double_t nCBb = 10;
  Double_t sigma1b = 5.8;
  Double_t sigma2b = 0.73;
  Double_t sigmaCBb = 3.07;

  Double_t PDF = 0; 

  if(CDA<-fabs(alphaCBb)){
    PDF = f2 * ( f1*( ((pow((nCBb/fabs(alphaCBb)),nCBb))*TMath::Exp(-0.5*alphaCBb*alphaCBb))/( pow( ((nCBb/fabs(alphaCBb))-fabs(alphaCBb) - CDA ) , nCBb) )  ) + (1-f1)*( (1/(sigma1b*sqrt(2*TMath::Pi()))  )*TMath::Exp(-0.5*((CDA - mean1b)/(sigma1b))*((CDA - mean1b)/(sigma1b)) ) )) + (1-f2)*( (1/(sigma2b*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((CDA - mean2b)/sigma2b)*((CDA - mean2b)/sigma2b) ) ); 
  }
  else if(CDA>-fabs(alphaCBb)){
    PDF = f2 * ( f1*( TMath::Exp(-0.5*((CDA-meanCBb)/sigmaCBb)*((CDA-meanCBb)/sigmaCBb))  ) + (1-f1)*( (1/(sigma1b*sqrt(2*TMath::Pi()))  )*TMath::Exp(-0.5*((CDA - mean1b)/(sigma1b))*((CDA - mean1b)/(sigma1b)) ) )) + (1-f2)*( (1/(sigma2b*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((CDA - mean2b)/sigma2b)*((CDA - mean2b)/sigma2b) ) ); 
  }


  return PDF;
}
//-------fitting function------------------------------------------
Double_t fitf_cdaBkg(Double_t *x, Double_t *par){
  Double_t PDF=0;
  Double_t alphaCBb = par[0];
  //Double_t f1 = par[1];
  //Double_t f2 = par[2];
  //Double_t mean1b = par[3];
  //Double_t mean2b = par[4];
  //Double_t meanCBb = par[5];
  //Double_t nCBb = par[6];
  //Double_t sigma1b = par[7];
  //Double_t sigma2b = par[8];
  //Double_t sigmaCBb = par[9];

  Double_t CDA = x[0];

  if(CDA<-fabs(alphaCBb)){
    PDF = par[2] * ( par[1]*( ((pow((par[6]/fabs(par[0])),par[6]))*TMath::Exp(-0.5*par[0]*par[0]))/( pow( ((par[6]/fabs(par[0]))-fabs(par[0]) - x[0] ) ,par[6]) )  ) + (1-par[1])*( (1/(par[7]*sqrt(2*TMath::Pi()))  )*TMath::Exp(-0.5*((x[0] - par[3])/(par[7]))*((x[0] - par[3])/(par[7])) ) )) + (1-par[2])*( (1/(par[8]*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((x[0] - par[4])/par[8])*((x[0] - par[4])/par[8]) ) );
  }
  else if(CDA>-fabs(alphaCBb)){
    PDF = par[2] * ( par[1]*( TMath::Exp(-0.5*((x[0]-par[5])/par[9])*((x[0]-par[5])/par[9]))  ) + (1-par[1])*( (1/(par[7]*sqrt(2*TMath::Pi()))  )*TMath::Exp(-0.5*((x[0] - par[3])/(par[7]))*((x[0] - par[3])/(par[7])) ) )) + (1-par[2])*( (1/(par[8]*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((x[0] - par[4])/par[8])*((x[0] - par[4])/par[8]) ) );
  }

  return PDF;
}


Double_t NumericalIntegrationCDABkg(){
  TF1 FitFuncCDABkg("FitFuncCDASig",fitf_cdaBkg,1,0,10);
  //       25.1.17 
  FitFuncCDABkg.SetParameter(0,-59.2);
  FitFuncCDABkg.SetParameter(1,0.94);
  FitFuncCDABkg.SetParameter(2,0.9998);
  FitFuncCDABkg.SetParameter(3,-0.75);
  FitFuncCDABkg.SetParameter(4,17);
  FitFuncCDABkg.SetParameter(5,0.41);
  FitFuncCDABkg.SetParameter(6,10);
  FitFuncCDABkg.SetParameter(7,5.8);
  FitFuncCDABkg.SetParameter(8,0.73);
  FitFuncCDABkg.SetParameter(9,3.07);

  ROOT::Math::WrappedTF1 wf2(FitFuncCDABkg);
  ROOT::Math::GaussIntegrator ig2;
  ig2.SetFunction(wf2);
  ig2.SetRelTolerance(0.001);
  //   cout << ig2.Integral(0, 80) << endl;
  return ig2.Integral(0, 80);
}

Double_t NumericalIntegrationCDABkg_VAL(Double_t cda){
  TF1 FitFuncCDABkg("FitFuncCDASig",fitf_cdaBkg,1,0,10);
  //        25.1.17
  FitFuncCDABkg.SetParameter(0,-59.2);
  FitFuncCDABkg.SetParameter(1,0.94);
  FitFuncCDABkg.SetParameter(2,0.9998);
  FitFuncCDABkg.SetParameter(3,-0.75);
  FitFuncCDABkg.SetParameter(4,17);
  FitFuncCDABkg.SetParameter(5,0.41);
  FitFuncCDABkg.SetParameter(6,10);
  FitFuncCDABkg.SetParameter(7,5.8);
  FitFuncCDABkg.SetParameter(8,0.73);
  FitFuncCDABkg.SetParameter(9,3.07);
  ROOT::Math::WrappedTF1 wf2(FitFuncCDABkg);
  ROOT::Math::GaussIntegrator ig2;
  ig2.SetFunction(wf2);
  ig2.SetRelTolerance(0.001);
  //   cout << ig2.Integral(0, cda) << endl;
  return ig2.Integral(0, cda);
}

//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

//-----------dt Signal-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::dtSignalPDF(Double_t dt){
  //26.1.17
  Double_t a0 = -0.024;
  Double_t a1 = -1.2873;
  Double_t a2 = 0.048;
  Double_t a3 = 0.2690;
  Double_t a4 = -0.030;
  Double_t a = 0.91;
  Double_t b = 0.48;
  Double_t mean1 = 0.045;
  Double_t mean2 = 0.040;
  Double_t sigma1 = 0.17;
  Double_t sigma2 = 0.12;

  Double_t PDF = b*( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((dt-mean2)/sigma2)*((dt-mean2)/sigma2))  ) + (1-a)*(1 + a0 + a1*dt + a2*(2*dt*dt-1) + a3*(4*dt*dt*dt-3*dt) + a4*(8*dt*dt*dt*dt-8*dt*dt+1) )  ) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi()))*TMath::Exp(-0.5*((dt-mean1)/sigma1)*((dt-mean1)/sigma1))) );


  return PDF;
}
//-----------------fitting function ----------------------------------
Double_t fitf_dtSig(Double_t *x, Double_t *par){
  Double_t PDF=0;
  //Double_t p0 = par[0];
  //Double_t p1 = par[1];
  //Double_t p2 = par[2];
  //Double_t p3 = par[3];
  //Double_t p4 = par[4];
  //Double_t a = par[5];
  //Double_t b = par[6];
  //Double_t mean1 = par[7];
  //Double_t mean2 = par[8];
  //Double_t sigma1 = par[9];
  //Double_t sigma2 = par[10];

  //Double_t dt = x[0];

  PDF = par[6]*( par[5]*( (1/(par[10]*sqrt(2*TMath::Pi())))*TMath::Exp(-0.5*((x[0]-par[8])/par[10])*((x[0]-par[8])/par[10]))  ) + (1-par[5])*(1 + par[0] + par[1]*x[0] + par[2]*(2*x[0]*x[0]-1) + par[3]*(4*x[0]*x[0]*x[0]-3*x[0]) + par[4]*(8*x[0]*x[0]*x[0]*x[0]-8*x[0]*x[0]+1) )  ) + (1-par[6])*( (1/(par[9]*sqrt(2*TMath::Pi()))*TMath::Exp(-0.5*((x[0]-par[7])/par[9])*((x[0]-par[7])/par[9]))) );


  return PDF;
}
//----------------------------------------------------------------------------
Double_t NumericalIntegrationdtSig(){
  TF1 FitFuncdtSig("FitFuncdtSig",fitf_dtSig,1,0,11);
  FitFuncdtSig.SetParameter(0,-0.024);
  FitFuncdtSig.SetParameter(1,-1.2873);
  FitFuncdtSig.SetParameter(2,0.048);
  FitFuncdtSig.SetParameter(3,0.2690);
  FitFuncdtSig.SetParameter(4,-0.030);
  FitFuncdtSig.SetParameter(5,0.91);
  FitFuncdtSig.SetParameter(6,0.48);
  FitFuncdtSig.SetParameter(7,0.045);
  FitFuncdtSig.SetParameter(8,0.040);
  FitFuncdtSig.SetParameter(9,0.17);
  FitFuncdtSig.SetParameter(10,0.12);

  ROOT::Math::WrappedTF1 wf3(FitFuncdtSig);
  ROOT::Math::GaussIntegrator ig3;
  ig3.SetFunction(wf3);
  ig3.SetRelTolerance(0.001);
  //   cout << ig3.Integral(-1, 1) << endl;
  return ig3.Integral(-1, 1);
}

Double_t NumericalIntegrationdtSig_VAL(Double_t dt){
  TF1 FitFuncdtSig("FitFuncdtSig",fitf_dtSig,1,0,11);
  FitFuncdtSig.SetParameter(0,-0.024);
  FitFuncdtSig.SetParameter(1,-1.2873);
  FitFuncdtSig.SetParameter(2,0.048);
  FitFuncdtSig.SetParameter(3,0.2690);
  FitFuncdtSig.SetParameter(4,-0.030);
  FitFuncdtSig.SetParameter(5,0.91);
  FitFuncdtSig.SetParameter(6,0.48);
  FitFuncdtSig.SetParameter(7,0.045);
  FitFuncdtSig.SetParameter(8,0.040);
  FitFuncdtSig.SetParameter(9,0.17);
  FitFuncdtSig.SetParameter(10,0.12);

  ROOT::Math::WrappedTF1 wf3(FitFuncdtSig);
  ROOT::Math::GaussIntegrator ig3;
  ig3.SetFunction(wf3);
  ig3.SetRelTolerance(0.001);
  //   cout << ig3.Integral(-dt, dt) << endl;
  return ig3.Integral(-dt, dt);
}
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

//------------dt background pdf----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::dtBackgroundPDF(Double_t dt){
  //dt background pdf is a 4th order Chebyschev polynomial
  //26.1.17
  Double_t a0 = -0.00065;
  Double_t a1 = -0.786;
  Double_t a2 = -0.018;
  Double_t a3 = -0.272;
  Double_t a4 = -0.0053;

  //	Double_t PDF =(1./2.51199)*(1 + a0 + a1*dt + a2*(2*dt*dt-1) + a3*(4*dt*dt*dt - 3*dt) + a4*(8*dt*dt*dt*dt - 8*dt*dt +1));	
  Double_t PDF = (1 + a0 + a1*dt + a2*(2*dt*dt-1) + a3*(4*dt*dt*dt - 3*dt) + a4*(8*dt*dt*dt*dt - 8*dt*dt +1));

  return PDF;
}
//--------------------------fitting functionm----------------------------------------
Double_t fitf_dtBkg(Double_t *x, Double_t *par){
  Double_t PDF=0;
  Double_t a0 = par[0];
  Double_t a1 = par[1];
  Double_t a2 = par[2];
  Double_t a3 = par[3];
  Double_t a4 = par[4];

  Double_t dt = x[0];


  PDF = 1 + a0 + a1*dt + a2*(2*dt*dt-1) + a3*(4*dt*dt*dt - 3*dt) + a4*(8*dt*dt*dt*dt - 8*dt*dt +1);

  return PDF;
}
//-------------------------------------------------------------------------------------
Double_t NumericalIntegrationdtBkg(){
  TF1 FitFuncdtBkg("FitFuncdtBkg",fitf_dtBkg,1,0,5);
  FitFuncdtBkg.SetParameter(0,-0.00065);
  FitFuncdtBkg.SetParameter(1,-0.786);
  FitFuncdtBkg.SetParameter(2,-0.018);
  FitFuncdtBkg.SetParameter(3,-0.272);
  FitFuncdtBkg.SetParameter(4,-0.0053);

  ROOT::Math::WrappedTF1 wf4(FitFuncdtBkg);
  ROOT::Math::GaussIntegrator ig4;
  ig4.SetFunction(wf4);
  ig4.SetRelTolerance(0.001);
  //   cout << ig4.Integral(-1, 1) << endl;
  return ig4.Integral(-1, 1);
}
//-------------------------------------------------------------------------------------
Double_t NumericalIntegrationdtBkg_VAL(Double_t dt){
  TF1 FitFuncdtBkg("FitFuncdtBkg",fitf_dtBkg,1,0,5);
  FitFuncdtBkg.SetParameter(0,-0.00065);
  FitFuncdtBkg.SetParameter(1,-0.786);
  FitFuncdtBkg.SetParameter(2,-0.018);
  FitFuncdtBkg.SetParameter(3,-0.272);
  FitFuncdtBkg.SetParameter(4,-0.0053);

  ROOT::Math::WrappedTF1 wf4(FitFuncdtBkg);
  ROOT::Math::GaussIntegrator ig4;
  ig4.SetFunction(wf4);
  ig4.SetRelTolerance(0.001);
  //   cout << ig4.Integral(-dt, dt) << endl;
  return ig4.Integral(-dt, dt);
}
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

///JCS : PDF functions New 4/2/17 ::://////////////////////////////////////////////////////////////
//CDA Signal pdf-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//PDF 
Double_t SpectrometerGigaTrackerMatching::CDASignalPDF_new(Double_t CDA){

  //New FIT PDF function 4.2.17
  //parameter values 4.2.17
  Double_t a = 0.17;
  Double_t b = 0.32;
  Double_t alphaCB = -59.2;
  Double_t mean1 = -0.23;
  Double_t mean2 = -2.95;
  Double_t meanCB = 0.27;
  Double_t nCB = 10;
  Double_t sigma1 = 1.69;
  Double_t sigma2 = 6.99;
  Double_t sigmaCB = 3.02;

  Double_t PDF = 0;

  if(CDA<-fabs(alphaCB)){
    PDF = b*( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean2)/sigma2)*((CDA-mean2)/sigma2) )))  + (1-a)*( ((pow((nCB/fabs(alphaCB)),nCB) )*(TMath::Exp(-0.5*((CDA-meanCB)/sigmaCB)*((CDA-meanCB)/sigmaCB))))/( pow( ( (nCB/fabs(alphaCB) ) - fabs(alphaCB) - ((CDA-meanCB)/sigmaCB) ) ,nCB)  )  ) ) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1) ))  ); 
  }
  else if(CDA>-fabs(alphaCB)){
    PDF = b*( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean2)/sigma2)*((CDA-mean2)/sigma2) )))  + (1-a)*( TMath::Exp(-0.5*((CDA-meanCB)/sigmaCB)*((CDA-meanCB)/sigmaCB))) ) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1) ))  );
  }



  return PDF;
}

//--------------------------------------------------------------------------------------------
//Fitting function
Double_t fitf_cdaSig_new(Double_t *x, Double_t *par){
  Double_t PDF=0;
  //Double_t a = par[0];
  //Double_t b = par[1];
  //Double_t alphaCB = par[2];
  //Double_t mean1 = par[3];
  //Double_t mean2 = par[4];
  //Double_t nCB = par[5];
  //Double_t sigma1 = par[6];
  //Double_t sigma2 = par[7];
  //Double_t sigmaCB = par[8];
  //Double_t meanCB = par[9];

  //Double_t CDA = x[0];

  if(x[0]<-fabs(par[2])){
    PDF = par[1]*( par[0]*( (1/(par[7]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[4])/par[7])*((x[0]-par[4])/par[7]) )))  + (1-par[0])*( ((pow((par[5]/fabs(par[2])),par[5]) )*(TMath::Exp(-0.5*((x[0]-par[9])/par[8])*((x[0]-par[9])/par[8]))))/( pow( ( (par[5]/fabs(par[2]) ) - fabs(par[2]) - ((x[0]-par[9])/par[8]) ) ,par[5])  )  ) ) + (1-par[1])*( (1/(par[6]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[3])/par[6])*((x[0]-par[3])/par[6]) ))  );
  }
  else if(x[0]>-fabs(par[2])){
    PDF = par[1]*( par[0]*( (1/(par[7]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[4])/par[7])*((x[0]-par[4])/par[7]) )))  + (1-par[0])*( TMath::Exp(-0.5*((x[0]-par[9])/par[8])*((x[0]-par[9])/par[8]))) ) + (1-par[1])*( (1/(par[6]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[3])/par[6])*((x[0]-par[3])/par[6]) ))  );
  }

  return PDF;
}


Double_t NumericalIntegrationCDASig_new(){
  TF1 FitFuncCDASig("FitFuncCDASig",fitf_cdaSig_new,1,0,10);
  //        4.2.17 values
  FitFuncCDASig.SetParameter(0,0.18);
  FitFuncCDASig.SetParameter(1,0.32);
  FitFuncCDASig.SetParameter(2,-59.2);
  FitFuncCDASig.SetParameter(3,-0.23);
  FitFuncCDASig.SetParameter(4,-2.95);
  FitFuncCDASig.SetParameter(5,0.27);
  FitFuncCDASig.SetParameter(6,10.0);
  FitFuncCDASig.SetParameter(7,1.69);
  FitFuncCDASig.SetParameter(8,6.99);
  FitFuncCDASig.SetParameter(9,3.02);

  ROOT::Math::WrappedTF1 wf1(FitFuncCDASig);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, 80) << endl;
  return ig.Integral(0, 80);
}
//----------------------------------------------------------------------------------------------------
Double_t NumericalIntegrationCDASig_VAL_new(Double_t cda){
  TF1 FitFuncCDASig("FitFuncCDASig",fitf_cdaSig_new,1,0,10);
  //        4.2.17 values
  FitFuncCDASig.SetParameter(0,0.18);
  FitFuncCDASig.SetParameter(1,0.32);
  FitFuncCDASig.SetParameter(2,-59.2);
  FitFuncCDASig.SetParameter(3,-0.23);
  FitFuncCDASig.SetParameter(4,-2.95);
  FitFuncCDASig.SetParameter(5,0.27);
  FitFuncCDASig.SetParameter(6,10.0);
  FitFuncCDASig.SetParameter(7,1.69);
  FitFuncCDASig.SetParameter(8,6.99);
  FitFuncCDASig.SetParameter(9,3.02);

  ROOT::Math::WrappedTF1 wf1(FitFuncCDASig);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, cda) << endl;
  return ig.Integral(0, cda);
}


//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

//CDA Background pdf-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//PDF 
Double_t SpectrometerGigaTrackerMatching::CDABackgroundPDF_new(Double_t CDA){

  //New FIT PDF function 4.2.17
  //parameter values 4.2.17
  Double_t a = 0.90;
  Double_t b = 0.67;
  Double_t alphaCB = -59.2;
  Double_t mean1 = -0.54;
  Double_t mean2 = -2.72;
  Double_t meanCB = 1.87;
  Double_t nCB = 10;
  Double_t sigma1 = 5.95;
  Double_t sigma2 = 3.50;
  Double_t sigmaCB = 2.60;

  Double_t PDF = 0;


  if(CDA<-fabs(alphaCB)){
    PDF = b*( a*(  ((pow((nCB/fabs(alphaCB)),nCB) )*(TMath::Exp(-0.5*((CDA-meanCB)/sigmaCB)*((CDA-meanCB)/sigmaCB))))/( pow( ( (nCB/fabs(alphaCB) ) - fabs(alphaCB) - ((CDA-meanCB)/sigmaCB) ) ,nCB)  )   )  + (1-a)*(  (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean2)/sigma2)*((CDA-mean2)/sigma2) ))  ) ) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1) ))  ); 
  }
  else if(CDA>-fabs(alphaCB)){

    PDF = b*( a*( TMath::Exp(-0.5*((CDA-meanCB)/sigmaCB)*((CDA-meanCB)/sigmaCB))) )  + (1-a)*((1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean2)/sigma2)*((CDA-mean2)/sigma2) )) ) + (1-b)*( (1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((CDA-mean1)/sigma1)*((CDA-mean1)/sigma1) ))  );
  }



  return PDF;
}

//--------------------------------------------------------------------------------------------
//Fitting function
Double_t fitf_cdaBkg_new(Double_t *x, Double_t *par){
  Double_t PDF=0;
  //Double_t a = par[0];
  //Double_t b = par[1];
  //Double_t alphaCB = par[2];
  //Double_t mean1 = par[3];
  //Double_t mean2 = par[4];
  //Double_t nCB = par[5];
  //Double_t sigma1 = par[6];
  //Double_t sigma2 = par[7];
  //Double_t sigmaCB = par[8];
  //Double_t meanCB = par[9];

  //Double_t CDA = x[0];

  if(x[0]<-fabs(par[2])){

    PDF = par[1]*( par[0]*(  ((pow((par[5]/fabs(par[2])),par[5]) )*(TMath::Exp(-0.5*((x[0]-par[9])/par[8])*((x[0]-par[9])/par[8]))))/( pow( ( (par[5]/fabs(par[2]) ) - fabs(par[2]) - ((x[0]-par[9])/par[8]) ) ,par[5])  )  )  + (1-par[0])*( (1/(par[7]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[4])/par[7])*((x[0]-par[4])/par[7]) ))) ) + (1-par[1])*( (1/(par[6]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[3])/par[6])*((x[0]-par[3])/par[6]) ))  );
  }
  else if(x[0]>-fabs(par[2])){

    PDF = par[1]*( par[0]*(   TMath::Exp(-0.5*((x[0]-par[9])/par[8])*((x[0]-par[9])/par[8])))  + (1-par[0])*((1/(par[7]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[4])/par[7])*((x[0]-par[4])/par[7]) )) ) ) + (1-par[1])*( (1/(par[6]*sqrt(2*TMath::Pi())))*(TMath::Exp( -0.5*((x[0]-par[3])/par[6])*((x[0]-par[3])/par[6]) ))  );
  }

  return PDF;
}


Double_t NumericalIntegrationCDABkg_new(){
  TF1 FitFuncCDABkg("FitFuncCDABkg",fitf_cdaBkg_new,1,0,10);
  //        4.2.17 values
  FitFuncCDABkg.SetParameter(0,0.90);
  FitFuncCDABkg.SetParameter(1,0.67);
  FitFuncCDABkg.SetParameter(2,-59.2);
  FitFuncCDABkg.SetParameter(3,-0.54);
  FitFuncCDABkg.SetParameter(4,-2.72);
  FitFuncCDABkg.SetParameter(5,1.87);
  FitFuncCDABkg.SetParameter(6,10.0);
  FitFuncCDABkg.SetParameter(7,5.95);
  FitFuncCDABkg.SetParameter(8,3.50);
  FitFuncCDABkg.SetParameter(9,2.60);

  ROOT::Math::WrappedTF1 wf1(FitFuncCDABkg);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, 80) << endl;
  return ig.Integral(0, 80);
}
//----------------------------------------------------------------------------------------------------
Double_t NumericalIntegrationCDABkg_VAL_new(Double_t cda){
  TF1 FitFuncCDABkg("FitFuncCDABkg",fitf_cdaBkg_new,1,0,10);
  //       4.2.17 values
  FitFuncCDABkg.SetParameter(0,0.90);
  FitFuncCDABkg.SetParameter(1,0.67);
  FitFuncCDABkg.SetParameter(2,-59.2);
  FitFuncCDABkg.SetParameter(3,-0.54);
  FitFuncCDABkg.SetParameter(4,-2.72);
  FitFuncCDABkg.SetParameter(5,1.87);
  FitFuncCDABkg.SetParameter(6,10.0);
  FitFuncCDABkg.SetParameter(7,5.95);
  FitFuncCDABkg.SetParameter(8,3.50);
  FitFuncCDABkg.SetParameter(9,2.60);

  ROOT::Math::WrappedTF1 wf1(FitFuncCDABkg);
  ROOT::Math::GaussIntegrator ig;
  ig.SetFunction(wf1);
  ig.SetRelTolerance(0.001);
  //   cout << ig.Integral(0, cda) << endl;
  return ig.Integral(0, cda);
}


//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//-----------dt Signal-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::dtSignalPDF_new(Double_t dt){
  //4.2.17
  Double_t a = 0.33;
  Double_t b = 0.94;
  Double_t mean1 = 0.047; //meanBDT
  Double_t mean2 = 0.040; //meanBDT2
  Double_t mean3 = 0.011; //meanDT
  Double_t sigma1 = 0.16;
  Double_t sigma2 = 0.11;
  Double_t sigma3 = 0.36;

  Double_t PDF = b*( a*( (1/(sigma2*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-mean2)/sigma2)*((dt-mean2)/sigma2) ) ) ) + (1-a*((1/(sigma1*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-mean1)/sigma1)*((dt-mean1)/sigma1) ) )) ) ) + (1-b)*((1/(sigma3*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-mean3)/sigma3)*((dt-mean3)/sigma3) ) ));	

  return PDF;
}
//-----------------fitting function ----------------------------------
Double_t fitf_dtSig_new(Double_t *x, Double_t *par){
  Double_t PDF=0;
  //Double_t a = par[0];
  //Double_t b = par[1];
  //Double_t mean1 = par[2];
  //Double_t mean2 = par[3];
  //Double_t mean3 = par[4];
  //Double_t sigma1 = par[5];
  //Double_t sigma2 = par[6];
  //Double_t sigma3 = par[7];

  Double_t dt = x[0];

  PDF = par[1]*( par[0]*( (1/(par[6]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-par[3])/par[6])*((dt-par[3])/par[6]) ) ) ) + (1-par[0]*((1/(par[5]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-par[2])/par[5])*((dt-par[2])/par[5]) ) )) ) ) + (1-par[1])*((1/(par[7]*sqrt(2*TMath::Pi())))*(TMath::Exp(-0.5*((dt-par[4])/par[7])*((dt-par[4])/par[7]) ) ));


  return PDF;
}
//----------------------------------------------------------------------------
Double_t NumericalIntegrationdtSig_new(){
  TF1 FitFuncdtSig("FitFuncdtSig",fitf_dtSig_new,1,0,8);
  //4.2.17
  FitFuncdtSig.SetParameter(0,0.33);
  FitFuncdtSig.SetParameter(1,0.94);
  FitFuncdtSig.SetParameter(2,0.047);
  FitFuncdtSig.SetParameter(3,0.040);
  FitFuncdtSig.SetParameter(4,0.011);
  FitFuncdtSig.SetParameter(5,0.16);
  FitFuncdtSig.SetParameter(6,0.11);
  FitFuncdtSig.SetParameter(7,0.36);

  ROOT::Math::WrappedTF1 wf3(FitFuncdtSig);
  ROOT::Math::GaussIntegrator ig3;
  ig3.SetFunction(wf3);
  ig3.SetRelTolerance(0.001);
  //   cout << ig3.Integral(-1, 1) << endl;
  return ig3.Integral(-0.6, 0.6);
}

Double_t NumericalIntegrationdtSig_VAL_new(Double_t dt){
  TF1 FitFuncdtSig("FitFuncdtSig",fitf_dtSig_new,1,0,8);
  //4.2.17
  FitFuncdtSig.SetParameter(0,0.33);
  FitFuncdtSig.SetParameter(1,0.94);
  FitFuncdtSig.SetParameter(2,0.047);
  FitFuncdtSig.SetParameter(3,0.040);
  FitFuncdtSig.SetParameter(4,0.011);
  FitFuncdtSig.SetParameter(5,0.16);
  FitFuncdtSig.SetParameter(6,0.11);
  FitFuncdtSig.SetParameter(7,0.36);

  ROOT::Math::WrappedTF1 wf3(FitFuncdtSig);
  ROOT::Math::GaussIntegrator ig3;
  ig3.SetFunction(wf3);
  ig3.SetRelTolerance(0.001);
  //   cout << ig3.Integral(-dt, dt) << endl;
  return ig3.Integral(-dt, dt);
}
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//------------dt background pdf----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Double_t SpectrometerGigaTrackerMatching::dtBackgroundPDF_new(Double_t dt){
  //dt background pdf is a 2nd order Chebyschev polynomial
  //4.2.17
  Double_t a0 = 0.00884;
  Double_t a1 = -0.501;

  Double_t PDF = 1 + a0*dt  + a1*(2*dt*dt-1);

  return PDF;
}
//--------------------------fitting functionm----------------------------------------
Double_t fitf_dtBkg_new(Double_t *x, Double_t *par){
  Double_t PDF=0;
  //Double_t a0 = par[0];
  //Double_t a1 = par[1];
  //Double_t dt = x[0];

  PDF = 1 + par[0]*x[0]  + par[1]*(2*x[0]*x[0]-1);

  return PDF;
}
//-------------------------------------------------------------------------------------
Double_t NumericalIntegrationdtBkg_new(){
  TF1 FitFuncdtBkg("FitFuncdtBkg",fitf_dtBkg_new,1,0,2);
  FitFuncdtBkg.SetParameter(0,0.00884);
  FitFuncdtBkg.SetParameter(1,-0.501);

  ROOT::Math::WrappedTF1 wf4(FitFuncdtBkg);
  ROOT::Math::GaussIntegrator ig4;
  ig4.SetFunction(wf4);
  ig4.SetRelTolerance(0.001);
  //   cout << ig4.Integral(-1, 1) << endl;
  return ig4.Integral(-0.6, 0.6);
}
//-------------------------------------------------------------------------------------
Double_t NumericalIntegrationdtBkg_VAL_new(Double_t dt){
  TF1 FitFuncdtBkg("FitFuncdtBkg",fitf_dtBkg_new,1,0,2);
  FitFuncdtBkg.SetParameter(0,0.00884);
  FitFuncdtBkg.SetParameter(1,-0.501);

  ROOT::Math::WrappedTF1 wf4(FitFuncdtBkg);
  ROOT::Math::GaussIntegrator ig4;
  ig4.SetFunction(wf4);
  ig4.SetRelTolerance(0.001);
  //   cout << ig4.Integral(-dt, dt) << endl;
  return ig4.Integral(-dt, dt);
}
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

void SpectrometerGigaTrackerMatching::SetK3piEventVariables(){
  // Set variables for the selected K3pi event 
  // (these used to be taken as input from K3piStrictSelection before it was replaced by a new version)

  //fIsK3pi = *(Bool_t*)GetOutput("K3piStrictSelection.EventSelected");     //Get valid K3pi events
  fVertexIndex = *(Int_t*)GetOutput("K3piStrictSelection.VertexIndex"); 
  fNegativePionIndex =  *(Int_t*)GetOutput("K3piStrictSelection.NegPion_ID");
  fPositivePionIndex1 = *(Int_t*)GetOutput("K3piStrictSelection.PosPionHighMomt_ID"); 
  fPositivePionIndex2 = *(Int_t*)GetOutput("K3piStrictSelection.PosPionLowMomt_ID");
  fNegativePionCHODsTime = *(Double_t*)GetOutput("K3piStrictSelection.NegPion_Time"); // CHOD time if it exists, NewCHOD otherwise, but if neither exists then take Spectrometer time 
  fPositivePion1CHODsTime = *(Double_t*)GetOutput("K3piStrictSelection.PosPionHighMomt_Time");	
  fPositivePion2CHODsTime = *(Double_t*)GetOutput("K3piStrictSelection.PosPionLowMomt_Time");

  // --- Set Cedar/KTAG time ---
  fK3piTime = ((fNegativePionCHODsTime+fPositivePion1CHODsTime+fPositivePion2CHODsTime)/3.0); 
  Double_t CedarCand_Best_Time = 0;
  Double_t BestTimeDiff=100000000.0;
  TRecoCedarEvent* CedarEvent  = GetEvent<TRecoCedarEvent>();
  Int_t NCedarCand = CedarEvent->GetNCandidates();
  for(Int_t i=0; i<NCedarCand; i++){
    TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(i)); //loping over all cedar candidates including bad ones

    if(Ccand->GetNSectors()<5) continue; //look only at good cedar candidates

    Double_t CedarCandTime = Ccand->GetTime();
    Double_t deltaT_CedarCandtime_K3piTime=CedarCandTime-fK3piTime;

    if(i==0){
      BestTimeDiff=deltaT_CedarCandtime_K3piTime;
      CedarCand_Best_Time=CedarCandTime;
    }

    if( abs(deltaT_CedarCandtime_K3piTime) < abs(BestTimeDiff)){    //if absolute time difference is smallar for the current candidate...
      BestTimeDiff=deltaT_CedarCandtime_K3piTime;
      CedarCand_Best_Time=CedarCandTime;

    }

  }

  fKTAGTime = CedarCand_Best_Time;	

  // Set Discriminant variables 
  std::vector<SpectrometerTrackVertex> Vertices =*(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
  TVector3 K3pi3Mom =  Vertices[fVertexIndex].GetTrackThreeMomentum(0) + Vertices[fVertexIndex].GetTrackThreeMomentum(1) + Vertices[fVertexIndex].GetTrackThreeMomentum(2); // Momentum (using BlueTube corrections for B field) [MeV/c]
  fp_K3pi = K3pi3Mom.Mag()*0.001; // [GeV/c] magnitude
  fdxdz_K3pi = K3pi3Mom.X()/K3pi3Mom.Z();
  fdydz_K3pi = K3pi3Mom.Y()/K3pi3Mom.Z();	

  //CORRECT for blue tube B field UPSTREAM of vertex####################################################################################################################################
  BlueTubeTracker::GetInstance()->SetCharge(+1);
  BlueTubeTracker::GetInstance()->SetInitialPosition(Vertices[fVertexIndex].GetPosition().x() , Vertices[fVertexIndex].GetPosition().y() , Vertices[fVertexIndex].GetPosition().z() );  // Unit: mm (x, y, z)
  BlueTubeTracker::GetInstance()->SetInitialMomentum(K3pi3Mom.X(), K3pi3Mom.Y(), K3pi3Mom.Z());  // Unit: MeV/c //px py pz corrected for downstream blue tube and for TRIM5 rotation of beam
  BlueTubeTracker::GetInstance()->SetZFinal(102400.);                         // Unit: mm  //GTK3 plane
  BlueTubeTracker::GetInstance()->TrackParticle();
  TVector3 NewPosition = BlueTubeTracker::GetInstance()->GetFinalPosition(); // Unit: mm
  TVector3 NewMomentum = BlueTubeTracker::GetInstance()->GetFinalMomentum(); // Unit: MeV/c

  fXGTK3_K3pi = NewPosition.X();       //position at GTK3 plane from BlueTubeTracker extrapolation
  fYGTK3_K3pi = NewPosition.Y(); 

  fp_K3piX = K3pi3Mom.X()*0.001; // in GeV/c
  fp_K3piY = K3pi3Mom.Y()*0.001; // in GeV/c
  fp_K3piZ = K3pi3Mom.Z()*0.001; // in GeV/c	

}

