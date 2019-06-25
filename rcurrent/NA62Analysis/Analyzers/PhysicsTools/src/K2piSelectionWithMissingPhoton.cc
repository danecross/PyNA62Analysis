// ------------------------------------------------------------
// K2pi Selection with  missing photon in LKr
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// March 2019
// ------------
// The selected sample could be useful for photon vetoes studies:
// the missing photon 4-momentum, the decay vertex position and
// the event time are given as ouput of the analyzer.
// ------------
// The selection is based on Calorimetric PID and Kinematics.
// ------------
/// The analyzer has the output:
/// SelectedK2piWithMissingPhoton =  K2piSelectionWithMissingPhotonOutput (struct defined in K2piSelectionWithMissingPhoton.hh)
/// Missing Photon Energy = 0 means that the event did not pass the K2piSelectionWithMissingPhoton.
///
/// An example of requesting the output:
/// \code
///   K2piSelectionWithMissingPhotonOutput K2piWithMissingPhoton =
///    *(K2piSelectionWithMissingPhotonOutput*)GetOutput("K2piSelectionWithMissingPhoton.SelectedK2piWithMissingPhoton");
///   TLorentzVector selectedMissingPhotonFourMomentum = K2piWithMissingPhoton.MissingPhotonFourMomentum ;
///   TVector3 selectedDecayVertex = K2piWithMissingPhoton.DecayVertex ;
///   Double_t selectedEventTime = K2piWithMissingPhoton.EventTime ;
///   if (selectedMissingPhotonFourMomentum.E()<=0) return; // K2piSelectionWithMissingPhoton not passed.
/// \endcode
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#include <iostream>
#include <TChain.h>
#include <TLine.h>
#include "K2piSelectionWithMissingPhoton.hh"
#include "Event.hh"
#include "ConfigSettings.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "EnergyCluster.hh"
#include "SpectrometerCalorimetersAssociation.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

K2piSelectionWithMissingPhoton::K2piSelectionWithMissingPhoton(Core::BaseAnalysis *ba) : Analyzer(ba, "K2piSelectionWithMissingPhoton"){
  RequestTree("CHOD", new TRecoCHODEvent, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("CHANTI", new TRecoCHANTIEvent, "Reco");
  RequestTree("LKr",    new TRecoLKrEvent,    "Reco");
  RequestTree("LAV",    new TRecoLAVEvent,    "Reco");
  RequestTree("IRC",    new TRecoIRCEvent,    "Reco");
  RequestTree("SAC",    new TRecoSACEvent,    "Reco");
  RequestTree("RICH",   new TRecoRICHEvent,   "Reco");
  RequestTree("Cedar",   new TRecoCedarEvent,   "Reco");
  RequestTree("GigaTracker",   new TRecoGigaTrackerEvent,   "Reco");
  RequestTree("SAV",    new TRecoSAVEvent,    "Reco");

  RequestL0Data();

  AddParam("TriggerMask", &fTriggerMask, 0xFF);

  // GigaTrackerRecoAlgorithm (needed only for MC)
  fGTKAlgo = new GigaTrackerRecoAlgorithm(ba, this, "GTKRecoAlgo");
  fGTKAlgo->SetRedoTimeCorr(false);
  fGTKAlgo->SetTimeWindowWrtReference(1.0); // [ns]

  fSG = new SpectrometerGigaTrackerMatchingTool();
  fSG->SetMatchingTimingCuts(-0.5, 0.5);

  // outputs
  fSelectedK2piWithMissingPhoton.MissingPhotonFourMomentum.SetPxPyPzE(0., 0., 0., 0.) ;
  fSelectedK2piWithMissingPhoton.DecayVertex.SetXYZ(0., 0., 0.) ;
  fSelectedK2piWithMissingPhoton.EventTime = -99999999.9 ;
}

K2piSelectionWithMissingPhoton::~K2piSelectionWithMissingPhoton() {
  delete fSG;
}

void K2piSelectionWithMissingPhoton::InitOutput() {
  RegisterOutput("SelectedK2piWithMissingPhoton", &fSelectedK2piWithMissingPhoton);
}

void K2piSelectionWithMissingPhoton::InitHist(){
  fReadingData = GetIsTree();

  if (fReadingData){

    BookHisto("hNEventsAfterCuts", new TH1D("hNEventsAfterCuts", "N events after cuts", 60, -0.5, 59.5));
    BookHisto("hL0TriggerWord", new TH1I("hL0TriggerWord","L0 Mask , 10=CTRL, 11=CTRL&Mask1 ", 12, -0.5, 11.5));

    // Track
    BookHisto("hNTracks", new TH1D("hNTracks", "Number of tracks", 11, -0.5, 10.5));
    BookHisto("hTrackCharge", new TH1D("hTrackCharge", "Track Charge", 11, -5.5, 5.5));
    BookHisto("hPtrack", new TH1D("hPtrack", "Track Momentum; P [GeV/c]", 100, 0.0, 100.0));
    BookHisto("hPtrack-Ptrackbefore", new TH1D("hPtrack-Ptrackbefore", " PTrack - PTrack_BeforeFit; [GeV/c]", 200, -10, 10));
    BookHisto("hTrackChi2", new TH1D("hTrackChi2", "TrackChi2; Chi2", 100, 0.0, 30.0));
    BookHisto("hTriggerTime-TrackRICHTime", new TH1D("hTriggerTime-TrackRICHTime","Trigger Time - TrackRICHTime; T [ns]", 600, -15, 15));
    BookHisto("hNGoodTracks", new TH1D("hNGoodTracks", "Number of good tracks", 11, -0.5, 10.5));
    BookHisto("hTrackAssociation", new TH1D("hTrackAssociation", "0=!RICH, 1=!NewCHOD, 2=!LKr, 3=MUV3, 4=!CHOD, 5=MUV1, 6=MUV2 7=return, 9=continue", 11, -0.5, 10.5));

    // RICH
    BookHisto("hRICHTime", new TH1D("hRICHTime","RICH Time; T [ns]", 100, -50, 50));
    BookHisto("hRICHRadiusVSMomentum", new TH2D("hRICHRadiusVSMomentum", "Radius VS Momentum in RICH;Momentum [GeV/c];Radius [mm]", 100,0, 100, 250, 0, 250));
    BookHisto("hRICHRadius", new TH1D("hRICHRadius", "Ring Radius in RICH; Radius [mm]", 150, 100, 250));
    BookHisto("hRICHMass", new TH1D("hRICHMass", "RICH Mass; [MeV/c^{2}]", 350, -100, 250));
    BookHisto("hRICHMass2", new TH1D("hRICHMass2", "RICH Mass2; [MeV^{2}/c^{4}]", 400, -10000, 30000));
    BookHisto("hLHbgmax", new TH1D("hLHbgmax","RICH LH not-pion max; LH bgmax", 101, 0, 1.01));

    // GTK
    BookHisto("hSpectrometerGTKMatching_Done", new TH1D("hSpectrometerGTKMatching_Done", "hSpectrometerGTKMatching_Done", 11, -5.5, 5.5));
    BookHisto("hGTKCandidateTimeRICH", new TH1D("hGTKCandidateTimeRICH","GTK - RICH time; T [ns]", 600, -3, 3));
    BookHisto("hNGTKCandidates", new TH1D("hNGTKCandidates", "Number of GTK Candidates", 50, -0.5, 49.5));
    BookHisto("hZvtx",    new TH1D("hZvtx", "Z of track-beam axis vertex;Vertex z [m]", 200, 50, 250));
    BookHisto("hCDA",     new TH1D("hCDA", "CDA of the track-beam axis vertex;CDA [mm]", 200, 0, 200));
    BookHisto("hZvtxCDA", new TH2D("hZvtxCDA", "CDA vs Z of the track-beam axis vertex;Vertex z [m];CDA [mm]", 75, 50, 200, 100, 0, 200));
    BookHisto("hKaonMomentum", new TH1D("hKaonMomentum","Kaon Momentum; P_Kaon [GeV/c]", 200, 65, 85));

    // KTAG
    BookHisto("hNCedarCandidates", new TH1D("hNCedarCandidates", "Number of Cedar Candidates", 11, -0.5, 10.5));
    BookHisto("hNCedarCandidates5Sectors", new TH1D("hNCedarCandidates5Sectors", "Number of Cedar Candidates with 5 Sectors at least", 11, -0.5, 10.5));
    BookHisto("hCedarTimes", new TH1D("hCedarTimes","Time of all Cedar candidates; T} [ns]", 100, -50, 50));
    BookHisto("hCedarTimes-EventTime", new TH1D("hCedarTimes-EventTime","Times in Cedar - Event Time; T [ns]", 200, -10, 10));
    BookHisto("hCedar-EventTime", new TH1D("hCedar-EventTime","Best Time in Cedar - Event Time; T [ns]", 600, -3, 3));

    // Calorim PID (R. Aliberti)
    BookHisto("hCalorimElectronProbability", new TH1D("hCalorimElectronProbability","Calorim electron probability; e prob", 100, 0, 1));
    BookHisto("hCalorimMuonProbability", new TH1D("hCalorimMuonProbability","Calorim muon probability; mu prob", 100, 0, 1));
    BookHisto("hCalorimPionProbability", new TH1D("hCalorimPionProbability","Calorim Pion probability; pi prob", 100, 0, 1));

    // LKr - positron
    BookHisto("hNClusters", new TH1D("hNClusters", "Number of LKr clusters", 11, -0.5, 10.5));
    BookHisto("hCounterEl", new TH1D("hCounterEl", "Number of found electron(s)", 11, -0.5, 10.5));
    BookHisto("hLKrElectronTime", new TH1D("hLKrElectronTime","Electron Time in LKr; T [ns]", 100, -50, 50));
    BookHisto("hEvent-LKrElectronTime", new TH1D("hEvent-LKrElectronTime","Event Time - e+ Time in LKr; T [ns]", 600, -3, 3));
    BookHisto("hElectronEnergyLKr", new TH1D("hElectronEnergyLKr","e+ Energy in LKr [GeV]; E_e+ [GeV]", 100, 0, 100));
    BookHisto("hEOP",     new TH1D("hEOP", "Track E/p; E/p", 200, 0.0, 2.0));

    // LKr - photons
    BookHisto("hClustersTimes-EventTime", new TH1D("hClustersTimes-EventTime","Cls Times Lkr - Event Time; T [ns]", 200, -10, 10));
    BookHisto("hNPhotonInTime", new TH1D("hNPhotonInTime", "Number of photon in time", 11, -0.5, 10.5));

    // Muv3
    BookHisto("hNMUV3Cand", new TH1D("hNMUV3Cand", "Number of Candidates in MUV3", 21, -0.5, 20.5));
    BookHisto("hMUV3-EventTime", new TH1D("hMUV3-EventTime","Closest Time in MUV3 - EventTime; T [ns]", 100, -50, 50));

    // Kinematics
    BookHisto("hMissMass_K2pi", new TH1D("hMissMass_K2pi","Pi0 mass in Pi+ Pi0 Hypothesis; M_{#pi0} [MeV/c^{2}]", 200, 90, 190));
    BookHisto("hLostPhotonEnergy", new TH1D("hLostPhotonEnergy","Lost Photon Energy; E [GeV]", 150, 0, 75));
  }

  else{ // histo mode
  }
}

void K2piSelectionWithMissingPhoton::Process(Int_t) {

  SetOutputState("SelectedK2piWithMissingPhoton", kOValid);
  fSelectedK2piWithMissingPhoton.MissingPhotonFourMomentum.SetPxPyPzE(0., 0., 0., 0.) ;
  fSelectedK2piWithMissingPhoton.DecayVertex.SetXYZ(0., 0., 0.) ;
  fSelectedK2piWithMissingPhoton.EventTime = -99999999.9 ;

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  L0TPData* L0Packet = GetL0Data();
  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  Bool_t Mask1 = false;
  Double_t TriggerTime = -9999999.9 ;
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only
  FillHisto("hNEventsAfterCuts",0);

  if (!GetWithMC()) {
    if (CTRLTrigger) {
      FillHisto("hL0TriggerWord", 10);
    }
    for(Int_t ibit=0; ibit<8; ibit++){
      if (L0TriggerWord & (Int_t)pow(2,ibit)) {
	FillHisto("hL0TriggerWord", ibit);
	if (ibit==1) Mask1 = true;
      }
    }
    if(CTRLTrigger && Mask1) FillHisto("hL0TriggerWord", 11);
    TriggerTime = (UChar_t)L0Packet->GetReferenceFineTime()*TdcCalib;
  } // end if !GetWithMC
  else{
    TriggerTime = 0.0 ;
  }
  //  if(!CTRLTrigger && !Mask1) return ;
  FillHisto("hNEventsAfterCuts",1);

  fRunID = GetRunID();
  fBurstID = GetBurstID();
  TRecoCedarEvent*    CedarEvent    = static_cast<TRecoCedarEvent*>(GetEvent("Cedar"));
  TRecoMUV3Event*    MUV3Event    = static_cast<TRecoMUV3Event*>(GetEvent("MUV3"));
  TRecoGigaTrackerEvent*    GTKEvent    = static_cast<TRecoGigaTrackerEvent*>(GetEvent("GigaTracker"));

  //////////////////////////////////////////   track
  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  FillHisto("hNTracks", Tracks.size());
  if (Tracks.size()<1) return;
  FillHisto("hNEventsAfterCuts",2);

  Int_t NGoodTracks = 0 ;
  Int_t GoodTrackID = -999;
  Double_t DeltaTTrigger = 10.0 ; // ns
  for(UInt_t iSpecTrack=0; iSpecTrack<Tracks.size();iSpecTrack++){

    TRecoSpectrometerCandidate* Scand = Tracks[iSpecTrack].GetSpectrometerCandidate();

    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kLKr)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kRICH)) continue;
    //  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kCHOD)) continue;
    //  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kMUV3)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kMUV1)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kMUV2)) continue;
    if (Scand->GetNChambers()!=4) continue;
    Int_t Q = Tracks[iSpecTrack].GetCharge();
    FillHisto("hTrackCharge", Q);
    if (Q!=1) continue;
    Double_t Ptrack       = Tracks[iSpecTrack].GetMomentum(); // spectrometer calibration included
    FillHisto("hPtrack", 0.001*Ptrack);
    if (Ptrack < 8000 || Ptrack > 60000) continue;

    Double_t Ptrackbefore = Tracks[iSpecTrack].GetMomentumBeforeFit();
    FillHisto("hPtrack-Ptrackbefore", 0.001*(Ptrack-Ptrackbefore));
    if (fabs(Ptrack-Ptrackbefore)>5000.0) continue;
    Double_t Chi2track    = Tracks[iSpecTrack].GetChi2();
    FillHisto("hTrackChi2", Chi2track);
    if (Chi2track>20.0) continue;
    if (!GetWithMC()){
      FillHisto("hTriggerTime-TrackRICHTime",(TriggerTime - Tracks[iSpecTrack].GetRICHSingleRingTrkCentredTime()));
      if (fabs(TriggerTime - Tracks[iSpecTrack].GetRICHSingleRingTrkCentredTime())>DeltaTTrigger) continue;
    }
    NGoodTracks++;
    GoodTrackID = iSpecTrack ;
  } // end for iSpecTrack
  FillHisto("hNGoodTracks",NGoodTracks);
  if (NGoodTracks != 1) return;
  FillHisto("hNEventsAfterCuts",3);

  Bool_t TrackAssociation = true ;
  if (!Tracks[GoodTrackID].RICHAssociationSuccessful()) { FillHisto("hTrackAssociation",0); TrackAssociation = false ; }
  if (!Tracks[GoodTrackID].NewCHODAssociationExists()) { FillHisto("hTrackAssociation",1); TrackAssociation = false ; }
  if (!Tracks[GoodTrackID].LKrAssociationExists()) { FillHisto("hTrackAssociation",2); TrackAssociation = false ; }
  if (Tracks[GoodTrackID].MUV3AssociationExists()) { FillHisto("hTrackAssociation",3); TrackAssociation = false ; }
  if (!Tracks[GoodTrackID].CHODAssociationExists()) { FillHisto("hTrackAssociation",4); TrackAssociation = false ; }
  if ( TrackAssociation ) { FillHisto("hTrackAssociation",9); }
  else { FillHisto("hTrackAssociation",7);
    //  return;
  }
  FillHisto("hNEventsAfterCuts",4);

  TRecoSpectrometerCandidate* Scand = Tracks[GoodTrackID].GetSpectrometerCandidate();

  TVector3 Track3Momentum = Tracks[GoodTrackID].GetMomentumBeforeMagnet() ;
  TLorentzVector Pion;
  Pion.SetVectM(Track3Momentum , MPI);

  // ************************************ RICH ***************************************
  // RICH cuts not applied

  // if (!(Tracks[GoodTrackID].GetRICHSingleRingTrkCentredRadius()) ) return ;
  Double_t RingRadius = Tracks[GoodTrackID].GetRICHSingleRingTrkCentredRadius();
  Double_t RICHTime = Tracks[GoodTrackID].GetRICHSingleRingTrkCentredTime();
  FillHisto("hNEventsAfterCuts",5);

  FillHisto("hRICHTime",RICHTime);
  FillHisto("hRICHRadiusVSMomentum",0.001*Pion.P() , RingRadius );
  FillHisto("hRICHRadius", RingRadius);

  Double_t RICHMass = Tracks[GoodTrackID].GetRICHSingleRingTrkCentredMass();
  Double_t RICHMass2 = RICHMass*RICHMass ;
  if (RICHMass<0) RICHMass2 = -1.0*RICHMass2 ;
  FillHisto("hRICHMass", RICHMass);
  FillHisto("hRICHMass2", RICHMass2);
  // if( RICHMass<130.0 || RICHMass>200.0) return ;
  FillHisto("hNEventsAfterCuts",6);

  Double_t RICH_LH[5];
  RICH_LH[0] = Tracks[GoodTrackID].GetRICHLikelihoodElectron();
  RICH_LH[1] = Tracks[GoodTrackID].GetRICHLikelihoodMuon();
  RICH_LH[2] = Tracks[GoodTrackID].GetRICHLikelihoodPion();
  RICH_LH[3] = Tracks[GoodTrackID].GetRICHLikelihoodKaon();
  RICH_LH[4] = Tracks[GoodTrackID].GetRICHLikelihoodBackground();
  Double_t LH_notPion_max = RICH_LH[0] ;
  for (Int_t i = 1 ; i < 5 ; i++ ){
    if (RICH_LH[i] > LH_notPion_max && i!=2){
      LH_notPion_max = RICH_LH[i] ;
    }
  }
  FillHisto("hLHbgmax",LH_notPion_max);
  // if(LH_notPion_max>0.8) return ;
  FillHisto("hNEventsAfterCuts",7);
  // ******************************************* end RICH *********************************************

  // ********************* Event Time definition *********************************************************
  Double_t DeltaT = 2.0 ;
  Double_t EventTime = RICHTime ;
  Double_t TimeWeightLKr = 1/5.0 ; // 500 ps
  Double_t TimeWeightGTK = 1/1.2 ; // 120 ps
  Double_t TimeWeightRICHKTAG = 1/0.8 ;// 80 ps
  // Double_t TimeWeightCHOD = 1/2.0 ; // 200 ps

  // ********************************************************** GTK ***************************************************************

  // GigaTrackerRecoAlgorithm (needed only for MC)
  if (GetWithMC()) fGTKAlgo->Process(GTKEvent, EventTime);

  Double_t cda = -999.9;
  TVector3 Vertex;
  TVector3 BeamParticleMomentum;
  Double_t GTKTime = -999.9 ;
  Bool_t SpectrometerGTKMatching_Done = false ;
  Int_t NGTKCandidates = -999 ;
  TRecoGigaTrackerCandidate* gtkcand ;
  fSG->Match(GTKEvent, Scand, RICHTime, kRICH);
  SpectrometerGTKMatching_Done = fSG->BestGTKTrackFound();
  if(SpectrometerGTKMatching_Done){
    NGTKCandidates = fSG->GetNGTKCandidates();
    cda = fSG->GetBestCDA();
    BeamParticleMomentum = fSG->GetBestBeamMomentum();
    Vertex = fSG->GetBestVertex();
    Int_t gtk_cand_index = fSG->GetBestIndex();
    gtkcand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(gtk_cand_index));

    GTKTime = gtkcand->GetTime() ;
    FillHisto("hGTKCandidateTimeRICH",GTKTime-RICHTime);
    FillHisto("hSpectrometerGTKMatching_Done",1) ;
    FillHisto("hNGTKCandidates",NGTKCandidates);
    }
  else { FillHisto("hSpectrometerGTKMatching_Done",-1) ;  return ; }
  FillHisto("hNEventsAfterCuts",8);

  Double_t Zvtx = Vertex.Z();
  FillHisto("hZvtx", 0.001*Zvtx); // [m]
  if (Zvtx<110000 || Zvtx>170000) return;
  FillHisto("hNEventsAfterCuts",9);
  FillHisto("hCDA", cda); // [mm]
  if (cda > 20) return ;
  FillHisto("hNEventsAfterCuts",10);
  FillHisto("hZvtxCDA", 0.001*Zvtx, cda);

  TLorentzVector Kaon;
  Kaon.SetVectM(BeamParticleMomentum , MKCH);
  FillHisto("hKaonMomentum", 0.001*Kaon.P());

  TLorentzVector Pi0_K2pi;
  Pi0_K2pi = Kaon - Pion ;

  EventTime = (TimeWeightRICHKTAG*RICHTime+TimeWeightGTK*GTKTime)/(TimeWeightRICHKTAG+TimeWeightGTK);

  // *********************************************  end  GTK *************************************************

  // *******************************************    KTAG  ***************************************************
  Int_t NCedarCandidatesNoSectors = CedarEvent->GetNCandidates();
  FillHisto("hNCedarCandidates",NCedarCandidatesNoSectors);
  if (NCedarCandidatesNoSectors<1) return;
  FillHisto("hNEventsAfterCuts",11);
  std::vector<TRecoCedarCandidate*> CedarCand5Sectors;
  CedarCand5Sectors.clear();
  for (Int_t iiCedar=0; iiCedar<NCedarCandidatesNoSectors; iiCedar++) {
    TRecoCedarCandidate* CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iiCedar));
    Int_t NSectors = CedarCand->GetNSectors();
    if (NSectors > 4) CedarCand5Sectors.push_back(CedarCand);
  }
  Int_t NCedarCandidates = CedarCand5Sectors.size();
  FillHisto("hNCedarCandidates5Sectors",NCedarCandidates);
  if (NCedarCandidates<1) return; // with 5 sectors
  FillHisto("hNEventsAfterCuts",12);
  std::vector<Double_t> CedarTimes(NCedarCandidates, -999999.9);
  for (Int_t iCedar=0; iCedar<NCedarCandidates; iCedar++) {
    CedarTimes[iCedar] = CedarCand5Sectors[iCedar]->GetTime();
    FillHisto("hCedarTimes",CedarTimes[iCedar]);
    FillHisto("hCedarTimes-EventTime", (CedarTimes[iCedar]-EventTime));
  }
  Double_t KTAGTime=CedarTimes[0];
  for (Int_t jCedar=1; jCedar<NCedarCandidates; jCedar++) {
    if ( fabs(CedarTimes[jCedar]-EventTime)<fabs(KTAGTime-EventTime) ) KTAGTime = CedarTimes[jCedar];
  }
  FillHisto("hCedar-EventTime", (KTAGTime-EventTime));
  if ( (!GetWithMC()) && fabs(KTAGTime-EventTime)>DeltaT ) return;
  FillHisto("hNEventsAfterCuts",13);

  EventTime = (TimeWeightRICHKTAG*RICHTime+TimeWeightGTK*GTKTime+TimeWeightRICHKTAG*KTAGTime)/(2.0*TimeWeightRICHKTAG+TimeWeightGTK);

  // ******************************************  end  KTAG  *************************************************

  // **************************************** Calorimetric PID (R. Aliberti analyzer) **************

  TClonesArray *ShowerCandidates = (TClonesArray*)GetOutput("SpectrometerCalorimetersAssociation.MatchedClusters");
  CalorimeterCluster *TrackCluster = static_cast<CalorimeterCluster*>(ShowerCandidates->At(GoodTrackID));
  Double_t Calorimetric_PID[3];
  Calorimetric_PID[0] = TrackCluster->GetIsElectronProbability();
  Calorimetric_PID[1] = TrackCluster->GetIsMuonProbability();
  Calorimetric_PID[2] = TrackCluster->GetIsPionProbability();
  FillHisto("hCalorimElectronProbability",Calorimetric_PID[0]);
  FillHisto("hCalorimMuonProbability",Calorimetric_PID[1]);
  FillHisto("hCalorimPionProbability",Calorimetric_PID[2]);
  if (Calorimetric_PID[2]<0.8) return;
  FillHisto("hNEventsAfterCuts",14);

  // ********************************  LKr track *********************************
  // "elctron" identifies the track (even if pion or muon)
  std::vector<EnergyCluster> Clusters =  *(std::vector<EnergyCluster>*)GetOutput("EnergyClusterBuilder.Output");
  Int_t NClusters = Clusters.size();
  FillHisto ("hNClusters", NClusters);
  if (NClusters < 2) return; // at least: track and one photon from pi0
  FillHisto("hNEventsAfterCuts",15);
  std::vector<EnergyCluster> ClustersInTime;
  ClustersInTime.clear();

  Int_t iElectron=99;
  Int_t CounterEl = 0;
  for (Int_t i=0; i<NClusters; i++) {
    if (Clusters[i].SpectrometerAssociationExists() && Clusters[i].GetLKrCandidate()->GetClusterEnergy()>4000 && fabs(Clusters[i].GetLKrCandidate()->GetTime()-EventTime)<DeltaT) {
      iElectron=i;
      CounterEl++;
    }
  }
  FillHisto("hCounterEl",CounterEl);
  if ( CounterEl!=1 ) return;
  FillHisto("hNEventsAfterCuts",16);
  Double_t ElectronTime = Clusters[iElectron].GetLKrCandidate()->GetTime();
  FillHisto("hLKrElectronTime", ElectronTime);
  FillHisto("hEvent-LKrElectronTime", (EventTime-ElectronTime));
  if(fabs(EventTime-ElectronTime)>DeltaT && (!GetWithMC())) return ;
  FillHisto("hNEventsAfterCuts",17);
  ClustersInTime.push_back(Clusters[iElectron]);

  Double_t ElectronEnergyLKr = ClustersInTime[0].GetLKrCandidate()->GetClusterEnergy();
  FillHisto("hElectronEnergyLKr", 0.001*ElectronEnergyLKr);
  if (ElectronEnergyLKr < 4000) return;
  FillHisto("hNEventsAfterCuts",18);

  Double_t eop = Tracks[GoodTrackID].GetLKrEoP();
  FillHisto("hEOP", eop);
  // if (eop < 0.1 || eop > 0.9) return;
  if (eop > 0.9) return;
  FillHisto("hNEventsAfterCuts",19);

  EventTime = (TimeWeightRICHKTAG*RICHTime+TimeWeightGTK*GTKTime+TimeWeightRICHKTAG*KTAGTime+TimeWeightLKr*ElectronTime)/(2.0*TimeWeightRICHKTAG+TimeWeightGTK+TimeWeightLKr);

  // ******************************************** LKr photons *************************************
  Double_t LKrStartPos = GeometricAcceptance::GetInstance()->GetZLKr();
  TVector3 GammaMomentum;
  std::vector<PhotonCandidate> GammaCandidates;
  GammaCandidates.clear();
  for(Int_t iCls=0; iCls<NClusters; iCls++){
    Double_t myClusterEnergy = Clusters[iCls].GetLKrCandidate()->GetClusterEnergy();
    // Discard clusters close to dead cells
    if (Clusters[iCls].GetLKrCandidate()->GetClusterDDeadCell()<20.) continue;
    // Discard clusters associated to track
    if (iCls==iElectron) continue;
    // Create the photon candidate
    PhotonCandidate GammaCandidate;
    GammaCandidate.ClusterID = iCls;
    GammaCandidate.Time = Clusters[iCls].GetLKrCandidate()->GetTime();
    // Set the momentum direction
    Double_t Cluster_X = Clusters[iCls].GetLKrCandidate()->GetClusterX();
    Double_t Cluster_Y = Clusters[iCls].GetLKrCandidate()->GetClusterY();
    GammaMomentum.SetXYZ(Cluster_X, Cluster_Y, LKrStartPos);
    if ( (Cluster_X*Cluster_X + Cluster_Y*Cluster_Y)<(180.0*180.0) ) continue; // 18 cm from the center
    GammaMomentum = GammaMomentum - Vertex;
    // Set the momentum magnitude
    GammaMomentum.SetMag(myClusterEnergy);
    // Set the lorentz vector
    GammaCandidate.FourMomentum.SetVect(GammaMomentum);
    GammaCandidate.FourMomentum.SetE(myClusterEnergy);

    // Timing check
    Double_t ClusterDeltaT = Clusters[iCls].GetLKrCandidate()->GetTime() - EventTime ;
    FillHisto("hClustersTimes-EventTime", ClusterDeltaT);
    if (fabs(ClusterDeltaT) > DeltaT) {
      continue;
    }
    if (myClusterEnergy < 4000) continue; // min cluster energy = 4 GeV
    GammaCandidates.push_back(GammaCandidate);
  }

  Int_t NPhotonInTime = GammaCandidates.size();
  FillHisto("hNPhotonInTime",NPhotonInTime); // "photon in time" means "good photon"
  if(NPhotonInTime!=1) return; // only one photon (from pi0)
  FillHisto("hNEventsAfterCuts",20);

  TLorentzVector LKrPhoton = GammaCandidates[0].FourMomentum;
  TLorentzVector LostPhoton = Pi0_K2pi - LKrPhoton ;

  // ******************************* Event - trigger Timing ************************************************

  if (!GetWithMC()) {
    if ( (fabs(EventTime - TriggerTime) > DeltaTTrigger) ) return ;
  }
  FillHisto("hNEventsAfterCuts",21);

  // ************************************************* MUV3 *****************************************
  Double_t DeltaTMUV3 = 5.0 ;
  Int_t NMUV3Cand = MUV3Event->GetNCandidates();
  FillHisto("hNMUV3Cand", NMUV3Cand);
  if(NMUV3Cand>0){
    TRecoMUV3Candidate* ClosestMUV3Cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(0));
    Double_t ClosestMUV3Time = ClosestMUV3Cand->GetTime();
    for (Int_t iMUV3=1; iMUV3<NMUV3Cand; iMUV3++) {
      TRecoMUV3Candidate* TempMUV3Cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(iMUV3));
      Double_t TempMUV3Time = TempMUV3Cand->GetTime();
      if ( fabs(TempMUV3Time-EventTime)<fabs(ClosestMUV3Time-EventTime) ) {
        ClosestMUV3Cand = TempMUV3Cand ;
        ClosestMUV3Time = ClosestMUV3Cand->GetTime();
      }
    }
    FillHisto("hMUV3-EventTime",ClosestMUV3Time-EventTime);
    if ( fabs(ClosestMUV3Time-EventTime)<DeltaTMUV3 || GetWithMC() ) return;
  }
  FillHisto("hNEventsAfterCuts",22);

  // ******************************   Kinematics K2pi ************************************
  FillHisto("hMissMass_K2pi",Pi0_K2pi.M());
  if( Pi0_K2pi.M()<120 || Pi0_K2pi.M()>150 ) return;
  FillHisto("hNEventsAfterCuts",23);

  FillHisto("hLostPhotonEnergy", 0.001*LostPhoton.E()); // [GeV]

  // **************************** END SELECTION **************************************************

  fSelectedK2piWithMissingPhoton.MissingPhotonFourMomentum = LostPhoton ;
  fSelectedK2piWithMissingPhoton.DecayVertex = Vertex ;
  fSelectedK2piWithMissingPhoton.EventTime = EventTime ;
} // end Process Function

void K2piSelectionWithMissingPhoton::PostProcess() {
  /// \MemberDescr
  /// This function is called after an event has been processed by all analyzers.
  /// Invalidate all registered outputs.
  /// \EndMemberDescr
  SetOutputState("SelectedK2piWithMissingPhoton", kOInvalid);
}

void K2piSelectionWithMissingPhoton::EndOfJobUser() {
  if (fReadingData) SaveAllPlots();
}
