// ----------------------------------------------------------------------------------------
// History:
//
// Created by Roberta Volpe (roberta.volpe@cern.ch) December 2016
//-----------------------------------------------------------------------------------------
/// \class RICHMonitor
/// \Brief
/// It monitors the quality of the data from the RICH point of view
/// \EndBrief
/// \Detailed
///  It can be run in two different mode, so that can be used in the two step postprocessing
/// 1) If it is run over the reconstructed data (fReadingData):
///    it selects control trigger events and performs general checks, as the number of candidates, hits, and timing.
///    The ring reconstruction is performed using the RICHSingleRingTrkSeededFit in ToolsLib
///    and SpectrometerRICHAssociationSingleRing.cc in PhysicsTools.
///    It produces as output a root file with several histograms concerning
///    the number of candidates, hits, timing informations,
///    efficiency of the ring reconstruction given the track reconstruction.
///    Furthermore, a simple selection is used to separate samples of pions, muons and positrons,
///    used to fill histograms useful to give raw estimation of the pion/muon/positron separation.
/// 2) If it is run over its own output, it should be run with the option --histo.
//     It reads the histos and draw the main significative distributions.
///    Its output is a pdf file and a root file with the histos.
/// \author  Roberta Volpe (roberta.volpe@cern.ch)
/// \EndDetailed
#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include <TF1.h>
#include "RICHMonitor.hh"
#include "BaseAnalysis.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "BeamParameters.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

RICHMonitor::RICHMonitor(Core::BaseAnalysis *ba) : Analyzer(ba, "RICHMonitor") {
  RequestTree("Cedar",  new TRecoCedarEvent,  "Reco");
  RequestTree("CHANTI", new TRecoCHANTIEvent, "Reco");
  RequestTree("LKr",    new TRecoLKrEvent,    "Reco");
  RequestTree("RICH",    new TRecoRICHEvent,    "Reco");
  RequestTree("LAV",new TRecoLAVEvent);
  RequestTree("IRC",new TRecoIRCEvent);
  RequestTree("SAC",new TRecoSACEvent);
  RequestTree("CHANTI",new TRecoCHANTIEvent);
  RequestTree("CHOD",new TRecoCHODEvent);

  RequestL0Data();
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  fReadingData = kTRUE;
  fCDAcomp     = new TwoLinesCDA();


  fRecoHitTimeWrtReferenceVsReadoutChannelNoT0= nullptr ;
  fRecoHitTimeWrtReferenceVsReadoutChannel= nullptr ;
  fRecoHitTimeWrtReferenceVsSeqChannelNoT0= nullptr ;
  fRecoHitTimeWrtReferenceVsSeqChannel= nullptr ;

  fDigiTimeRawFineVsROChannel= nullptr ;
  fHTimeRawJUP_PerBurst= nullptr ;
  fHTimeRawJDW_PerBurst= nullptr ;
  fHTimeRawSUP_PerBurst= nullptr ;
  fHTimeRawSDW_PerBurst= nullptr ;
  fHTimeRawMUL_PerBurst= nullptr ;

  fHSigmaTimeRawJUP_PerBurst= nullptr ;
  fHSigmaTimeRawJDW_PerBurst= nullptr ;
  fHSigmaTimeRawSUP_PerBurst= nullptr ;
  fHSigmaTimeRawSDW_PerBurst= nullptr ;
  fHSigmaTimeRawMUL_PerBurst= nullptr ;

  fHTotalEventsPerBurst= nullptr ;
  fHPhysicsEventsPerBurst= nullptr ;
  fHControlEventsPerBurst= nullptr ;
  fhNSCHitsPerBurst= nullptr ;
  fhNPMHitsPerBurst= nullptr ;
  fhNPMTimeCandPerBurst= nullptr ;
  fhNPMTimeCandPerBurst_phys= nullptr ;
  fhNSCHitsInTimePerBurst= nullptr ;
  fhNPMHitsInTimePerBurst= nullptr ;
  fhNSCHitsInTimePerBurst_phys= nullptr ;
  fhNPMHitsInTimePerBurst_phys= nullptr ;
  fhNPMTimeCandInTimePerBurst= nullptr ;
  fhNPMTimeCandInTimePerBurst_phys= nullptr ;
  fhNSCHitsPerBurst_phys= nullptr ;
  fhNPMHitsPerBurst_phys= nullptr ;
  fHNSCHitsPerControlEvent= nullptr ;
  fHNPMHitsPerControlEvent= nullptr ;
  fHNPMTimeCandPerControlEvent= nullptr ;
  fHNSCHitsInTimePerControlEvent= nullptr ;
  fHNPMHitsInTimePerControlEvent= nullptr ;
  fHNSCHitsPerPhysicsEvent= nullptr ;
  fHNPMHitsPerPhysicsEvent= nullptr ;
  fHNPMTimeCandPerPhysicsEvent= nullptr ;
  fHNSCHitsInTimePerPhysicsEvent= nullptr ;
  fHNPMHitsInTimePerPhysicsEvent= nullptr ;
  fHNPMTimeCandInTimePerControlEvent= nullptr ;
  fHNPMTimeCandInTimePerPhysicsEvent= nullptr ;
  fhNElecRing_vs_BurstID= nullptr ;
  fhNPiRing_vs_BurstID= nullptr ;
  fhNMuRing_vs_BurstID= nullptr ;
  fhNElecTrack_vs_BurstID= nullptr ;
  fhNPiTrack_vs_BurstID= nullptr ;
  fhNMuTrack_vs_BurstID= nullptr ;
  fhNSCHitsPerBurstID= nullptr ;
  fhNPMHitsPerBurstID= nullptr ;
  fhPMHitQualityBadBurst= nullptr ;
  fhSCHitQualityBadBurst= nullptr ;
  fhPhysEvWith0SCHitsPerBurst= nullptr ;
  fhPhysEvWith0SCHitsPerBurst_EvQual0= nullptr ;
  fhPhysEvWith0SCHitsPerBurst_EvQualNot0= nullptr ;
  fhNPMTimeCandPerBurstID= nullptr ;
  fhEffiElRing_vs_BurstID= nullptr ;
  fhEffiMuRing_vs_BurstID= nullptr ;
  fhEffiPiRing_vs_BurstID= nullptr ;
  fhNRICHEventPMHits= nullptr ;
  fhNRICHEventSCHits= nullptr ;
  fhNRICHEventPMHits_phys= nullptr ;
  fhNRICHEventSCHits_phys= nullptr ;
  fhPMHitQuality= nullptr ;
  fhSCHitQuality= nullptr ;
  fhNRICHTimeCand= nullptr ;
  fhNRICHRingCand= nullptr ;
  fhRICHTimeCandNhits= nullptr ;
  fhTimeCandTime= nullptr ;
  fhRingCandTime= nullptr ;
  fhNRICHSingleRingCand= nullptr ;
  fhDeltaT_t1t2= nullptr ;
  fhDeltaT_RICHCand_CedarCand= nullptr ;
  fhDeltaT_CedarCand_RICHHit= nullptr ;
  fhDeltaT_RICHHitRichCand_vs_SeqID= nullptr ;
  fhDeltaT_RICHHitCedarCand_vs_SeqID= nullptr ;

  fhPAllPiTracks= nullptr ;
  fhPPiTracks= nullptr ;
  fhPAllMuTracks= nullptr ;
  fhPMuTracks= nullptr ;
  fhPAllElecTracks= nullptr ;
  fhPElecTracks= nullptr ;

  fhSingleRing_Dist= nullptr ;
  fhMuRing_RICHMass= nullptr ;
  fhPiRing_RICHMass= nullptr ;
  fhElecRing_RICHMass= nullptr ;
  fhPEvtSelTracks= nullptr ;
  fhSingleRing_RICHMass= nullptr ;

  fhPiRing_Radius= nullptr ;
  fhPiRing_nHits= nullptr ;
  fhPiRing_nHits_vs_p= nullptr ;
  fhMuRing_Radius= nullptr ;
  fhMuRing_nHits= nullptr ;
  fhMuRing_nHits_vs_p= nullptr ;
  fhElecRing_Radius= nullptr ;
  fhElecRing_nHits= nullptr ;
  fhElecRing_nHits_vs_p= nullptr ;

  fhSingleRing_p_vs_RICHMass= nullptr ;
  fhPiRing_p_vs_RICHMass= nullptr ;
  fhMuRing_p_vs_RICHMass= nullptr ;
  fhElecRing_p_vs_RICHMass= nullptr ;

  fhSingleRing_Radius_vs_p= nullptr ;
  fhPiRing_Radius_vs_p= nullptr ;
  fhMuRing_Radius_vs_p= nullptr ;
  fhElecRing_Radius_vs_p= nullptr ;


  AddParam("TriggerMask", &fTriggerMask, 0xFF);
  AddParam("MaxNBursts",  &fMaxNBursts,  5000); // max number of bins in histograms

}

RICHMonitor::~RICHMonitor() {
}

void RICHMonitor::InitHist() {

  fDigiTimeRawFineVsROChannel = static_cast<TH2F*>(RequestHistogram("RICHMonitor", "DigiTimeRawFineVsROChannel", true));
  BookHisto("hTimeRawJUP_PerBurst", new TH1F("TimeRawJUP_PerBurst", "Raw Time JUP ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hTimeRawJDW_PerBurst", new TH1F("TimeRawJDW_PerBurst", "Raw Time JDW ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hTimeRawSUP_PerBurst", new TH1F("TimeRawSUP_PerBurst", "Raw Time SUP ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hTimeRawSDW_PerBurst", new TH1F("TimeRawSDW_PerBurst", "Raw Time SDW ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hTimeRawMUL_PerBurst", new TH1F("TimeRawMUL_PerBurst", "Raw Time MUL ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

  BookHisto("hSigmaTimeRawJUP_PerBurst", new TH1F("SigmaTimeRawJUP_PerBurst", "Raw Time Sigma JUP ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hSigmaTimeRawJDW_PerBurst", new TH1F("SigmaTimeRawJDW_PerBurst", "Raw Time Sigma JDW ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hSigmaTimeRawSUP_PerBurst", new TH1F("SigmaTimeRawSUP_PerBurst", "Raw Time Sigma SUP ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hSigmaTimeRawSDW_PerBurst", new TH1F("SigmaTimeRawSDW_PerBurst", "Raw Time Sigma SDW ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  BookHisto("hSigmaTimeRawMUL_PerBurst", new TH1F("SigmaTimeRawMUL_PerBurst", "Raw Time Sigma MUL ;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

  fReadingData = GetIsTree();
  if (fReadingData) {
    fRecoHitTimeWrtReferenceVsReadoutChannelNoT0 = static_cast<TH2F*>(RequestHistogram("RICHMonitor", "RecoHitTimeWrtReferenceVsReadoutChannelNoT0", true));
    fRecoHitTimeWrtReferenceVsReadoutChannel = static_cast<TH2F*>(RequestHistogram("RICHMonitor", "RecoHitTimeWrtReferenceVsReadoutChannel", true));
    fRecoHitTimeWrtReferenceVsSeqChannelNoT0 =  static_cast<TH2F*>(RequestHistogram("RICHMonitor", "RecoHitTimeWrtReferenceVsSeqChannelNoT0", true));
    fRecoHitTimeWrtReferenceVsSeqChannel = static_cast<TH2F*>(RequestHistogram("RICHMonitor", "RecoHitTimeWrtReferenceVsSeqChannel", true));
    BookHisto("hTotalEventsPerBurst", new
    	      TH1F("TotalEventsPerBurst", "Total events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPhysicsEventsPerBurst", new
    	      TH1F("PhysicsEventsPerBurst", "Physics events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hControlEventsPerBurst", new
	      TH1F("ControlEventsPerBurst", "Control events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));

    BookHisto("hNSCHitsPerBurst", new
    	      TH1F("NSCHitsPerBurst", "N SC Hits per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMHitsPerBurst", new
    	      TH1F("NPMHitsPerBurst", "N PM Hits per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNSCHitsInTimePerBurst", new
    	      TH1F("NSCHitsInTimePerBurst", "N SC Hits in time per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMHitsInTimePerBurst", new
    	      TH1F("NPMHitsInTimePerBurst", "N PM Hits in time per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNSCHitsPerBurst_phys", new
    	      TH1F("NSCHitsPerBurst_phys", "N SC Hits per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMHitsPerBurst_phys", new
    	      TH1F("NPMHitsPerBurst_phys", "N PM Hits per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNSCHitsInTimePerBurst_phys", new
    	      TH1F("NSCHitsInTimePerBurst_phys", "N SC Hits in time per burst (phys);Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMHitsInTimePerBurst_phys", new
    	      TH1F("NPMHitsInTimePerBurst_phys", "N PM Hits in time per burst (phys);Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMTimeCandPerBurst_phys", new
    	      TH1F("NPMTimeCandPerBurst_phys", "N PM TimeCand per burst (phys);Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMTimeCandInTimePerBurst_phys", new
    	      TH1F("NPMTimeCandInTimePerBurst_phys", "N PM in time TimeCand per burst (phys);Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPhysEvWith0SCHitsPerBurst", new
    	      TH1F("PhysEvWith0SCHitsPerBurst", "N Phys Events with 0 SChits per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPhysEvWith0SCHitsPerBurst_EvQual0", new
              TH1F("PhysEvWith0SCHitsPerBurst_EvQual0", "N Qual Phys Events with 0 SChits per burst;Burst ID",
                   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPhysEvWith0SCHitsPerBurst_EvQualNot0", new
              TH1F("PhysEvWith0SCHitsPerBurst_EvQualNot0", "N NotQual Phys Events with 0 SChits per burst;Burst ID",
                   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPMHitQualityBadBurst", new
	      TH1F("PMHitQualityBadBurst", "PM Hit with missing edge;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hSCHitQualityBadBurst", new
	      TH1F("SCHitQualityBadBurst", "SC Hit with edge problem;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMTimeCandPerBurst", new
    	      TH1F("NPMTimeCandPerBurst", "N PM TimeCand per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPMTimeCandInTimePerBurst", new
    	      TH1F("NPMTimeCandInTimePerBurst", "N PM in time TimeCand per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNElecRing_vs_BurstID", new
    	      TH1F("NElecRing_vs_BurstID", "number of e^{+} rings per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPiRing_vs_BurstID", new
    	      TH1F("NPiRing_vs_BurstID", "number of #pi^{+} rings per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNMuRing_vs_BurstID", new
    	      TH1F("NMuRing_vs_BurstID", "number of #mu^{+} rings per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNElecTrack_vs_BurstID", new
    	      TH1F("NElecTrack_vs_BurstID", "number of e^{+} tracks per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNPiTrack_vs_BurstID", new
    	      TH1F("NPiTrack_vs_BurstID", "number of #pi^{+} tracks per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hNMuTrack_vs_BurstID", new
    	      TH1F("NMuTrack_vs_BurstID", "number of #mu^{+} tracks per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));

    // event quantities:

    // physics trigger
    BookHisto("hPMHitQuality_phys", new TH1F("PMHitQuality_phys","PMHitQuality_phys", 4, 0., 4));
    BookHisto("hSCHitQuality_phys", new TH1F("SCHitQuality_phys","SCHitQuality_phys", 4, 0., 4));
    BookHisto("hNRICHEventPMHits_phys", new TH1F("NRICHEventPMHits_phys","NRICHEventPMHits_phys", 50, 0., 50));
    BookHisto("hNRICHEventSCHits_phys", new TH1F("NRICHEventSCHits_phys","NRICHEventSCHits_phys", 50, 0., 50));
    BookHisto("hNRICHSingleRingCand_phys", new TH1F("NRICHSingleRingCand_phys","NRICHSingleRingCand_phys", 50, 0., 50));
    BookHisto("hNRICHTimeCand_phys", new TH1F("NRICHTimeCand_phys","NRICHTimeCand_phys", 50, 0., 50));
    BookHisto("hNRICHRingCand_phys", new TH1F("NRICHRingCand_phys","NRICHRingCand_phys", 50, 0., 50));


    // control trigger
    BookHisto("hPMHitQuality", new TH1F("PMHitQuality","PMHitQuality", 4, 0., 4));
    BookHisto("hSCHitQuality", new TH1F("SCHitQuality","SCHitQuality", 4, 0., 4));
    BookHisto("hNRICHEventPMHits", new TH1F("NRICHEventPMHits","NRICHEventPMHits", 50, 0., 50));
    BookHisto("hNRICHEventSCHits", new TH1F("NRICHEventSCHits","NRICHEventSCHits", 50, 0., 50));
    BookHisto("hNRICHSingleRingCand", new TH1F("NRICHSingleRingCand","NRICHSingleRingCand", 50, 0., 50));
    BookHisto("hNRICHTimeCand", new TH1F("NRICHTimeCand","NRICHTimeCand", 50, 0., 50));
    BookHisto("hNRICHRingCand", new TH1F("NRICHRingCand","NRICHRingCand", 50, 0., 50));


    //------------------  Time resolution ----------------------------
    BookHisto("hRICHTimeCandNhits", new TH1F("RICHTimeCandNhits","RICHTimeCandNhits", 50, 0., 50));
    BookHisto("hTimeCandTime", new TH1F("TimeCandTime","TimeCandTime", 200, -100, 100));
    BookHisto("hRingCandTime", new TH1F("RingCandTime","RingCandTime", 200, -100, 100));
    BookHisto("hDeltaT_t1t2",new TH1F("DeltaT_t1t2","DeltaT_t1t2", 200, -2, 2));
    BookHisto("hDeltaT_RICHCand_CedarCand",new TH1F("DeltaT_RICHCand_CedarCand","DeltaT_RICHCand_CedarCand", 1000, -5, 5));
    BookHisto("hDeltaT_CedarCand_RICHHit",new TH1F("DeltaT_CedarCand_RICHHit","DeltaT_CedarCand_RICHHit", 1000, -5, 5));
    BookHisto("hrange_DeltaT_RICHCand_CedarCand",new TH1F("range_DeltaT_RICHCand_CedarCand","range_DeltaT_RICHCand_CedarCand", 1000, -50, 50));
    BookHisto("hrange_DeltaT_CedarCand_RICHHit",new TH1F("range_DeltaT_CedarCand_RICHHit","range_DeltaT_CedarCand_RICHHit",1000, -50, 50));

    BookHisto("hDeltaT_RICHHitRichCand_vs_SeqID",new TH2F("DeltaT_RICHHitRichCand_vs_SeqID","DeltaT_RICHHitRichCand_vs_SeqID", 2000,0,2000,1000,-5,5));
    BookHisto("hDeltaT_RICHHitCedarCand_vs_SeqID",new TH2F("DeltaT_RICHHitCedarCand_vs_SeqID","DeltaT_RICHHitCedarCand_vs_SeqID", 2000,0,2000,1000,-5,5));
    //--------------------------------------------------------------

    BookHisto("hPAllTracks", new TH1F("PAllTracks","PAllTracks", 95, 0., 95));
    BookHisto("hPSelTracks", new TH1F("PSelTracks","PSelTracks", 95, 0., 95));
    BookHisto("hPAccTracks", new TH1F("PAccTracks","PAccTracks", 95, 0., 95));
    BookHisto("hPEvtSelTracks", new TH1F("PEvtSelTracks","PEvtSelTracks", 95, 0., 95));
    BookHisto("hEOP",
	      new TH1F("EOP", "Track E/p; E/p", 150, 0.0, 1.5));
    BookHisto("hPAllMuTracks", new TH1F("PAllMuTracks","PAllMuTracks", 95, 0., 95.));
    BookHisto("hPAllPiTracks", new TH1F("PAllPiTracks","PAllPiTracks", 95, 0., 95.));
    BookHisto("hPAllElecTracks", new TH1F("PAllElecTracks","PAllElecTracks", 95, 0., 95.));
    BookHisto("hPMuTracks", new TH1F("PMuTracks","PMuTracks", 95, 0., 95.));
    BookHisto("hPPiTracks", new TH1F("PPiTracks","PPiTracks", 95, 0., 95.));
    BookHisto("hPElecTracks", new TH1F("PElecTracks","PElecTracks", 95, 0., 95.));
    BookHisto("hSingleRing_Radius",new TH1F("SingleRing_Radius","SingleRing_Radius", 2500, 0.,250.));
    BookHisto("hSingleRing_nHits", new TH1F("SingleRing_nHits","SingleRing_nHits", 50, 0.,50.));
    BookHisto("hSingleRing_nHits_vs_p",new TH2F("SingleRing_nHits_vs_p","SingleRing_nHits_vs_p",180, 0,90,50,0,50));
    BookHisto("hSingleRing_MissMass2_vs_EoP",new TH2F("SingleRing_MissMass2_vs_EoP","SingleRing_MissMass2_vs_EoP",120,0,1.2,30,-0.15,0.15));
    BookHisto("hSingleRing_RICHMass",new TH1F("SingleRing_RICHMass","SingleRing_RICHMass", 200, 0.,0.2));

    BookHisto("hSingleRing_Dist",new TH1F("SingleRing_Dist","SingleRing_Dist", 100, 0.,50.));
    BookHisto("hSingleRing_DTime",new TH1F("SingleRing_DTime","SingleRing_DTime", 100, -50.,50.));
    BookHisto("hSingleRing_DTime_vs_Dist",new TH2F("SingleRing_DTime_vs_Dist","SingleRing_DTime_vs_Dist", 100, 0., 50., 100, -50.,50.));
    BookHisto("hElecRing_Radius",new TH1F("ElecRing_Radius","ElecRing_Radius", 2500, 0.,250.));
    BookHisto("hElecRing_nHits",new TH1F("ElecRing_nHits","ElecRing_nHits", 50, 0.,50.));
    BookHisto("hElecRing_nHits_vs_p",new TH2F("ElecRing_nHits_vs_p","ElecRing_nHits_vs_p",180, 0,90,50,0,50));
    BookHisto("hElecRing_MissMass2_vs_EoP",new TH2F("ElecRing_MissMass2_vs_EoP","ElecRing_MissMass2_vs_EoP",120,0,1.2,30,-0.15,0.15));
    BookHisto("hElecRing_RICHMass",new TH1F("ElecRing_RICHMass","ElecRing_RICHMass", 200, 0.,0.2));


    BookHisto("hPiRing_Radius",new TH1F("PiRing_Radius","PiRing_Radius", 2500, 0.,250.));
    BookHisto("hPiRing_nHits",new TH1F("PiRing_nHits","PiRing_nHits", 50, 0.,50.));
    BookHisto("hPiRing_nHits_vs_p",new TH2F("PiRing_nHits_vs_p","PiRing_nHits_vs_p",180, 0,90,50,0,50));
    BookHisto("hPiRing_MissMass2_vs_EoP",new TH2F("PiRing_MissMass2_vs_EoP","PiRing_MissMass2_vs_EoP",120,0,1.2,30,-0.15,0.15));
    BookHisto("hPiRing_RICHMass",new TH1F("PiRing_RICHMass","PiRing_RICHMass", 200, 0.,0.2));

    BookHisto("hMuRing_Radius",new TH1F("MuRing_Radius","MuRing_Radius", 2500, 0.,250.));
    BookHisto("hMuRing_nHits",new TH1F("MuRing_nHits","MuRing_nHits", 50, 0.,50.));
    BookHisto("hMuRing_nHits_vs_p",new TH2F("MuRing_nHits_vs_p","MuRing_nHits_vs_p",180, 0,90,50,0,50));
    BookHisto("hMuRing_MissMass2_vs_EoP",new TH2F("MuRing_MissMass2_vs_EoP","MuRing_MissMass2_vs_EoP",120,0,1.2,30,-0.15,0.15));
    BookHisto("hMuRing_RICHMass",new TH1F("MuRing_RICHMass","MuRing_RICHMass", 200, 0.,0.2));

    BookHisto("hSingleRing_p_vs_RICHMass",new TH2F("SingleRing_p_vs_RICHMass","SingleRing_p_vs_RICHMass", 200, 0.,0.2,95,0,95));
    BookHisto("hElecRing_p_vs_RICHMass",new TH2F("ElecRing_p_vs_RICHMass","ElecRing_p_vs_RICHMass", 200, 0.,0.2,95,0,95));
    BookHisto("hPiRing_p_vs_RICHMass",new TH2F("PiRing_p_vs_RICHMass","PiRing_p_vs_RICHMass", 200, 0.,0.2,95,0,95));
    BookHisto("hMuRing_p_vs_RICHMass",new TH2F("MuRing_p_vs_RICHMass","MuRing_p_vs_RICHMass", 200, 0.,0.2,95,0,95));
    BookHisto("hSingleRing_Radius_vs_p",new TH2F("SingleRing_Radius_vs_p","SingleRing_Radius_vs_p", 95,0,95,300, 0.,300.));
    BookHisto("hElecRing_Radius_vs_p",new TH2F("ElecRing_Radius_vs_p","ElecRing_Radius_vs_p", 95,0,95,300, 0.,300.));
    BookHisto("hPiRing_Radius_vs_p",new TH2F("PiRing_Radius_vs_p","PiRing_Radius_vs_p", 95,0,95,300, 0.,300.));
    BookHisto("hMuRing_Radius_vs_p",new TH2F("MuRing_Radius_vs_p","MuRing_Radius_vs_p", 95,0,95,300, 0.,300.));

  }else {  // step2 - not reading data


    fHTimeRawJUP_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeRawJUP_PerBurst",      true));
    fHTimeRawJDW_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeRawJDW_PerBurst",      true));
    fHTimeRawSUP_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeRawSUP_PerBurst",      true));
    fHTimeRawSDW_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeRawSDW_PerBurst",      true));
    fHTimeRawMUL_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeRawMUL_PerBurst",      true));
    fHSigmaTimeRawJUP_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SigmaTimeRawJUP_PerBurst",      true));
    fHSigmaTimeRawJDW_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SigmaTimeRawJDW_PerBurst",      true));
    fHSigmaTimeRawSUP_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SigmaTimeRawSUP_PerBurst",      true));
    fHSigmaTimeRawSDW_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SigmaTimeRawSDW_PerBurst",      true));
    fHSigmaTimeRawMUL_PerBurst                     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SigmaTimeRawMUL_PerBurst",      true));
    fHTotalEventsPerBurst            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TotalEventsPerBurst",      true));
    fHPhysicsEventsPerBurst          = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PhysicsEventsPerBurst",    true));
    fHControlEventsPerBurst          = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ControlEventsPerBurst",    true));
    fhNElecRing_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NElecRing_vs_BurstID",      true));
    fhNPiRing_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPiRing_vs_BurstID",      true));
    fhNMuRing_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NMuRing_vs_BurstID",      true));
    fhNElecTrack_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NElecTrack_vs_BurstID",      true));
    fhNPiTrack_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPiTrack_vs_BurstID",      true));
    fhNMuTrack_vs_BurstID            = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NMuTrack_vs_BurstID",      true));
    fhNSCHitsPerBurst    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NSCHitsPerBurst",      true));
    fhNPMHitsPerBurst    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMHitsPerBurst",      true));
    fhNSCHitsPerBurst_phys    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NSCHitsPerBurst_phys",      true));
    fhNPMHitsPerBurst_phys    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMHitsPerBurst_phys",      true));
    fhNPMTimeCandPerBurst   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMTimeCandPerBurst",      true));
    fhNSCHitsInTimePerBurst    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NSCHitsInTimePerBurst",      true));
    fhNPMHitsInTimePerBurst    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMHitsInTimePerBurst",      true));
    fhNSCHitsInTimePerBurst_phys    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NSCHitsInTimePerBurst_phys",      true));
    fhNPMHitsInTimePerBurst_phys    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMHitsInTimePerBurst_phys",      true));
    fhNPMTimeCandInTimePerBurst   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMTimeCandInTimePerBurst",      true));
    fhNPMTimeCandPerBurst_phys   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMTimeCandPerBurst_phys",      true));
    fhNPMTimeCandInTimePerBurst_phys   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NPMTimeCandInTimePerBurst_phys",      true));
    fhPMHitQualityBadBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"PMHitQualityBadBurst",      true));
    fhSCHitQualityBadBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"SCHitQualityBadBurst",      true));
    fhPhysEvWith0SCHitsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"PhysEvWith0SCHitsPerBurst",      true));
    fhPhysEvWith0SCHitsPerBurst_EvQual0 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"PhysEvWith0SCHitsPerBurst_EvQual0",      true));
    fhPhysEvWith0SCHitsPerBurst_EvQualNot0 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"PhysEvWith0SCHitsPerBurst_EvQualNot0",      true));


    //----------------------------------------------------------------------------
    fhNRICHEventPMHits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHEventPMHits",      true));
    fhNRICHEventSCHits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHEventSCHits",      true));
    fhNRICHEventPMHits_phys = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHEventPMHits_phys",      true));
    fhNRICHEventSCHits_phys = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHEventSCHits_phys",      true));
    fhPMHitQuality = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PMHitQuality",      true));
    fhSCHitQuality = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SCHitQuality",      true));
    fhNRICHTimeCand = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHTimeCand",      true));
    fhNRICHRingCand = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHRingCand",      true));
    fhNRICHSingleRingCand = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NRICHSingleRingCand",      true));
    fhRICHTimeCandNhits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "RICHTimeCandNhits",      true));
    fhTimeCandTime = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TimeCandTime",      true));
    fhRingCandTime = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "RingCandTime",      true));
    fhDeltaT_t1t2 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "DeltaT_t1t2",      true));
    fhDeltaT_RICHCand_CedarCand = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "DeltaT_RICHCand_CedarCand",      true));
    fhDeltaT_CedarCand_RICHHit = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "DeltaT_CedarCand_RICHHit",      true));
    fhDeltaT_RICHHitRichCand_vs_SeqID = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "DeltaT_RICHHitRichCand_vs_SeqID",      true));
    fhDeltaT_RICHHitCedarCand_vs_SeqID = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "DeltaT_RICHHitCedarCand_vs_SeqID",      true));
    fhSingleRing_Dist = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SingleRing_Dist",      true));
    fhPEvtSelTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PEvtSelTracks",      true));
    fhSingleRing_RICHMass= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "SingleRing_RICHMass",      true));
    fhPAllPiTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PAllPiTracks",      true));
    fhPPiTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PPiTracks",      true));
    fhPAllMuTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PAllMuTracks",      true));
    fhPMuTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PMuTracks",      true));
    fhPAllElecTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PAllElecTracks",      true));
    fhPElecTracks= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PElecTracks",      true));
    fhMuRing_RICHMass = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MuRing_RICHMass",      true));
    fhPiRing_RICHMass = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PiRing_RICHMass",      true));
    fhElecRing_RICHMass = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ElecRing_RICHMass",      true));
    fhPiRing_Radius = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PiRing_Radius",      true));
    fhPiRing_nHits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PiRing_nHits",      true));
    fhPiRing_nHits_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "PiRing_nHits_vs_p",      true));
    fhMuRing_Radius = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MuRing_Radius",      true));
    fhMuRing_nHits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MuRing_nHits",      true));
    fhMuRing_nHits_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MuRing_nHits_vs_p",      true));
    fhElecRing_Radius= static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ElecRing_Radius",      true));
    fhElecRing_nHits = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ElecRing_nHits",      true));
    fhElecRing_nHits_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "ElecRing_nHits_vs_p",      true));
    fhSingleRing_p_vs_RICHMass = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "SingleRing_p_vs_RICHMass",      true));
    fhPiRing_p_vs_RICHMass = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "PiRing_p_vs_RICHMass",      true));
    fhMuRing_p_vs_RICHMass = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MuRing_p_vs_RICHMass",      true));
    fhElecRing_p_vs_RICHMass = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "ElecRing_p_vs_RICHMass",      true));
    fhSingleRing_Radius_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "SingleRing_Radius_vs_p",      true));
    fhPiRing_Radius_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "PiRing_Radius_vs_p",      true));
    fhMuRing_Radius_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MuRing_Radius_vs_p",      true));
    fhElecRing_Radius_vs_p = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "ElecRing_Radius_vs_p",      true));
  }
}

void RICHMonitor::Process(Int_t) {
  if (!fReadingData) return; // no action if reading its own output in --histo mode
  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t RunNumber = GetEventHeader()->GetRunID();
  Int_t BurstID   = GetEventHeader()->GetBurstID();
  Bool_t ControlData   = L0DataType    & 0x10;
  Bool_t PhysicsData   = L0DataType    & 0x1;
  Bool_t PeriodicData   = L0DataType    & 0x2;
  FillHisto("hTotalEventsPerBurst", BurstID);
  if (PhysicsData) FillHisto("hPhysicsEventsPerBurst", BurstID);
  if (ControlData) FillHisto("hControlEventsPerBurst", BurstID);
  Double_t RefTime = GetEventHeader()->GetFineTime() * TdcCalib;
  TRecoCedarEvent*  CEDARevent  = GetEvent<TRecoCedarEvent>();
  TRecoRICHEvent*   RICHevent    = GetEvent<TRecoRICHEvent>();
  TRecoCHODEvent*   CHODevent   = GetEvent<TRecoCHODEvent>();

  if(PeriodicData)return;
  if (PhysicsData){
    Int_t NHits = RICHevent->GetNHits();
    Int_t NSCHits=0;
    Int_t NPMHits=0;
    for(int iHit=0; iHit< NHits; iHit++){
      TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>(RICHevent->GetHit(iHit));
      if(Hit->GetOrSuperCellID()){
	FillHisto("hNSCHitsPerBurst_phys",BurstID);
	if(fabs(Hit->GetTime() - RefTime)<10.0)FillHisto("hNSCHitsInTimePerBurst_phys",BurstID);
	NSCHits++;
      }
      else{
       	FillHisto("hNPMHitsPerBurst_phys",BurstID);
	if(fabs(Hit->GetTime() - RefTime)<10.0)FillHisto("hNPMHitsInTimePerBurst_phys",BurstID);
	NPMHits++;
      }
    }
    FillHisto("hNRICHEventSCHits_phys",NSCHits);
    FillHisto("hNRICHEventPMHits_phys",NPMHits);
    FillHisto("hNRICHTimeCand_phys",RICHevent->GetNPMTimeCandidates());
    FillHisto("hNRICHRingCand_phys",RICHevent->GetNRingCandidates());
    // if(NSCHits==0)cout <<" eventID " << iEvent <<" Physics trigger but   NSCHits="<<  NSCHits <<endl;
    //if(NPMHits==0)cout <<" eventID " << iEvent<<" Physics trigger but   NPMHits= "<<  NPMHits <<endl;
    if(NSCHits==0){
      FillHisto("hPhysEvWith0SCHitsPerBurst",BurstID);
      if(GetEventHeader()->GetEventQualityMask()==0)FillHisto("hPhysEvWith0SCHitsPerBurst_EvQual0",BurstID);
      if(GetEventHeader()->GetEventQualityMask()!=0)FillHisto("hPhysEvWith0SCHitsPerBurst_EvQualNot0",BurstID);
    }
  }

  for(int iRICHCand=0; iRICHCand<RICHevent->GetNPMTimeCandidates(); iRICHCand++){ //loop over PM Time Cand
    TRecoRICHCandidate *RICHCandidate = RICHevent->GetPMTimeCandidate(iRICHCand);
    RICHCandidate->SetEvent(RICHevent);
    if(PhysicsData){
      FillHisto("hNPMTimeCandPerBurst_phys",BurstID);
      if(fabs(RICHCandidate->GetTime() - RefTime)<10.0)FillHisto("hNPMTimeCandInTimePerBurst_phys",BurstID);
    }
    if(ControlData){
      FillHisto("hNPMTimeCandPerBurst",BurstID);
      if(fabs(RICHCandidate->GetTime() - RefTime)<10.0)FillHisto("hNPMTimeCandInTimePerBurst",BurstID);
    }
  }
  if(RunNumber>4147){
    if (!ControlData)return; //==================!!!!!!!!!!!!! only control data (no control in 2015)
  }
  Int_t NHits = RICHevent->GetNHits();
  Int_t NSCHits=0;
  Int_t NPMHits=0;
  for(int iHit=0; iHit< NHits; iHit++){
    TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>(RICHevent->GetHit(iHit));
    if(Hit->GetOrSuperCellID()){
      FillHisto("hSCHitQuality",Hit->GetHitQuality());
      FillHisto("hNSCHitsPerBurst",BurstID);
      if(fabs(Hit->GetTime() - RefTime)<10.0)FillHisto("hNSCHitsInTimePerBurst",BurstID);
      if(Hit->GetHitQuality()!=1)FillHisto("hSCHitQualityBadBurst",BurstID);
      NSCHits++;
    }
    else{
      FillHisto("hPMHitQuality",Hit->GetHitQuality());
      FillHisto("hNPMHitsPerBurst",BurstID);
      if(fabs(Hit->GetTime() - RefTime)<10.0)FillHisto("hNPMHitsInTimePerBurst",BurstID);
      if(Hit->GetHitQuality()!=3)  FillHisto("hPMHitQualityBadBurst",BurstID);
      NPMHits++;
    }
  }
  FillHisto("hNRICHEventSCHits",NSCHits);
  FillHisto("hNRICHEventPMHits",NPMHits);
  FillHisto("hNRICHTimeCand",RICHevent->GetNPMTimeCandidates());
  FillHisto("hNRICHRingCand",RICHevent->GetNRingCandidates());

  ////////////////////////////////////////////////
  // Require a good Cedar candidates (>=5 sectors)
  Int_t NCEDARcand = CEDARevent->GetNCandidates();
  Int_t NGoodCedarCand = 0;
  for (Int_t i=0; i<NCEDARcand; i++) {
    TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    if (Ccand->GetNSectors()>4) NGoodCedarCand++;
  }
  // from run 6761 to run 6821 KTAG was empty
  // from run 6560 KTAG4 was out
  //if(RunNumber < 6899  || RunNumber> 6905){   not A+ ???
  //  if (!NGoodCedarCand) return;
  // }

  Double_t ClosestCedarTime = -999;
  Int_t NSingleRingCand = 0;
  for(int iRICHCand=0; iRICHCand<RICHevent->GetNPMTimeCandidates(); iRICHCand++){ //loop over PM Time Cand
    TRecoRICHCandidate *RICHCandidate = RICHevent->GetPMTimeCandidate(iRICHCand);
    RICHCandidate->SetEvent(RICHevent);
    FillHisto("hTimeCandTime",RICHCandidate->GetTime());
    FillHisto("hRICHTimeCandNhits",RICHCandidate->GetNHits());
    Double_t minDeltaT = 999;
    TRecoCedarCandidate *CedarCandidate;
    for(int icc=0; icc<CEDARevent->GetNCandidates(); icc++){ //loop cedar Cand
      CedarCandidate = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(icc));
      CedarCandidate->SetEvent(CEDARevent);
      if(CedarCandidate->GetNSectors()<4)continue;
      FillHisto("hDeltaT_RICHCand_CedarCand",RICHCandidate->GetTime()-CedarCandidate->GetTime());
      Double_t deltaT = fabs(RICHCandidate->GetTime()-CedarCandidate->GetTime());
      if(deltaT<minDeltaT){
        minDeltaT= deltaT;
        ClosestCedarTime = CedarCandidate->GetTime();
      }
    }// end loop over Cedar candidates
    if(ClosestCedarTime!=-999){
      FillHisto("hDeltaT_RICHCand_CedarCand",RICHCandidate->GetTime()-ClosestCedarTime);
      FillHisto("hrange_DeltaT_RICHCand_CedarCand",RICHCandidate->GetTime()-ClosestCedarTime);
    }
    float t1=0;
    float t2=0;
    int i1=0;
    int i2=0;
    for(int jch=0; jch<RICHCandidate->GetNHits(); jch++){
      TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>(RICHCandidate->GetHit(jch));
      FillHisto("hDeltaT_CedarCand_RICHHit",Hit->GetTime()-ClosestCedarTime);
      FillHisto("hrange_DeltaT_CedarCand_RICHHit",Hit->GetTime()-ClosestCedarTime);
      Int_t SeqID  = Hit->GetChannelSeqID();
      FillHisto("hDeltaT_RICHHitRichCand_vs_SeqID",SeqID,Hit->GetTime()-RICHCandidate->GetTime());
      FillHisto("hDeltaT_RICHHitCedarCand_vs_SeqID",SeqID,Hit->GetTime()-ClosestCedarTime);
      if(jch< RICHCandidate->GetNHits()/2){
        t1=t1+Hit->GetTime();
        i1++;
      }else{
        t2=t2+Hit->GetTime();
        i2++;
      }
    }
    if(i1>0 && i2>0){
      t1 = t1/int(RICHCandidate->GetNHits()/2);
      t2 = t2/(RICHCandidate->GetNHits()-int(RICHCandidate->GetNHits()/2));
      FillHisto("hDeltaT_t1t2",t1-t2);
    }else{
      t1 = -99;
      t2 = -99;
    }
    if(RICHCandidate->GetRingChi2SingleRing()<3)NSingleRingCand++;
  } // end loop over RICH Ring candidates

  FillHisto("hNRICHSingleRingCand",NSingleRingCand);

  //////////////////////////////////////////
  // Require exactly one track in acceptance

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (Tracks.size()!=1) return;
  Int_t itrack=0;
  TRecoSpectrometerCandidate* Scand = Tracks[itrack].GetSpectrometerCandidate();
  Int_t    Q            = Tracks[itrack].GetCharge();
  Double_t Ptrack       = Tracks[itrack].GetMomentum(); // spectrometer calibration included
  Double_t Ptrackbefore = Tracks[itrack].GetMomentumBeforeFit();
  Double_t Ttrack       =
    (Tracks[itrack].CHODAssociationExists()) ? Tracks[itrack].GetCHODTime() : Tracks[itrack].GetTrackTime();
  Double_t Chi2track    = Tracks[itrack].GetChi2();
  FillHisto("hPAllTracks",0.001*Ptrack);

  if (Q!=1) return;
  if (Chi2track>20.0) return;
  if (Scand->GetNChambers()!=4) return;
  if (fabs(Ptrack-Ptrackbefore)>20000.0) return; // 20 GeV
  FillHisto("hPSelTracks",0.001*Ptrack);
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD))      return   ;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kMUV3))            return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kCHOD))            return;

  FillHisto("hPAccTracks",0.001*Ptrack);
  if (!Tracks[itrack].LKrAssociationExists()) return;
  Double_t eop = Tracks[itrack].GetLKrEoP();
  FillHisto("hEOP", eop);

  TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  fCDAcomp->SetLine1Point1(0.0, 0.0, 102000.0);
  fCDAcomp->SetDir1(KaonThreeMomentum);
  fCDAcomp->SetLine2Point1(Scand->GetPositionBeforeMagnet());
  fCDAcomp->SetDir2(Scand->GetThreeMomentumBeforeMagnet());
  fCDAcomp->ComputeVertexCDA();
  Double_t cda  = fCDAcomp->GetCDA();
  Double_t Zvtx = fCDAcomp->GetVertex().z();
  Bool_t pas_zvtx = (Zvtx>110000 && Zvtx<165000);
  Bool_t pas_cda  = (cda<40);
  //--------------------------------------------
  if (!pas_zvtx) return;
  if (!pas_cda) return;
  TLorentzVector Kaon;
  Kaon.SetVectM(KaonThreeMomentum, MKCH);
  TLorentzVector Pion;
  Pion.SetVectM(Scand->GetThreeMomentumBeforeMagnet(), MPI);
  Double_t Mmiss2Pi = (Kaon-Pion).M2();
  Ptrack =0.001*Ptrack;

  FillHisto("hPEvtSelTracks",Ptrack);
  std::vector<SpectrometerMUV3AssociationOutput> SpecMUV3 =
    *(std::vector<SpectrometerMUV3AssociationOutput>*)GetOutput("SpectrometerMUV3Association.Output");
  if(eop <1.03 && eop > 0.96 && Mmiss2Pi*1e-6 > 0.04  && SpecMUV3[0].GetNAssociationRecords()< 1){
    if(CHODevent->GetNCandidates()==1){
      FillHisto("hPAllElecTracks",Ptrack);
      if(Ptrack>15 && Ptrack<35){
	FillHisto("hNElecTrack_vs_BurstID",BurstID);
      }
    }
  }else if(eop <0.88 && eop >0.1 && Mmiss2Pi*1e-6 > 0.01 && Mmiss2Pi*1e-6 <0.04 && SpecMUV3[0].GetNAssociationRecords() <1){
    FillHisto("hPAllPiTracks",Ptrack);
    if(Ptrack>15 && Ptrack<35){
      FillHisto("hNPiTrack_vs_BurstID",BurstID);
    }
  }else if((eop<0.05) && (Mmiss2Pi*1e-6 < 0.005)   && SpecMUV3[0].GetNAssociationRecords()>0){
    FillHisto("hPAllMuTracks",Ptrack);
    if(Ptrack>15 && Ptrack<35){
      FillHisto("hNMuTrack_vs_BurstID",BurstID);
    }
  }
  Double_t RingRadius;
  //Double_t RingXc = -9999;
  //Double_t RingYc = -9999;
  //Double_t RingChi2 = -99;
  Double_t RICHMass;
  Double_t RingTime;
  Int_t  SingleRingID = -1;
  //========== trk seeded ring reconstruction (at analysis level)===============
  //----------------------------------------------------------------------------
  SingleRingID = Tracks[itrack].GetRICHSingleRingTrkSeededCandidateID();
  //------------------------------------------------------------------------------
  //============================================================================
  if(SingleRingID>=0 ){
    //---------- from trk seeded ------------
    RingRadius = Tracks[itrack].GetRICHSingleRingTrkSeededRadius();
    RICHMass = Tracks[itrack].GetRICHSingleRingTrkSeededMass()*0.001;
    RingTime = Tracks[itrack].GetRICHSingleRingTrkSeededTime();
    Double_t dist = Tracks[itrack].GetRICHSingleRingTrkSeededTrkDist();
    Int_t NHits1 = Tracks[itrack].GetRICHSingleRingTrkSeededNHits();
    Double_t dTime =  RingTime -Ttrack;
    FillHisto("hSingleRing_Dist",   dist);
    FillHisto("hSingleRing_DTime",    dTime);
    FillHisto("hSingleRing_DTime_vs_Dist",dist,dTime);
    FillHisto("hSingleRing_Radius",RingRadius);
    FillHisto("hSingleRing_Radius_vs_p",Ptrack,RingRadius);
    FillHisto("hSingleRing_nHits",NHits1);
    Double_t  missMass2 = Mmiss2Pi*1e-6;
    if(dist>15)return;
    if(eop <1.03 && eop > 0.96 && Mmiss2Pi*1e-6 > 0.04  && SpecMUV3[0].GetNAssociationRecords()< 1){
      if(CHODevent->GetNCandidates()==1){
	FillHisto("hPElecTracks",Ptrack);
	FillHisto("hElecRing_Radius",RingRadius);
	FillHisto("hElecRing_nHits",NHits1);
	FillHisto("hElecRing_nHits_vs_p",Ptrack,NHits1);
	FillHisto("hElecRing_MissMass2_vs_EoP",eop,missMass2);
	FillHisto("hElecRing_Radius_vs_p",Ptrack, RingRadius);
	FillHisto("hElecRing_p_vs_RICHMass",RICHMass,Ptrack);
	if(Ptrack>15 && Ptrack<35){
	  FillHisto("hElecRing_RICHMass",RICHMass);
	  FillHisto("hNElecRing_vs_BurstID",BurstID);
	}
      }
    }else if(eop <0.88 && eop >0.1 && Mmiss2Pi*1e-6 > 0.01 && Mmiss2Pi*1e-6 <0.04 && SpecMUV3[0].GetNAssociationRecords() <1){
      FillHisto("hPPiTracks",Ptrack);
      FillHisto("hPiRing_Radius",RingRadius);
      FillHisto("hPiRing_nHits",NHits1);
      FillHisto("hPiRing_nHits_vs_p", Ptrack,NHits1);
      FillHisto("hPiRing_MissMass2_vs_EoP",eop,missMass2);
      FillHisto("hPiRing_Radius_vs_p",Ptrack,RingRadius);
      FillHisto("hPiRing_p_vs_RICHMass",RICHMass,Ptrack);
      if(Ptrack>15 && Ptrack<35){
	FillHisto("hPiRing_RICHMass",RICHMass);
	FillHisto("hNPiRing_vs_BurstID",BurstID);
      }
    }else if((eop<0.05) && (Mmiss2Pi*1e-6 < 0.005)   && SpecMUV3[0].GetNAssociationRecords()>0){
      FillHisto("hPMuTracks",Ptrack);
      FillHisto("hMuRing_Radius",RingRadius);
      FillHisto("hMuRing_nHits",NHits1);
      FillHisto("hMuRing_nHits_vs_p",Ptrack,NHits1);
      FillHisto("hMuRing_MissMass2_vs_EoP",eop,missMass2);
      FillHisto("hMuRing_Radius_vs_p",Ptrack, RingRadius);
      FillHisto("hMuRing_p_vs_RICHMass",RICHMass,Ptrack);
      if(Ptrack>15 && Ptrack<35){
	FillHisto("hMuRing_RICHMass",RICHMass);
	FillHisto("hNMuRing_vs_BurstID",BurstID);
      }
    }
  }
}



void RICHMonitor::StartOfBurstUser() {

}
void RICHMonitor::EndOfBurstUser() {

}


void RICHMonitor::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    TH1D *ROID_TEL62JUP =nullptr;
    TH1D *ROID_TEL62JDW =nullptr;
    TH1D *ROID_TEL62SUP =nullptr;
    TH1D *ROID_TEL62SDW =nullptr;
    TH1D *ROID_TEL62MUL =nullptr;
    Int_t BurstID = GetEventHeader()->GetBurstID();
    fRunNumber = GetRunID();
    if(fDigiTimeRawFineVsROChannel){
      fDigiTimeRawFineVsROChannel->Write();
      ROID_TEL62JUP =   fDigiTimeRawFineVsROChannel->ProjectionY("ROID_TEL62JUP",1,512);
      ROID_TEL62JDW  = fDigiTimeRawFineVsROChannel->ProjectionY("ROID_TEL62JDW",513,1024);
      ROID_TEL62SUP = fDigiTimeRawFineVsROChannel->ProjectionY("ROID_TEL62SUP",1025,1536);
      ROID_TEL62SDW = fDigiTimeRawFineVsROChannel->ProjectionY("ROID_TEL62SDW",1537,2048);
      ROID_TEL62MUL = fDigiTimeRawFineVsROChannel->ProjectionY("ROID_TEL62MUL",2049,2560);
      ROID_TEL62JUP->Write();
      ROID_TEL62JDW->Write();
      ROID_TEL62SUP->Write();
      ROID_TEL62SDW->Write();
      ROID_TEL62MUL->Write();
      Float_t ROID_TEL62_maxbin;
      Float_t ROID_TEL62_maxX;
      Float_t fitmax;
      Float_t fitmin;
      TF1 *fitTEL62 = new TF1("fitTEL62","gaus",-10,10);
      Int_t FineTimePrecision = 256;
      Double_t DigiTimeFineWidth = ClockPeriod/((Double_t)FineTimePrecision);
      Float_t TimeRawJUP = -99;
      Float_t TimeRawJDW = -99;
      Float_t TimeRawSUP = -99;
      Float_t TimeRawSDW = -99;
      Float_t TimeRawMUL = -99;

      Float_t SigmaTimeRawJUP = -99;
      Float_t SigmaTimeRawJDW = -99;
      Float_t SigmaTimeRawSUP = -99;
      Float_t SigmaTimeRawSDW = -99;
      Float_t SigmaTimeRawMUL = -99;
      ROID_TEL62_maxbin = ROID_TEL62JUP->GetMaximumBin();
      if(fRunNumber>7000){
        ROID_TEL62_maxX = ROID_TEL62_maxbin * DigiTimeFineWidth - 100.;
      }else{
        ROID_TEL62_maxX = (Double_t)ROID_TEL62_maxbin - 500.;
        if(ROID_TEL62_maxX==0)ROID_TEL62_maxX = ROID_TEL62_maxX+0.0001;
      }
      fitmax =  ROID_TEL62_maxX + 0.5;
      fitmin =  ROID_TEL62_maxX - 0.5;
      fitTEL62->SetParameter(0,200000);
      fitTEL62->SetParameter(1,ROID_TEL62_maxX );
      fitTEL62->SetParameter(2,0.2);
      TFitResultPtr res = ROID_TEL62JUP->Fit("fitTEL62","","",fitmin,fitmax);
      Int_t fitStatus = res;
      cout  << user_normal() << "  BurstID: "<<  BurstID<<"  RAW   JUP:   "<< fitTEL62->GetParameter(1) <<"  "<< fitTEL62->GetParameter(2)  <<endl;;
      if(fRunNumber>7000 && fitStatus==0){
	TimeRawJUP = fitTEL62->GetParameter(1);
	SigmaTimeRawJUP = fitTEL62->GetParameter(2);
	FillHisto("hTimeRawJUP_PerBurst",BurstID,TimeRawJUP);
	FillHisto("hSigmaTimeRawJUP_PerBurst",BurstID,SigmaTimeRawJUP);
      }else{
	TimeRawJUP = ROID_TEL62_maxX;
	SigmaTimeRawJUP = -1;
	FillHisto("hTimeRawJUP_PerBurst",BurstID,TimeRawJUP);
	FillHisto("hSigmaTimeRawJUP_PerBurst",BurstID,SigmaTimeRawJUP);
      }
      ROID_TEL62_maxbin = ROID_TEL62JDW->GetMaximumBin();
      if(fRunNumber>7000){
        ROID_TEL62_maxX = ROID_TEL62_maxbin * DigiTimeFineWidth - 100.;
      }else{
        ROID_TEL62_maxX = (Double_t)ROID_TEL62_maxbin - 500.;
        if(ROID_TEL62_maxX==0)ROID_TEL62_maxX = ROID_TEL62_maxX+0.0001;
      }
      fitmax =  ROID_TEL62_maxX + 0.5;
      fitmin =  ROID_TEL62_maxX - 0.5;
      fitTEL62->SetParameter(0,200000);
      fitTEL62->SetParameter(1,ROID_TEL62_maxX );
      fitTEL62->SetParameter(2,0.2);
      res = ROID_TEL62JDW->Fit("fitTEL62","","",fitmin,fitmax);
      fitStatus = res;
      cout  << user_normal() << "  BurstID: "<<  BurstID<<"  RAW   JDW:   "<< fitTEL62->GetParameter(1) <<"  "<< fitTEL62->GetParameter(2)  <<endl;;
      if(fRunNumber>7000 && fitStatus==0){
	TimeRawJDW = fitTEL62->GetParameter(1);
	SigmaTimeRawJDW = fitTEL62->GetParameter(2);
	FillHisto("hTimeRawJDW_PerBurst",BurstID,TimeRawJDW);
	FillHisto("hSigmaTimeRawJDW_PerBurst",BurstID,SigmaTimeRawJDW);
      }else{
	TimeRawJDW = ROID_TEL62_maxX;
	SigmaTimeRawJDW = -1;
	FillHisto("hTimeRawJDW_PerBurst",BurstID,TimeRawJDW);
	FillHisto("hSigmaTimeRawJDW_PerBurst",BurstID,SigmaTimeRawJDW);
      }
      ROID_TEL62_maxbin = ROID_TEL62SUP->GetMaximumBin();
      if(fRunNumber>7000){
        ROID_TEL62_maxX = ROID_TEL62_maxbin * DigiTimeFineWidth - 100.;
      }else{
        ROID_TEL62_maxX = (Double_t)ROID_TEL62_maxbin - 500.;
        if(ROID_TEL62_maxX==0)ROID_TEL62_maxX = ROID_TEL62_maxX+0.0001;
      }

      fitmax =  ROID_TEL62_maxX + 0.5;
      fitmin =  ROID_TEL62_maxX - 0.5;
      fitTEL62->SetParameter(0,200000);
      fitTEL62->SetParameter(1,ROID_TEL62_maxX );
      fitTEL62->SetParameter(2,0.2);
      res = ROID_TEL62SUP->Fit("fitTEL62","","",fitmin,fitmax);
      fitStatus = res;
      cout  << user_normal() << "  BurstID: "<<  BurstID<<"  RAW   SUP:   "<< fitTEL62->GetParameter(1) <<"  "<< fitTEL62->GetParameter(2)  <<endl;
      if(fRunNumber>7000 && fitStatus==0){
	TimeRawSUP = fitTEL62->GetParameter(1);
	SigmaTimeRawSUP = fitTEL62->GetParameter(2);
	FillHisto("hTimeRawSUP_PerBurst",BurstID,TimeRawSUP);
	FillHisto("hSigmaTimeRawSUP_PerBurst",BurstID,SigmaTimeRawSUP);
      }else{
	TimeRawSUP = ROID_TEL62_maxX;
	SigmaTimeRawSUP = -1;
	FillHisto("hTimeRawSUP_PerBurst",BurstID,TimeRawSUP);
	FillHisto("hSigmaTimeRawSUP_PerBurst",BurstID,SigmaTimeRawSUP);
      }
      ROID_TEL62_maxbin = ROID_TEL62SDW->GetMaximumBin();
      if(fRunNumber>7000){
        ROID_TEL62_maxX = ROID_TEL62_maxbin * DigiTimeFineWidth - 100.;
      }else{
        ROID_TEL62_maxX = (Double_t)ROID_TEL62_maxbin - 500.;
        if(ROID_TEL62_maxX==0)ROID_TEL62_maxX = ROID_TEL62_maxX+0.0001;
      }
      fitmax =  ROID_TEL62_maxX + 0.5;
      fitmin =  ROID_TEL62_maxX - 0.5;
      fitTEL62->SetParameter(0,200000);
      fitTEL62->SetParameter(1,ROID_TEL62_maxX );
      fitTEL62->SetParameter(2,0.2);
      res = ROID_TEL62SDW->Fit("fitTEL62","","",fitmin,fitmax);
      fitStatus = res;
      cout << user_normal() << "  BurstID: "<<  BurstID <<"  RAW   SDW:   "<<  fitTEL62->GetParameter(1) <<"  "<<   fitTEL62->GetParameter(2)  <<endl;;
      if(fRunNumber>7000 && fitStatus==0){
	TimeRawSDW = fitTEL62->GetParameter(1);
	SigmaTimeRawSDW = fitTEL62->GetParameter(2);
	FillHisto("hTimeRawSDW_PerBurst",BurstID,TimeRawSDW);
	FillHisto("hSigmaTimeRawSDW_PerBurst",BurstID,SigmaTimeRawSDW);
      }else{
	TimeRawSDW = ROID_TEL62_maxX;
	SigmaTimeRawSDW = -1;
	FillHisto("hTimeRawSDW_PerBurst",BurstID,TimeRawSDW);
	FillHisto("hSigmaTimeRawSDW_PerBurst",BurstID,SigmaTimeRawSDW);
      }
      ROID_TEL62_maxbin = ROID_TEL62MUL->GetMaximumBin();
      if(fRunNumber>7000){
        ROID_TEL62_maxX = ROID_TEL62_maxbin * DigiTimeFineWidth - 100.;
      }else{
        ROID_TEL62_maxX = (Double_t)ROID_TEL62_maxbin - 500.;
        if(ROID_TEL62_maxX==0)ROID_TEL62_maxX = ROID_TEL62_maxX+0.0001;
      }
      fitmax =  ROID_TEL62_maxX + 0.5;
      fitmin =  ROID_TEL62_maxX - 0.5;
      fitTEL62->SetParameter(0,200000);
      fitTEL62->SetParameter(1,ROID_TEL62_maxX );
      fitTEL62->SetParameter(2,0.5);
      res = ROID_TEL62MUL->Fit("fitTEL62","","",fitmin,fitmax);
      fitStatus = res;
      cout  << user_normal() << "BurstID:"<<  BurstID <<"  RAW   MUL:   "<<  fitTEL62->GetParameter(1) <<"  "<<   fitTEL62->GetParameter(2)  <<endl;
      if(fRunNumber>7000 && fitStatus==0){
	TimeRawMUL = fitTEL62->GetParameter(1);
	SigmaTimeRawMUL = fitTEL62->GetParameter(2);
	FillHisto("hTimeRawMUL_PerBurst",BurstID,TimeRawMUL);
	FillHisto("hSigmaTimeRawMUL_PerBurst",BurstID,SigmaTimeRawMUL);
      }else{
	TimeRawMUL = ROID_TEL62_maxX;
	SigmaTimeRawMUL = -1;
	FillHisto("hTimeRawMUL_PerBurst",BurstID,TimeRawMUL);
	FillHisto("hSigmaTimeRawMUL_PerBurst",BurstID,SigmaTimeRawMUL);
      }
    } //
    SaveAllPlots();
    return;
  } // end of "reading data"

  ofstream InfoFile;
  InfoFile.open("RICHMonitorInfo.dat");
  ofstream BadBurstFile;
  BadBurstFile.open("RICHMonitorBadBurst.dat");

  if (!fHControlEventsPerBurst) { // Histo mode required but no histograms found
    InfoFile << "[RICHMonitor] Asked to read my own output but cannot found it" << endl;
    return;
  }
  fRunNumber = GetRunID();
  if (fRunNumber>=0) {
    InfoFile << "[RICHMonitor] Run number found: " << fRunNumber << endl;
  }
  else {
    InfoFile << "[RICHMonitor] Invalid run number (" << fRunNumber << "): exiting " << endl;
    return;
  }
  InfoFile <<"[RICHMonitor] Number of events -- total:"<< fHTotalEventsPerBurst->GetEntries()  <<
    "   control:"<<  fHControlEventsPerBurst->GetEntries()  <<  "   physics:"<<  fHPhysicsEventsPerBurst->GetEntries()  << endl;
  InfoFile<<"[RICHMonitor] PM HitQuality  Mean:"<< fhPMHitQuality->GetMean() << "  RMS:"<<  fhPMHitQuality->GetRMS() << endl;
  InfoFile<<"[RICHMonitor] SC HitQuality  Mean:"<< fhSCHitQuality->GetMean() << "  RMS:"<<  fhSCHitQuality->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] Event PM Hits  Mean:"<<  fhNRICHEventPMHits->GetMean() << "  RMS:"<<  fhNRICHEventPMHits->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] Event SC Hits  Mean:"<<  fhNRICHEventSCHits->GetMean() << "  RMS:"<<  fhNRICHEventSCHits->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] Event PMTimeCand  Mean:"<<  fhNRICHTimeCand->GetMean() << "  RMS:"<< fhNRICHTimeCand->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] Event RingCand  Mean:"<<  fhNRICHRingCand->GetMean() << "  RMS:"<< fhNRICHRingCand->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] T(RICHHit)-T(KTAGCand)  Mean:"<<  fhDeltaT_CedarCand_RICHHit->GetMean() << "  RMS:"<<  fhDeltaT_CedarCand_RICHHit->GetRMS() << endl;
  InfoFile <<"[RICHMonitor] T2-T1  Mean:"<<  fhDeltaT_t1t2->GetMean() << "  RMS:"<<  fhDeltaT_t1t2->GetRMS() << endl;


  fHNSCHitsPerControlEvent = new TH1F("fHNSCHitsPerControlEvent",
				      "SC hits per control trigger;Burst ID;SCHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNSCHitsPerControlEvent->Divide(fhNSCHitsPerBurst,fHControlEventsPerBurst);

  fHNPMHitsPerControlEvent = new TH1F("fHNPMHitsPerControlEvent",
				      "PM hits per control trigger;Burst ID;PMHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMHitsPerControlEvent->Divide(fhNPMHitsPerBurst,fHControlEventsPerBurst);

  fHNPMTimeCandPerControlEvent = new TH1F("fHNPMTimeCandPerControlEvent",
					  "PM TimeCand per control trigger;Burst ID;PMTimeCand/trigger"
					  , fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMTimeCandPerControlEvent->Divide(fhNPMTimeCandPerBurst,fHControlEventsPerBurst);

  fHNSCHitsInTimePerControlEvent = new TH1F("fHNSCHitsInTimePerControlEvent",
					    "SC hits in time per control trigger;Burst ID;SCHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNSCHitsInTimePerControlEvent->Divide(fhNSCHitsInTimePerBurst,fHControlEventsPerBurst);


  fHNPMHitsInTimePerControlEvent = new TH1F("fHNPMHitsInTimePerControlEvent",
					    " PM hits in time per control trigger;Burst ID;PMHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMHitsInTimePerControlEvent->Divide(fhNPMHitsInTimePerBurst,fHControlEventsPerBurst);

  fHNPMTimeCandInTimePerControlEvent = new TH1F("fHNPMTimeCandInTimePerControlEvent",
						"PM TimeCand in time per control trigger;Burst ID;PMTimeCand/trigger",
						fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMTimeCandInTimePerControlEvent->Divide(fhNPMTimeCandInTimePerBurst,fHControlEventsPerBurst);


  fHNSCHitsPerPhysicsEvent = new TH1F("fHNSCHitsPerPhysicsEvent",
				      "SC hits per physics trigger;Burst ID;SCHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNSCHitsPerPhysicsEvent->Divide(fhNSCHitsPerBurst_phys,fHPhysicsEventsPerBurst);


  fHNPMHitsPerPhysicsEvent = new TH1F("fHNPMHitsPerPhysicsEvent",
				      "PM hits per physics trigger;Burst ID;PMHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMHitsPerPhysicsEvent->Divide(fhNPMHitsPerBurst_phys,fHPhysicsEventsPerBurst);

  fHNPMTimeCandPerPhysicsEvent = new TH1F("fHNPMTimeCandPerPhysicsEvent",
					  "PM TimeCand per physics trigger;Burst ID;PMTimeCand/trigger"

					  , fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMTimeCandPerPhysicsEvent->Divide(fhNPMTimeCandPerBurst_phys,fHPhysicsEventsPerBurst);


  fHNSCHitsInTimePerPhysicsEvent = new TH1F("fHNSCHitsInTimePerPhysicsEvent",
					    "SC hits in time per physics trigger;Burst ID;SCHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNSCHitsInTimePerPhysicsEvent->Divide(fhNSCHitsInTimePerBurst_phys,fHPhysicsEventsPerBurst);

  fHNPMHitsInTimePerPhysicsEvent = new TH1F("fHNPMHitsInTimePerPhysicsEvent",
					    "PM hits in time per physics trigger;Burst ID;PMHits/trigger", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMHitsInTimePerPhysicsEvent->Divide(fhNPMHitsInTimePerBurst_phys,fHPhysicsEventsPerBurst);
  fHNPMTimeCandInTimePerPhysicsEvent = new TH1F("fHNPMTimeCandInTimePerPhysicsEvent",
						"PM TimeCand in time per physics trigger;Burst ID;PMTimeCand/trigger",
						fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fHNPMTimeCandInTimePerPhysicsEvent->Divide(fhNPMTimeCandInTimePerBurst_phys,fHPhysicsEventsPerBurst);


  Double_t EffiRecoMu=-999.;
  Double_t EffiRecoPi=-999.;
  Double_t EffiRecoElec=-999.;
  if(fhPAllPiTracks->Integral(15,35)>0){
    EffiRecoPi=  fhPPiTracks->Integral(15,35)/fhPAllPiTracks->Integral(15,35);
  }
  if(fhPAllMuTracks->Integral(15,35)>0){
    EffiRecoMu=  fhPMuTracks->Integral(15,35)/fhPAllMuTracks->Integral(15,35);
  }
  if(fhPAllElecTracks->Integral(15,35)>0){
    EffiRecoElec=  fhPElecTracks->Integral(15,35)/fhPAllElecTracks->Integral(15,35);
  }
  InfoFile <<"[RICHMonitor] N(e Ring):"<<  fhNElecRing_vs_BurstID->Integral() << "  N(e trk):"<<  fhNElecTrack_vs_BurstID->Integral()     <<  endl;
  InfoFile <<"[RICHMonitor] N(mu Ring):"<<  fhNMuRing_vs_BurstID->Integral() << "  N(mu trk):"<<  fhNMuTrack_vs_BurstID->Integral()    <<endl;
  InfoFile <<"[RICHMonitor] N(pi Ring):"<<  fhNPiRing_vs_BurstID->Integral() <<  "  N(pi trk):"<<  fhNPiTrack_vs_BurstID->Integral()   <<endl;

  fhEffiElRing_vs_BurstID = new TH1F("EffiElRing_vs_BurstID",
				     "e^{+} ring reconstruction efficiency; Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fhEffiElRing_vs_BurstID->Divide(fhNElecRing_vs_BurstID,fhNElecTrack_vs_BurstID);
  fhEffiPiRing_vs_BurstID = new TH1F("EffiPiRing_vs_BurstID",
				     "#pi^{+} ring reconstruction efficiency; Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fhEffiPiRing_vs_BurstID->Divide(fhNPiRing_vs_BurstID,fhNPiTrack_vs_BurstID);
  fhEffiMuRing_vs_BurstID = new TH1F("EffiMuRing_vs_BurstID",
				     "#mu^{+} ring reconstruction efficiency; Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5);
  fhEffiMuRing_vs_BurstID->Divide(fhNMuRing_vs_BurstID,fhNMuTrack_vs_BurstID);

  InfoFile <<"[RICHMonitor] Pion ring reconstruction efficiency: "<< EffiRecoPi  <<endl;
  InfoFile <<"[RICHMonitor] Muon ring reconstruction efficiency: "<< EffiRecoMu  <<endl;
  InfoFile <<"[RICHMonitor] Elec ring reconstruction efficiency: "<< EffiRecoElec  <<endl;


  Int_t NBursts = 0;
  Int_t NBadBursts = 0;
  Int_t NBadStat =0;
  Int_t minSC = 5;
  Int_t minPM = 5;
  Int_t maxSC = 20;
  Int_t maxPM = 20;
  Int_t minCand = 0.5;
  Int_t maxCand = 10;
  Double_t minEffiPi = 0.75;
  for (Int_t iBur=1; iBur<=fHControlEventsPerBurst->GetNbinsX(); iBur++) { // loop over bursts
    Double_t NTotalEvents   = fHTotalEventsPerBurst->GetBinContent(iBur);
    Double_t NControlEvents = fHControlEventsPerBurst->GetBinContent(iBur);
    Bool_t   BadBurstStat   =  (NTotalEvents>0 && NControlEvents< 100);
    if (NTotalEvents){
      ++NBursts; // non-empty burst count
      if(BadBurstStat)NBadStat++;
      if (NTotalEvents && !BadBurstStat) { // exclude low-statistics bursts
        Bool_t BadBurst = false;
        if(fhEffiPiRing_vs_BurstID->GetBinContent(iBur)< minEffiPi ){
          BadBurstFile << Form("BadBurst %06d %04d : Reco efficiency for pions < 0.75", fRunNumber, iBur-1) << std::endl;
          BadBurst=true;
        }
        if(fHNSCHitsInTimePerControlEvent->GetBinContent(iBur)< minSC || fHNSCHitsInTimePerControlEvent->GetBinContent(iBur)> maxSC ){
          InfoFile << Form("[RICHMonitor] %06d %04d : number of SC Hits out of quality range",
                  fRunNumber, iBur-1) << std::endl;
        }
        if(fHNPMHitsInTimePerControlEvent->GetBinContent(iBur)<minPM || fHNPMHitsInTimePerControlEvent->GetBinContent(iBur)> maxPM){
          InfoFile << Form("[RICHMonitor] %06d %04d : number of PM Hits out of quality range",
                  fRunNumber, iBur-1) << std::endl;
        }
        if(fHNPMTimeCandInTimePerControlEvent->GetBinContent(iBur)<minCand || fHNPMTimeCandInTimePerControlEvent->GetBinContent(iBur)> maxCand){
          InfoFile << Form("[RICHMonitor] %06d %04d : number of in time Cand out of quality range",
                  fRunNumber, iBur-1) << std::endl;
        }
        if(fhPMHitQualityBadBurst->GetBinContent(iBur)!=0){
          BadBurstFile << Form("BadBurst %06d %04d : PM hit bad quality", fRunNumber, iBur-1) << std::endl;
          BadBurst=true;
        }
        if(fhSCHitQualityBadBurst->GetBinContent(iBur)!=0){
          BadBurstFile << Form("BadBurst %06d %04d : SC hit bad quality", fRunNumber, iBur-1) << std::endl;
          BadBurst=true;
        }
        if(BadBurst) ++NBadBursts;
      }
    }
  }
  InfoFile << "[RICHMonitor] Total = good + NBadStat + bad bursts: " <<
    NBursts<<" = "<<NBursts-NBadBursts-NBadStat<<" + " <<   NBadStat  <<" + "<<NBadBursts << endl;
  Int_t NStrangeBursts = 0;
  Int_t TotNev0hits = 0;
  for (Int_t iBur=1; iBur<=fHPhysicsEventsPerBurst->GetNbinsX(); iBur++) { // loop over bursts
    Int_t NPhysicsEvents = fHPhysicsEventsPerBurst->GetBinContent(iBur);
    Int_t Nev0SChits=0;
    if(fhPhysEvWith0SCHitsPerBurst_EvQual0->GetBinContent(iBur)!=0){
      Nev0SChits = fhPhysEvWith0SCHitsPerBurst_EvQual0->GetBinContent(iBur);
      InfoFile << Form("[RICHMonitor] Burst %06d %04d : N quality events with 0 SC hits/NPhys= %d / %d",
			fRunNumber, iBur-1, Nev0SChits,NPhysicsEvents) << std::endl;
      NStrangeBursts++;
      TotNev0hits += Nev0SChits;
    }
  }
  InfoFile << "[RICHMonitor] number of bursts with quality events with 0 SC hits: " <<NStrangeBursts <<endl;
  InfoFile << "[RICHMonitor] number of quality events with 0 SC hits: " << TotNev0hits <<endl;

  if(fHTimeRawJUP_PerBurst != nullptr ){   // <--------------------
    TString runburst;
    Int_t NTimeProblemBurst=0;

    for (Int_t iBur=1; iBur<=fHPhysicsEventsPerBurst->GetNbinsX(); iBur++) { // loop over bursts
      if( (fRunNumber > 8060) &&    // from run 8061 the raw time is aligned with trigger reference time (RICH Multi)
	 ( (fabs(fHTimeRawJUP_PerBurst->GetBinContent(iBur)) >1.5 ) ||
	   (fabs(fHTimeRawJDW_PerBurst->GetBinContent(iBur)) >1.5 ) ||
	   (fabs(fHTimeRawSUP_PerBurst->GetBinContent(iBur)) >1.5 ) ||
	   (fabs(fHTimeRawSDW_PerBurst->GetBinContent(iBur)) >1.5 ) ||
	   (fabs(fHTimeRawMUL_PerBurst->GetBinContent(iBur)) >1.5 )
	   )
	  ) {
	runburst = Form("[RICHMonitor] RawTimeShift Burst %06d %04d", fRunNumber, iBur-1);
	InfoFile <<
	  runburst   <<
	  "  RAW Time    JUP: "<<  fHTimeRawJUP_PerBurst->GetBinContent(iBur)  <<
	  " , "<<	fHSigmaTimeRawJUP_PerBurst->GetBinContent(iBur) <<
	  "   JDW: "<<  fHTimeRawJDW_PerBurst->GetBinContent(iBur)   <<
	  " , "<<	fHSigmaTimeRawJDW_PerBurst->GetBinContent(iBur)   <<
	  "   SUP: "<<  fHTimeRawSUP_PerBurst->GetBinContent(iBur)    <<
	  " , "<<	fHSigmaTimeRawSUP_PerBurst->GetBinContent(iBur)  <<
	  "   SDW: "<<  fHTimeRawSDW_PerBurst->GetBinContent(iBur)    <<
	  " , "<<	fHSigmaTimeRawSDW_PerBurst->GetBinContent(iBur) <<
	  "   MUL: "<<  fHTimeRawMUL_PerBurst->GetBinContent(iBur)  <<
	  " , "<<	fHSigmaTimeRawMUL_PerBurst->GetBinContent(iBur) <<
	  endl;
	NTimeProblemBurst++;
      }
    }
    if(fRunNumber > 8060)InfoFile <<" Number of bursts with raw time problem: "<<  NTimeProblemBurst <<endl;
  }

  InfoFile.close();
  BadBurstFile.close();

  if(fHTimeRawJUP_PerBurst != nullptr ){
    fHTimeRawJUP_PerBurst->Write();
    fHTimeRawJDW_PerBurst->Write();
    fHTimeRawSUP_PerBurst->Write();
    fHTimeRawSDW_PerBurst->Write();
    fHTimeRawMUL_PerBurst->Write();

    fHSigmaTimeRawJUP_PerBurst->Write();
    fHSigmaTimeRawJDW_PerBurst->Write();
    fHSigmaTimeRawSUP_PerBurst->Write();
    fHSigmaTimeRawSDW_PerBurst->Write();
    fHSigmaTimeRawMUL_PerBurst->Write();
  }

  fHTotalEventsPerBurst->Write();
  fHPhysicsEventsPerBurst->Write();
  fHControlEventsPerBurst->Write();
  fHNPMTimeCandPerControlEvent->Write();
  fHNSCHitsPerControlEvent->Write();
  fHNPMHitsPerControlEvent->Write();
  fHNPMTimeCandInTimePerControlEvent->Write();
  fHNPMTimeCandInTimePerPhysicsEvent->Write();
  fHNSCHitsInTimePerControlEvent->Write();
  fHNPMHitsInTimePerControlEvent->Write();
  fHNPMTimeCandPerPhysicsEvent->Write();
  fHNSCHitsPerPhysicsEvent->Write();
  fHNPMHitsPerPhysicsEvent->Write();
  fHNSCHitsInTimePerPhysicsEvent->Write();
  fHNPMHitsInTimePerPhysicsEvent->Write();
  fhPhysEvWith0SCHitsPerBurst->Write();
  fhPhysEvWith0SCHitsPerBurst_EvQual0->Write();
  fhPhysEvWith0SCHitsPerBurst_EvQualNot0->Write();
  fhEffiElRing_vs_BurstID->Write();
  fhEffiMuRing_vs_BurstID->Write();
  fhEffiPiRing_vs_BurstID->Write();
  fhNRICHEventPMHits->Write();
  fhNRICHEventSCHits->Write();
  fhNRICHEventPMHits_phys->Write();
  fhNRICHEventSCHits_phys->Write();
  fhNRICHTimeCand->Write();
  fhNRICHRingCand->Write();
  fhPMHitQuality->Write();
  fhSCHitQuality->Write();
  fhDeltaT_t1t2->Write();
  fhDeltaT_RICHCand_CedarCand->Write();
  fhDeltaT_CedarCand_RICHHit->Write();
  fhDeltaT_RICHHitRichCand_vs_SeqID->Write();
  fhDeltaT_RICHHitCedarCand_vs_SeqID->Write();
  fhPAllPiTracks->Write();
  fhPPiTracks->Write();
  fhPAllMuTracks->Write();
  fhPMuTracks->Write();
  fhPAllElecTracks->Write();
  fhPElecTracks->Write();

  fhPiRing_Radius->Write();
  fhPiRing_nHits->Write();
  fhPiRing_nHits_vs_p->Write();
  fhMuRing_Radius->Write();
  fhMuRing_nHits->Write();
  fhMuRing_nHits_vs_p->Write();
  fhElecRing_Radius->Write();
  fhElecRing_nHits->Write();
  fhElecRing_nHits_vs_p->Write();

  fhSingleRing_p_vs_RICHMass->Write();
  fhPiRing_p_vs_RICHMass->Write();
  fhMuRing_p_vs_RICHMass->Write();
  fhElecRing_p_vs_RICHMass->Write();

  fhSingleRing_Radius_vs_p->Write();
  fhPiRing_Radius_vs_p->Write();
  fhMuRing_Radius_vs_p->Write();
  fhElecRing_Radius_vs_p->Write();
  BuildPDFReport();
}



void RICHMonitor::BuildPDFReport() {
  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  Int_t MinBurst = 0;
  Int_t MaxBurst = fMaxNBursts;
  if (fHControlEventsPerBurst->Integral()) {
    while (!fHControlEventsPerBurst->GetBinContent(MinBurst+1)) MinBurst++;
    while (!fHControlEventsPerBurst->GetBinContent(MaxBurst+1)) MaxBurst--;
  }
  if(fHTimeRawJUP_PerBurst != nullptr ){
    fHTimeRawJUP_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHTimeRawJDW_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHTimeRawSUP_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHTimeRawSDW_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHTimeRawMUL_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHSigmaTimeRawJUP_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHSigmaTimeRawJDW_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHSigmaTimeRawSUP_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHSigmaTimeRawSDW_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHSigmaTimeRawMUL_PerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  }


  fHTotalEventsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHControlEventsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNSCHitsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNPMHitsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNPMTimeCandPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNSCHitsPerControlEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNPMHitsPerControlEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNPMTimeCandPerControlEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNSCHitsPerBurst_phys->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNPMHitsPerBurst_phys->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhNPMTimeCandPerBurst_phys->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNSCHitsPerPhysicsEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNPMHitsPerPhysicsEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHNPMTimeCandPerPhysicsEvent->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhEffiElRing_vs_BurstID->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhEffiMuRing_vs_BurstID->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhEffiPiRing_vs_BurstID->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhPhysEvWith0SCHitsPerBurst_EvQual0->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fhPhysEvWith0SCHitsPerBurst_EvQualNot0->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);

  TCanvas *CanvasEvt = new TCanvas("CanvasEvt");
  CanvasEvt->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
  TString runnumb = Form("Run %05d - ", fRunNumber);

  fHTotalEventsPerBurst->SetTitle(runnumb + "total, control and physics triggers");
  fHTotalEventsPerBurst->SetLineColor(kBlue);
  fHTotalEventsPerBurst->SetMarkerColor(kBlue);
  //fHTotalEventsPerBurst->GetYaxis()->SetRangeUser(0.5,500000);
  fHTotalEventsPerBurst->Draw("");
  fHControlEventsPerBurst->SetLineColor(kRed);
  fHControlEventsPerBurst->SetMarkerColor(kRed);
  fHControlEventsPerBurst->Draw("same");
  fHPhysicsEventsPerBurst->SetLineColor(kGreen);
  fHPhysicsEventsPerBurst->SetMarkerColor(kGreen+2);
  fHPhysicsEventsPerBurst->Draw("same");

  TLegend *leg = new TLegend(0.78,0.92,1.00,1.00);
  leg->SetFillColor(0);
  leg->AddEntry(fHTotalEventsPerBurst,   "Total triggers", "pl");
  leg->AddEntry(fHControlEventsPerBurst, "Control events", "pl");
  leg->AddEntry(fHPhysicsEventsPerBurst, "Physics events", "pl");
  leg->Draw();
  CanvasEvt->Print(OutputPDFFileName, "pdf");
  //---------------------------------------------------------

  TCanvas *CanvasTimeTEL = new TCanvas("CanvasTimeTEL");
  CanvasTimeTEL->cd(1);
  fHTimeRawJUP_PerBurst->SetLineColor(kBlue+3);
  fHTimeRawJDW_PerBurst->SetLineColor(kRed+3);
  fHTimeRawSUP_PerBurst->SetLineColor(kGreen+3);
  fHTimeRawSDW_PerBurst->SetLineColor(kMagenta+3);
  fHTimeRawMUL_PerBurst->SetLineColor(1);
  fHTimeRawJUP_PerBurst->SetMarkerColor(kBlue+3);
  fHTimeRawJDW_PerBurst->SetMarkerColor(kRed+3);
  fHTimeRawSUP_PerBurst->SetMarkerColor(kGreen+3);
  fHTimeRawSDW_PerBurst->SetMarkerColor(kMagenta+3);
  fHTimeRawMUL_PerBurst->SetMarkerColor(1);
  fHTimeRawJUP_PerBurst->SetMarkerStyle(20);
  fHTimeRawJDW_PerBurst->SetMarkerStyle(21);
  fHTimeRawSUP_PerBurst->SetMarkerStyle(22);
  fHTimeRawSDW_PerBurst->SetMarkerStyle(23);
  fHTimeRawMUL_PerBurst->SetMarkerStyle(34);
  if(fRunNumber>7000) fHTimeRawMUL_PerBurst->GetYaxis()->SetRangeUser(-3.,1.);
  else fHTimeRawMUL_PerBurst->GetYaxis()->SetRangeUser(-3.,3.);
  fHTimeRawMUL_PerBurst->SetTitle("TEL62 Raw Time Mean");
  fHTimeRawMUL_PerBurst->Draw("histp");
  fHTimeRawJUP_PerBurst->Draw("histpsame");
  fHTimeRawJDW_PerBurst->Draw("histpsame");
  fHTimeRawSUP_PerBurst->Draw("histpsame");
  fHTimeRawSDW_PerBurst->Draw("histpsame");
  CanvasTimeTEL->SetGridx(1);
  CanvasTimeTEL->SetGridy(1);

  TLegend *leg00 = new TLegend(0.85,0.7,1.00,1.00);
  leg00->AddEntry(fHTimeRawMUL_PerBurst,   "MULTI", "p");
  leg00->AddEntry(fHTimeRawJUP_PerBurst,   "JUP", "p");
  leg00->AddEntry(fHTimeRawJDW_PerBurst,   "JDW", "p");
  leg00->AddEntry(fHTimeRawSUP_PerBurst,   "SUP", "p");
  leg00->AddEntry(fHTimeRawSDW_PerBurst,   "SDW", "p");
  leg00->Draw();

  CanvasTimeTEL->Print(OutputPDFFileName, "pdf");

  TCanvas *CanvasSigmaTimeTEL = new TCanvas("CanvasSigmaTimeTEL");
  CanvasSigmaTimeTEL->cd(1);
  fHSigmaTimeRawJUP_PerBurst->SetLineColor(kBlue+3);
  fHSigmaTimeRawJDW_PerBurst->SetLineColor(kRed+3);
  fHSigmaTimeRawSUP_PerBurst->SetLineColor(kGreen+3);
  fHSigmaTimeRawSDW_PerBurst->SetLineColor(kMagenta+3);
  fHSigmaTimeRawMUL_PerBurst->SetLineColor(1);
  fHSigmaTimeRawJUP_PerBurst->SetMarkerColor(kBlue+3);
  fHSigmaTimeRawJDW_PerBurst->SetMarkerColor(kRed+3);
  fHSigmaTimeRawSUP_PerBurst->SetMarkerColor(kGreen+3);
  fHSigmaTimeRawSDW_PerBurst->SetMarkerColor(kMagenta+3);
  fHSigmaTimeRawMUL_PerBurst->SetMarkerColor(1);
  fHSigmaTimeRawJUP_PerBurst->SetMarkerStyle(20);
  fHSigmaTimeRawJDW_PerBurst->SetMarkerStyle(21);
  fHSigmaTimeRawSUP_PerBurst->SetMarkerStyle(22);
  fHSigmaTimeRawSDW_PerBurst->SetMarkerStyle(23);
  fHSigmaTimeRawMUL_PerBurst->SetMarkerStyle(34);
  if(fRunNumber>7000) fHSigmaTimeRawMUL_PerBurst->GetYaxis()->SetRangeUser(0.,1.);
  else fHSigmaTimeRawMUL_PerBurst->GetYaxis()->SetRangeUser(-1.,1.);
  fHSigmaTimeRawMUL_PerBurst->SetTitle("TEL62 Raw Time Resolution");
  fHSigmaTimeRawMUL_PerBurst->Draw("histp");
  fHSigmaTimeRawJUP_PerBurst->Draw("histpsame");
  fHSigmaTimeRawJDW_PerBurst->Draw("histpsame");
  fHSigmaTimeRawSUP_PerBurst->Draw("histpsame");
  fHSigmaTimeRawSDW_PerBurst->Draw("histpsame");
  CanvasSigmaTimeTEL->SetGridx(1);
  CanvasSigmaTimeTEL->SetGridy(1);

  TLegend *leg0 = new TLegend(0.85,0.7,1.00,1.00);
  leg0->AddEntry(fHTimeRawMUL_PerBurst,   "MULTI", "p");
  leg0->AddEntry(fHTimeRawJUP_PerBurst,   "JUP", "p");
  leg0->AddEntry(fHTimeRawJDW_PerBurst,   "JDW", "p");
  leg0->AddEntry(fHTimeRawSUP_PerBurst,   "SUP", "p");
  leg0->AddEntry(fHTimeRawSDW_PerBurst,   "SDW", "p");
  leg0->Draw();
  CanvasSigmaTimeTEL->Print(OutputPDFFileName, "pdf");

  //---------------------------------------------------------

  TCanvas *CanvasHitsPerBurst = new TCanvas("CanvasHitsPerBurst");
  CanvasHitsPerBurst->Divide(1,2);
  CanvasHitsPerBurst->cd(1);
  fHNPMHitsPerControlEvent->SetTitle("Hits per control trigger");
  fHNSCHitsPerControlEvent->SetLineColor(kBlue);
  fHNSCHitsPerControlEvent->SetMarkerColor(kBlue);
  fHNPMHitsPerControlEvent->SetLineColor(kRed);
  fHNPMHitsPerControlEvent->SetMarkerColor(kRed);
  fHNPMHitsPerControlEvent->Draw("");
  fHNSCHitsPerControlEvent->Draw("same");
  fHNSCHitsInTimePerControlEvent->SetLineColor(kBlue+2);
  fHNSCHitsInTimePerControlEvent->SetMarkerColor(kBlue+2);
  fHNSCHitsInTimePerControlEvent->Draw("same");
  fHNPMHitsInTimePerControlEvent->SetLineColor(kRed+2);
  fHNPMHitsInTimePerControlEvent->SetMarkerColor(kRed+2);
  fHNPMHitsInTimePerControlEvent->Draw("same");

  TLegend *leg1 = new TLegend(0.78,0.80,1.00,1.00);
  leg1->SetFillColor(0);
  leg1->AddEntry(fHNSCHitsPerControlEvent,   "SuperCells Hits", "pl");
  leg1->AddEntry(fHNPMHitsPerControlEvent,   "PhotoMultipliers Hits", "pl");
  leg1->AddEntry(fHNSCHitsInTimePerControlEvent,   "In Time SuperCells Hits", "pl");
  leg1->AddEntry(fHNPMHitsInTimePerControlEvent,   "In Time PhotoMultipliers Hits", "pl");
  leg1->Draw();
  CanvasHitsPerBurst->cd(2);
  fHNPMTimeCandPerControlEvent->SetLineColor(kBlue);
  fHNPMTimeCandPerControlEvent->SetMarkerColor(kBlue);
  fHNPMTimeCandPerControlEvent->Draw("");
  fHNPMTimeCandInTimePerControlEvent->SetLineColor(kViolet);
  fHNPMTimeCandInTimePerControlEvent->SetMarkerColor(kViolet);
  fHNPMTimeCandInTimePerControlEvent->Draw("same");
  TLegend *leg2 = new TLegend(0.78,0.92,1.00,1.00);
  leg2->SetFillColor(0);
  leg2->AddEntry(fHNPMTimeCandPerControlEvent,   "TimeCandidates", "pl");
  leg2->AddEntry(fHNPMTimeCandInTimePerControlEvent,   "In Time TimeCandidates", "pl");
  leg2->Draw();
  CanvasHitsPerBurst->Print(OutputPDFFileName, "pdf");
   //---------------------------------------------------------
  TCanvas *CanvasHitsPerBurst_phys = new TCanvas("CanvasHitsPerBurst_phys");
  CanvasHitsPerBurst_phys->Divide(1,2);
  CanvasHitsPerBurst_phys->cd(1);
  fHNPMHitsPerPhysicsEvent->SetTitle("Hits per control trigger");
  fHNSCHitsPerPhysicsEvent->SetLineColor(kBlue);
  fHNSCHitsPerPhysicsEvent->SetMarkerColor(kBlue);
  //  fHNSCHitsPerPhysicsEvent->Draw("");
  fHNPMHitsPerPhysicsEvent->SetLineColor(kRed);
  fHNPMHitsPerPhysicsEvent->SetMarkerColor(kRed);
  fHNPMHitsPerPhysicsEvent->Draw("");
  fHNSCHitsPerPhysicsEvent->Draw("same");
  fHNSCHitsInTimePerPhysicsEvent->SetLineColor(kBlue+2);
  fHNSCHitsInTimePerPhysicsEvent->SetMarkerColor(kBlue+2);
  fHNSCHitsInTimePerPhysicsEvent->Draw("same");
  fHNPMHitsInTimePerPhysicsEvent->SetLineColor(kRed+2);
  fHNPMHitsInTimePerPhysicsEvent->SetMarkerColor(kRed+2);
  fHNPMHitsInTimePerPhysicsEvent->Draw("same");

  TLegend *l1 = new TLegend(0.78,0.80,1.00,1.00);
  l1->SetFillColor(0);
  l1->AddEntry(fHNSCHitsPerPhysicsEvent,   "SuperCells Hits", "pl");
  l1->AddEntry(fHNPMHitsPerPhysicsEvent,   "PhotoMultipliers Hits", "pl");
  l1->AddEntry(fHNSCHitsInTimePerPhysicsEvent,   "In Time SuperCells Hits", "pl");
  l1->AddEntry(fHNPMHitsInTimePerPhysicsEvent,   "In Time PhotoMultipliers Hits", "pl");
  l1->Draw();
  CanvasHitsPerBurst_phys->cd(2);
  fHNPMTimeCandPerPhysicsEvent->SetLineColor(kBlue);
  fHNPMTimeCandPerPhysicsEvent->SetMarkerColor(kBlue);
  fHNPMTimeCandPerPhysicsEvent->Draw("");
  fHNPMTimeCandInTimePerPhysicsEvent->SetLineColor(kViolet);
  fHNPMTimeCandInTimePerPhysicsEvent->SetMarkerColor(kViolet);
  fHNPMTimeCandInTimePerPhysicsEvent->Draw("same");
  TLegend *l2 = new TLegend(0.78,0.92,1.00,1.00);
  l2->SetFillColor(0);
  l2->AddEntry(fHNPMTimeCandPerPhysicsEvent,   "TimeCandidates", "pl");
  l2->AddEntry(fHNPMTimeCandInTimePerPhysicsEvent,   "In Time TimeCandidates", "pl");
  l2->Draw();
  CanvasHitsPerBurst_phys->Print(OutputPDFFileName, "pdf");
  //---------------------------------------------------------
  TCanvas *CanvasEffiPerBurst = new TCanvas("CanvasEffiPerBurst");
  fhEffiElRing_vs_BurstID->SetTitle("Ring reconstruction efficiency");
  fhEffiElRing_vs_BurstID->SetMarkerSize(3);
  fhEffiElRing_vs_BurstID->SetLineColor(kGreen+2);
  fhEffiElRing_vs_BurstID->SetMarkerColor(kGreen+2);
  fhEffiElRing_vs_BurstID->Draw("p");
  fhEffiMuRing_vs_BurstID->SetMarkerSize(3);
  fhEffiMuRing_vs_BurstID->SetLineColor(kRed);
  fhEffiMuRing_vs_BurstID->SetMarkerColor(kRed);
  fhEffiMuRing_vs_BurstID->Draw("psame");
  fhEffiPiRing_vs_BurstID->SetMarkerSize(3);
  fhEffiPiRing_vs_BurstID->SetLineColor(kBlue);
  fhEffiPiRing_vs_BurstID->SetMarkerColor(kBlue);
  fhEffiPiRing_vs_BurstID->Draw("psame");
  CanvasEffiPerBurst->SetGridx(1);
  CanvasEffiPerBurst->SetGridy(1);

  TLegend *legE = new TLegend(0.78,0.92,1.00,1.00);
  legE->SetFillColor(0);
  legE->AddEntry(fhEffiElRing_vs_BurstID,   "e^{+} ring", "pl");
  legE->AddEntry(fhEffiMuRing_vs_BurstID,   "#mu^{+} ring", "pl");
  legE->AddEntry(fhEffiPiRing_vs_BurstID,   "#pi^{+} ring", "pl");
  legE->Draw();
  CanvasEffiPerBurst->Print(OutputPDFFileName, "pdf");

  TCanvas *Canvas0hits = new TCanvas("Canvas0hits");
  Canvas0hits->Divide(1,2);
  Canvas0hits->cd(1);
  Canvas0hits->SetRightMargin(0.02);
  fhPhysEvWith0SCHitsPerBurst_EvQual0->Draw();
  Canvas0hits->cd(2);
  fhPhysEvWith0SCHitsPerBurst_EvQualNot0->Draw();
  Canvas0hits->Print(OutputPDFFileName, "pdf");
  //---------------------------------------------------------

  TCanvas *CanvasTime = new TCanvas("CanvasTime");
  CanvasTime->Divide(1,2);
  CanvasTime->cd(1);
  CanvasTime->SetRightMargin(0.02);
  // CanvasTime->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
  fhDeltaT_t1t2->GetXaxis()->SetRangeUser(-2,2);
  fhDeltaT_t1t2->GetXaxis()->SetTitleSize(0.04);
  fhDeltaT_t1t2->GetXaxis()->SetTitle("t[Set2]-t[Set1] (ns)");
  fhDeltaT_t1t2->Draw();
  CanvasTime->cd(2);
  fhDeltaT_CedarCand_RICHHit->GetXaxis()->SetTitleSize(0.04);
  fhDeltaT_CedarCand_RICHHit->GetXaxis()->SetTitle("t[RICHHit]-t[KTAGCand] (ns)");
  fhDeltaT_CedarCand_RICHHit->Draw();
  CanvasTime->Print(OutputPDFFileName, "pdf");
  TCanvas *CanvasEffi = new TCanvas("CanvasEffi");
  CanvasEffi->SetRightMargin(0.02);
  CanvasEffi->Divide(1,2);
  CanvasEffi->cd(1);
  TEfficiency* PiEff = 0;
  PiEff = new TEfficiency(*fhPPiTracks,*fhPAllPiTracks);
  PiEff->SetTitle("Pion ring reconstruction efficiency vs track momentum");
  PiEff->Draw("AP");
  CanvasEffi->cd(2);
  TEfficiency* MuEff = 0;
  MuEff = new TEfficiency(*fhPMuTracks,*fhPAllMuTracks);
  MuEff->SetTitle("Muon ring reconstruction efficiency vs track momentum");
  MuEff->Draw("AP");
  CanvasEffi->Print(OutputPDFFileName, "pdf");
  CanvasEffi->Print(Form(OutputPDFFileName + "]"), "pdf"); // open file


  delete CanvasHitsPerBurst;
  delete CanvasEvt;
  delete CanvasTimeTEL;
  delete CanvasSigmaTimeTEL;
  delete CanvasTime;
  delete CanvasEffi;
}
