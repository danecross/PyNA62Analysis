// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-19
//
// ---------------------------------------------------------------

/// \class K3piSelection
/// \Brief
/// K3pi decay selection and evaluation of the beam parameters
/// \EndBrief
/// \Detailed
/// Produces the basic monitoring plots: mass, momentum, vertex position, beam profile.
/// For MC, also builds the true beam spectra of basic kinematic variables.
/// The plots are arranged in three subdirs: "mctrue" for MC true quantities,
/// "general" for histograms filled in the middle of selection, and "selected" for histograms based
/// on the sample passing the full selection. The latter subset is to be used for data/MC comparisons.
/// The selection also computes the number of kaon decays in the fiducial decay volume and
/// the number of protons on target (POT) burst by burst using the sample collected with the control trigger.
/// The analyzer can be run in two modes: 1) read the reconstructed data;
/// 2) read its own output (using the --histo command line option) to compute the
/// central reconstructed momentum and direction (dx/dz, dy/dz) of the beam
/// and produce a PDF report with the most important plots.
/// The outputs can be read by other analyzers in the following way:
/// \code
/// Bool_t   Selected = *(Bool_t*)  GetOutput("K3piSelection.EventSelected");
/// Int_t    VertexID = *(Int_t*)   GetOutput("K3piSelection.VertexID");
/// Double_t M3pi     = *(Double_t*)GetOutput("K3piSelection.M3pi");
/// \endcode
/// To print burst-by-burst N(K decays) and POT, process ONE RUN ONLY at step 1,
/// and use the following parameter settings at step 2:
/// \code
/// ./MyExecutable -l list --histo -p "K3piSelection:PrintFlux=1"
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TStyle.h>
#include "K3piSelection.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "DownstreamTrack.hh"
#include "SpectrometerTrackVertex.hh"
#include "TProfile.h"
#include "TF1.h"
#include "TLegend.h"
#include "TArrow.h"
#include "BeamParameters.hh"
#include "LAVMatching.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

K3piSelection::K3piSelection(Core::BaseAnalysis *ba) :
  Analyzer(ba, "K3piSelection"), fSG(nullptr),
  fHZtrue(nullptr), fHMass(nullptr), fHMomentum(nullptr),
  fHdxdz(nullptr), fHdydz(nullptr), fHx(nullptr), fHy(nullptr) {

  RequestTree("Cedar", new TRecoCedarEvent, "Reco");
  RequestTree("CHOD",  new TRecoCHODEvent,  "Reco");
  RequestTree("RICH",  new TRecoRICHEvent,  "Reco");
  RequestTree("LKr",   new TRecoLKrEvent,   "Reco");
  RequestTree("LAV",   new TRecoLAVEvent,   "Reco");
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");

  RequestL0Data();
  RequestL1Data();
  RequestBeamSpecialTrigger();
  fReadingData = kTRUE;

  AddParam("MaxNBursts",   &fMaxNBursts,   5000); // max number of bins in "vs burst" histograms
  AddParam("PrintFlux",    &fPrintFlux,    false);
  AddParam("ZAcceptedMin", &fZAcceptedMin, 104000); // [mm]
  AddParam("ZAcceptedMax", &fZAcceptedMax, 180000); // [mm]

  fMinRunID = 6278; // for histograms
  fMaxRunID = 8282;

  //////////////////////////////////////////////////////////
  // Initialize parameters for kaon flux and POT computation

  fZFiducialMin     =  105000; // [mm]
  fZFiducialMax     =  180000; // [mm]
  fZCedar           =   70559; // start of Cedar vessel [mm]
  fZGTK3            = GeometricAcceptance::GetInstance()->GetZGTK3();
  fZLKr             = GeometricAcceptance::GetInstance()->GetZLKr();
  fZArgonion        = GeometricAcceptance::GetInstance()->GetZArgonion();
  fKaonMeanPath     =  563857; // [mm] at 75 GeV/c
  fPionMeanPath     = 4193851; // [mm] at 75 GeV/c
  fFVCedarConv      = exp((fZCedar-fZFiducialMin)/fKaonMeanPath);    // ~0.94
  fArgonionFVConvK  = exp((fZFiducialMin-fZArgonion)/fKaonMeanPath); // ~0.75
  fArgonionFVConvPi = exp((fZFiducialMin-fZArgonion)/fPionMeanPath); // ~0.96
  fDecayProb        = 1.0 - exp((fZFiducialMin-fZFiducialMax)/fKaonMeanPath); // ~0.125
  fAcceptance       = 0.1536;  // Acceptance in Z range (105-180)m, evaluated with MC v0.11.2
  fBRK3pi           = 0.05583; // PDG 2017
  fDownscaling1     = -1; // control trigger downscaling; initialized in StartOfRunUser()
  fDownscaling2     = -1; // multi-track trigger downscaling; initialized in StartOfRunUser()
  fStartOfRunTime   = 0;

  // Conversion of protons-on-target to number of kaons entering the decay volume.
  // Source: the NA62 detector paper [JINST 12 (2017) P05025, arXiv:1703.08501], Table 2.
  fPOT_to_Kaon = 1.1e12 / 45.0e6;

  // Beam composition at the entrance to FV
  fPionFractionFV   = 0.70;
  fProtonFractionFV = 0.24;
  fKaonFractionFV   = 0.06;
  fSpillLength      = 3.0; // effective spill length [s]

  // Beam composition at Argonion (normalized to beam at entrance to FV)
  fPionFractionArgonion   = fPionFractionFV * fArgonionFVConvPi;
  fProtonFractionArgonion = fProtonFractionFV;
  fKaonFractionArgonion   = fKaonFractionFV * fArgonionFVConvK;

  // Argonion to kaon at FV entrance conversion factor
  fKaon_to_Argonion = fKaonFractionFV /
    (fPionFractionArgonion + fProtonFractionArgonion + fKaonFractionArgonion); // ~0.06

  fTriggerConditions = TriggerConditions::GetInstance();
  fTriggerMultiTrack = fTriggerConditions->GetL0TriggerID("RICH-QX");

  // add_preanalyzer L0RICHEmulator L0MUV3Emulator L0NewCHODEmulator // Add the L0Emulators as preanalyzers
  fPrimitiveHandler = L0PrimitiveHandler::GetInstance();
  fPrimitiveHandler->DeclareL0Emulators(fParent, kL0RICH, kL0NewCHOD, kL0MUV3);
  //fPrimitiveHandler->PrintDetectorKey();

  // Initialize the spectrometer-GTK matching tool
  fSG = new SpectrometerGigaTrackerMatchingTool();
  fSG->SetMatchingTimingCuts(-0.5, 0.5);
}

K3piSelection::~K3piSelection() {
  if (fSG) delete fSG;
}

void K3piSelection::StartOfBurstUser() {
  if (!fReadingData) return;
  fMinTimeInSpill  = 1e9;
  fMaxTimeInSpill = -1e9;
  // Burst count, works for multiple runs mixed in the list
  FillHisto("general/hBurstID", GetBurstID());
  // Kaon rate and its error [MHz], works for single run only
  fHisto.GetHisto("general/hKaonRateKTAG")->SetBinContent
    (GetBurstID()+1, fFVCedarConv * GetStreamInfo()->GetRecoInfo().GetKaonRate());
  fHisto.GetHisto("general/hKaonRateKTAG")->SetBinError
    (GetBurstID()+1, fFVCedarConv * GetStreamInfo()->GetRecoInfo().GetKaonRateError());

  // Check if this sample comes from fast MC simulation
  // (with detectors downstream of the RICH not simulated and not reconstructed)
  fFastSimulation = false;
  if (GetWithMC()) fFastSimulation = GetStreamInfo()->GetMCInfo().GetFastSimulationMode();
}

void K3piSelection::EndOfBurstUser() {
  if (!fReadingData) return;
  if (fMaxTimeInSpill>=fMinTimeInSpill) { // burst has control or physics events
    FillHisto("general/hMinTimeInSpill", GetBurstID(), fMinTimeInSpill);
    FillHisto("general/hMaxTimeInSpill", GetBurstID(), fMaxTimeInSpill);
  }
}

void K3piSelection::ProcessEOBEvent() {
  if (!fReadingData) return;
  Double_t ArgonionCount = GetBeamSpecialTrigger()->GetCountsARGONION()/1e6; // [mln]
  FillHisto("general/hKaonRateArgonion", GetBurstID(),
	    fKaon_to_Argonion*ArgonionCount/fSpillLength);
}

void K3piSelection::StartOfRunUser() {
  // DS of the control trigger:
  // -999 means control trigger is disabled or information is missing the database;
  // -1 means downscaling is variable for this run.
  fDownscaling1 = fTriggerConditions->GetControlTriggerDownscaling(GetRunID());
  fDownscaling2 = fTriggerConditions->GetL0TriggerDownscaling(GetRunID(), fTriggerMultiTrack);

  fStartOfRunTime = GetBurstTime();

  fPrimitiveHandler->SetRunID(GetRunID());
  fPrimitiveHandler->ConfigureL0Emulators(); // config for the above run number
}

void K3piSelection::InitHist() {
  fReadingData = GetIsTree(); // false in --histo mode, true otherwise

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;

    // Ask for three-track vertices to be built
    ReconfigureAnalyzer("SpectrometerVertexBuilder", "Build3TrackVtx", true);

    // Histograms of MC true quantities
    BookHisto("mctrue/hZvertex",       new TH1F("Zvertex_true", "True Zvertex; z [m]", 300, 0, 300));
    BookHisto("mctrue/hMomentum",      new TH1F("Momentum_true", "True kaon momentum; Momentum [GeV/c]", 60, 72, 78));
    BookHisto("mctrue/hTrackMomentum", new TH1F("TrackMomentum_true", "True track momentum;Momentum [GeV/c]", 50, 0, 50));
    BookHisto("mctrue/hTrackX1",       new TH1F("TrackX1_true", "True track x at Straw1;x [mm]", 50, -500, 500));
    BookHisto("mctrue/hTrackY1",       new TH1F("TrackY1_true", "True track y at Straw1;y [mm]", 50, -500, 500));
    BookHisto("mctrue/hdxdz",          new TH1F("dxdz_true",    "True kaon dx/dz",  100, -0.0025, 0.0025));
    BookHisto("mctrue/hdydz",          new TH1F("dydz_true",    "True kaon dy/dz",  100, -0.0025, 0.0025));
    BookHisto("mctrue/hXYstart",       new TH2F("XYstart_true", "True kaon (x,y) at GTK3 plane (z=102.4m);x [mm]; y[mm]",
					   50, -50, 50, 50, -50, 50));
    BookHisto("mctrue/hXstart",        new TH1F("Xstart_true", "True kaon x at GTK3 plane (z=102.4m);x [mm]", 100, -50, 50));
    BookHisto("mctrue/hYstart",        new TH1F("Ystart_true", "True kaon y at GTK3 plane (z=102.4m);y [mm]", 100, -50, 50));
    BookHisto("mctrue/hdxdzVsXstart", new TH2F
	      ("dxdzVsX_true", "True dx/dz vs x; x [mm]; dx/dz", 20, -50, 50, 50, -0.0025, 0.0025));
    BookHisto("mctrue/hdydzVsXstart", new TH2F
	      ("dydzVsX_true", "True dy/dz vs x; x [mm]; dy/dz", 20, -50, 50, 50, -0.0025, 0.0025));
    BookHisto("mctrue/hdxdzVsYstart", new TH2F
	      ("mctrue/dxdzVsY_true", "True dx/dz vs y; y [mm]; dx/dz", 20, -25, 25, 50, -0.0025, 0.0025));
    BookHisto("mctrue/hdydzVsYstart", new TH2F
	      ("dydzVsY_true", "True dy/dz vs y; y [mm]; dy/dz", 20, -25, 25, 50, -0.0025, 0.0025));
    BookHisto("mctrue/pdxdzVsXstart", new TProfile("dxdzVsXP_true", "True dx/dz vs x; x [mm]; dx/dz", 20, -50, 50));
    BookHisto("mctrue/pdydzVsXstart", new TProfile("dydzVsXP_true", "True dy/dz vs x; x [mm]; dy/dz", 20, -50, 50));
    BookHisto("mctrue/pdxdzVsYstart", new TProfile("dxdzVsYP_true", "True dx/dz vs y; y [mm]; dx/dz", 20, -25, 25));
    BookHisto("mctrue/pdydzVsYstart", new TProfile("dydzVsYP_true", "True dy/dz vs y; y [mm]; dy/dz", 20, -25, 25));

    // Histograms of reconstructed quantities: general monitoring
    BookHisto("general/hNTracks",        new TH1F("NTracks",    "Number of tracks",           11, -0.5, 10.5));
    BookHisto("general/hNVertices",      new TH1F("NVertices",  "Number of 3-track vertices", 11, -0.5, 10.5));
    BookHisto("general/hVertexTime",     new TH1F("VertexTime", "Vertex STRAW time", 200, -100, 100));

    BookHisto("general/hTrackXY_straw1", new TH2F("TrackXY_straw1", "Track (x,y) at Straw chamber 1;x [mm];y [mm]",
					  60, -1200, 1200, 60, -1200, 1200));
    BookHisto("general/hTrackXY_straw2", new TH2F("TrackXY_straw2", "Track (x,y) at Straw chamber 2;x [mm];y [mm]",
					  60, -1200, 1200, 60, -1200, 1200));
    BookHisto("general/hTrackXY_straw3", new TH2F("TrackXY_straw3", "Track (x,y) at Straw chamber 3;x [mm];y [mm]",
					  60, -1200, 1200, 60, -1200, 1200));
    BookHisto("general/hTrackXY_straw4", new TH2F("TrackXY_straw4", "Track (x,y) at Straw chamber 4;x [mm];y [mm]",
					  60, -1200, 1200, 60, -1200, 1200));
    BookHisto("general/DistanceStraw1", new TH1D("DistanceStraw1", "Distance between track pairs in Straw1;[mm]", 500, 0, 1000));
    BookHisto("general/DistanceLKr", new TH1D("DistanceLKr", "Distance between track pairs in LKr;[mm]", 500, 0, 2000));
    BookHisto("general/hVertexChi2", new TH1F("VertexChi2", "#chi^{2} of the vertices;#chi^{2}", 100,  0.0, 100.0));
    BookHisto("general/hZvertex0", new TH1F("Zvertex0", "Zvertex; z [m]", 100, 90, 190));
    BookHisto("general/hVertexTimeControl", new
	      TH1F("VertexTimeControl", "Vertex CHOD time wrt trigger (control triggers);time [ns]", 100, -50, 50));
    BookHisto("general/hVertexTimePhysics", new
	      TH1F("VertexTimePhysics", "Vertex CHOD time wrt trigger (physics triggers);time [ns]", 100, -50, 50));
    BookHisto("general/hMomentum0", new TH1F("Momentum0", "Kaon momentum;Momentum [GeV/c]", 200, 50, 100));
    BookHisto("general/hPt0", new TH1F("Pt0", "Kaon transverse momentum;Momentum [MeV/c]", 200, 0, 200));

    // Histograms of reconstructed quantities: fully selected K3pi decays
    BookHisto("selected/hM3pi",          new TH1F("M3pi",     "M(3#pi); M(3#pi) [MeV]", 120, 480, 510));
    BookHisto("selected/hM3piNoBF",      new TH1F("M3piNoBF", "M(3#pi), no blue tube corrections; M(3#pi) [MeV]", 120, 480, 510));
    BookHisto("selected/hZvertex",       new TH1F("Zvertex",  "Zvertex; z [m]", 40, 100, 180));
    BookHisto("selected/hMomentum",      new TH1F("Momentum", "Kaon momentum; Momentum [GeV/c]", 60, 72, 78));
    BookHisto("selected/hPt", new TH1F("Pt", "Kaon transverse momentum;Momentum [MeV/c]", 200, 0, 200));
    BookHisto("selected/hM3piControlTrigger", new TH1F
	      ("M3piControlTrigger", "M(3#pi); M(3#pi) [MeV]", 120, 480, 510));
    BookHisto("selected/hM3piNoBFControlTrigger", new TH1F
	      ("M3piNoBFControlTrigger", "M(3#pi), no blue tube corrections; M(3#pi) [MeV]", 120, 480, 510));
    BookHisto("selected/hZvertexControlTrigger", new TH1F
	      ("ZvertexControlTrigger",  "Zvertex; z [m]", 40, 100, 180));
    BookHisto("selected/hMomentumControlTrigger", new TH1F
	      ("MomentumControlTrigger", "Kaon momentum; Momentum [GeV/c]", 60, 72, 78));

    BookHisto("selected/hTrackMomentum", new TH1F("TrackMomentum", "Track momentum;Momentum [GeV/c]", 50, 0, 50));
    BookHisto("selected/hTrackX1",       new TH1F("TrackX1",  "Track x at Straw1;x [mm]", 50, -500, 500));
    BookHisto("selected/hTrackY1",       new TH1F("TrackY1",  "Track y at Straw1;y [mm]", 50, -500, 500));
    BookHisto("selected/hdxdz",          new TH1F("dxdz",     "Kaon dx/dz",  100, -0.0025, 0.0025));
    BookHisto("selected/hdydz",          new TH1F("dydz",     "Kaon dy/dz",  100, -0.0025, 0.0025));
    BookHisto("selected/hXYstart",       new TH2F("XYstart",  "Kaon (x,y) at GTK3 plane (z=102.4m);x [mm]; y[mm]",
						  50, -50, 50, 50, -50, 50));
    BookHisto("selected/hXstart",        new TH1F("Xstart", "Kaon x at GTK3 plane (z=102.4m);x [mm]", 100, -50, 50));
    BookHisto("selected/hYstart",        new TH1F("Ystart", "Kaon y at GTK3 plane (z=102.4m);y [mm]", 100, -50, 50));
    BookHisto("selected/hXstartVsT", new TProfile
	      ("XstartVsT", "Kaon x at GTK3 plane (z=102.4m) vs time in burst;Event time [ns];x [mm]", 25, 1, 6));
    BookHisto("selected/hYstartVsT", new TProfile
	      ("YstartVsT", "Kaon y at GTK3 plane (z=102.4m) vs time in burst;Event time [ns];y [mm]", 25, 1, 6));
    BookHisto("selected/hXY180m", new TH2F
	      ("XY180m",  "Kaon (x,y) at z=180m;x [mm]; y[mm]", 50, 50, 150, 50, -50, 50));
    BookHisto("selected/hX180m",         new TH1F("X180m", "Kaon x at z=180m;x [mm]", 100, 50, 150));
    BookHisto("selected/hY180m",         new TH1F("Y180m", "Kaon y at z=180m;y [mm]", 100, -50, 50));
    BookHisto("selected/hPhi",           new TH1F("Phi", "Odd track #varphi; #varphi/#pi", 100, -1, 1));
    BookHisto("selected/hdxdzVsPhi",     new TH2F("dxdzVsPhi", "dx/dz vs #varphi; #varphi/#pi; dx/dz",
						  20, -1, 1, 50, -0.0025, 0.0025));
    BookHisto("selected/hdydzVsPhi",     new TH2F("dydzVsPhi", "dy/dz vs #varphi; #varphi/#pi; dy/dz",
						  20, -1, 1, 50, -0.0025, 0.0025));
    BookHisto("selected/hdxdzVsXstart",  new TH2F("dxdzVsX", "dx/dz vs x; x [mm]; dx/dz", 20, -50, 50, 50, -0.0025, 0.0025));
    BookHisto("selected/hdydzVsXstart",  new TH2F("dydzVsX", "dy/dz vs x; x [mm]; dy/dz", 20, -50, 50, 50, -0.0025, 0.0025));
    BookHisto("selected/hdxdzVsYstart",  new TH2F("dxdzVsY", "dx/dz vs y; y [mm]; dx/dz", 20, -25, 25, 50, -0.0025, 0.0025));
    BookHisto("selected/hdydzVsYstart",  new TH2F("dydzVsY", "dy/dz vs y; y [mm]; dy/dz", 20, -25, 25, 50, -0.0025, 0.0025));
    BookHisto("selected/pdxdzVsXstart",  new TProfile("dxdzVsXP", "dx/dz vs x; x [mm]; dx/dz", 20, -50, 50));
    BookHisto("selected/pdydzVsXstart",  new TProfile("dydzVsXP", "dy/dz vs x; x [mm]; dy/dz", 20, -50, 50));
    BookHisto("selected/pdxdzVsYstart",  new TProfile("dxdzVsYP", "dx/dz vs y; y [mm]; dx/dz", 20, -25, 25));
    BookHisto("selected/pdydzVsYstart",  new TProfile("dydzVsYP", "dy/dz vs y; y [mm]; dy/dz", 20, -25, 25));

    // M3pi vs the phi angle: reconstructed events
    BookHisto("selected/hM3piVsPhi", new TH2F
	      ("M3piVsPhi", "M(3#pi) vs #varphi; #varphi/#pi; M(3#pi) [MeV]", 40, -1, 1, 28, 490, 497));
    BookHisto("selected/hM3piNoBFVsPhi", new TH2F
	      ("M3piNoBFVsPhi", "M(3#pi) vs #varphi, no blue tube corrections; #varphi/#pi; M(3#pi) [MeV]",
	       40, -1, 1, 28, 490, 497));
    BookHisto("selected/pM3piVsPhi", new TProfile
	      ("pM3piVsPhi", "M(3#pi) vs #varphi; #varphi/#pi; M(3#pi) [MeV]", 40, -1, 1));
    BookHisto("selected/pM3piNoBFVsPhi", new TProfile
	      ("pM3piNoBFVsPhi", "M(3#pi) vs #varphi, no blue tube corrections; #varphi/#pi; M(3#pi) [MeV]", 40, -1, 1));
    BookHisto("selected/NSharedRICHHits", new TH1D
	      ("NSharedRICHHits", "NSharedRICHHits", 20, -0.5, 19.5));

    // M3pi vs the phi angle: true MC events, this is a pure blue field effect
    BookHisto("mctrue/hM3piVsPhi", new TH2F
	      ("M3piVsPhi_true", "True M(3#pi) vs #varphi: pure blue field effect; #varphi/#pi; M(3#pi) [MeV]", 40, -1, 1, 30, 492, 495));
    BookHisto("mctrue/pM3piVsPhi", new TProfile
	      ("pM3piVsPhi_true", "True M(3#pi) vs #varphi: pure blue field effect; #varphi/#pi; M(3#pi) [MeV]", 40, -1, 1));

    BookHisto("general/hBurstID", new TH1F ("BurstID", "Burst ID;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hKaonRateArgonion", new TH1F
	      ("KaonRateArgonion", "Kaon rate (from Argonion) vs burst ID;Burst ID;Kaon rate [MHz]",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hKaonRateKTAG", new TH1F
	      ("KaonRateKTAG", "Kaon rate (from KTAG) vs burst ID;Burst ID;Kaon rate [MHz]",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hMinTimeInSpill", new TH1F
	      ("MinTimeInSpill", "MinTimeInSpill (physics & control triggers);Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hMaxTimeInSpill", new TH1F
	      ("MaxTimeInSpill", "MaxTimeInSpill (physics & control triggers);Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hEventsPerBurst", new TH1F
	      ("EventsPerBurst", "Events per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hEventsPerBurstQM0", new TH1F
	      ("EventsPerBurstQM0", "Events per burst (quality mask = 0);Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hControlEventsPerBurst", new TH1F
	      ("ControlEventsPerBurst", "Control events per burst;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hPhysicsEventsPerBurst", new TH1F
	      ("PhysicsEventsPerBurst", "Physics events per burst;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("general/hThreeTrackEventsPerBurst", new TH1F
	      ("ThreeTrackEventsPerBurst", "Events with at least 3 tracks per burst;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("selected/hK3piEventsPerBurst", new TH1F
	      ("K3piEventsPerBurst", "K3pi candidates per burst;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("selected/hK3piEventsPerBurstControlTrigger", new TH1F
	      ("K3piEventsPerBurstControlTrigger", "K3pi candidates per burst (control trigger)*DS;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("selected/hK3piEventsPerBurstControlTriggerQM0", new TH1F
	      ("K3piEventsPerBurstControlTriggerQM0", "K3pi candidates per burst (control trigger, quality mask = 0)*DS;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("selected/hK3piEventsPerBurstMultiTrackTrigger", new TH1F
	      ("K3piEventsPerBurstMultiTrackTrigger", "K3pi candidates per burst (multi-track trigger)*DS;Burst ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5));

    BookHisto("selected/hNCHODHits", new TH1F
	      ("NCHODHits", "Number of CHOD hits;Number of hits", 50, -0.5, 49.5));
    BookHisto("selected/hNRICHHits", new TH1F
	      ("NRICHHits", "Number of RICH hits;Number of hits", 100, -0.5, 99.5));
    BookHisto("selected/CedarTime", new TH1F
	      ("CedarTime", "Cedar Time - Mean vertex CHOD track time;Time [ns]", 200, -50, 50));
    BookHisto("selected/NCedarOctants", new TH1F
	      ("NCedarOctants", "Number of Cedar Octants in a candidate;N(octants)", 9, -0.5, 8.5));

    // LKr-related quantities
    BookHisto("LKr/hNLKrCells", new TH1F
	      ("NLKrCells", "Number of LKr cells with signal;Number of cells", 125, -0.5, 249.5));
    BookHisto("LKr/hNLKrClusters", new TH1F
	      ("NLKrClusters", "Number of LKr clusters;Number of clusters", 10, -0.5, 9.5));
    BookHisto("LKr/hLKrNAssociatedClusters", new TH1F
	      ("LKrNAssociatedClusters", "Number of LKr clusters associated to a track", 4, -0.5, 3.5));
    BookHisto("LKr/hLKrDDeadCell", new TH1F
	      ("LKrDDeadCell", "Track distance to nearest dead cell;Distance to deal cell [mm]",
	       150, 0, 3000));
    BookHisto("LKr/hLKrClusterEnergy", new TH1F
	      ("LKrClusterEnergy", "LKr cluster energy;Energy [GeV]", 100, 0, 50));
    BookHisto("LKr/hLKrClusterTime", new TH1F
	      ("LKrClusterTime", "LKr cluster time wrt trigger;Time [ns]", 200, -50, 50));
    BookHisto("LKr/hLKrCellTotalEnergy", new TH1F
	      ("LKrCellTotalEnergy",
	       "LKr total cell (E>40MeV) energy;Total cell energy [GeV]", 70, 0, 70));
    BookHisto("LKr/hLKrCellClusterTotalEnergy", new TH2F
	      ("LKrCellClusterTotalEnergy",
	       "LKr total cluster energy vs cell energy;Total cell (>40MeV) energy [GeV];Total cluster energy [GeV]",
	       70, 0, 70, 70, 0, 70));

    // Pion E/p studies
    BookHisto("LKr/hLKrEoP_All", new TH1F("LKrEoP_All", "Pion E/p;Track E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_All", new TH2F
	      ("LKrEoPVsMomentum_All", "Pion E/p vs momentum;Track momentum [GeV/c];Track E/p",
	       50, -50, 50, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoP", new TH1F("LKrEoP", "Pion E/p;Track E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum", new TH2F
	      ("LKrEoPVsMomentum", "Pion E/p vs momentum;Track momentum [GeV/c];Track E/p",
	       50, -50, 50, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoP_nomuv3", new TH1F("LKrEoP_nomuv3", "Pion E/p;Track E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_nomuv3", new TH2F
	      ("LKrEoPVsMomentum_nomuv3", "Pion E/p vs momentum;Track momentum [GeV/c];Track E/p",
	       50, -50, 50, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoP_DistDeadCell", new TH1F
	      ("LKrEoP_DistDeadCell", "Pion E/p;Pion E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_DistDeadCell", new TH2F
	      ("LKrEoPVsMomentum_DistDeadCell",
	       "Pion E/p vs momentum [D(DeadCell)>20mm];Pion momentum [GeV/c];Pion E/p",
	       50, -50, 50, 150, 0.0, 1.5));

    // Pion PID studies with the RICH
    BookHisto("LKr/hLKrEoPVsMomentum_RICH_pion", new TH2F
	      ("LKrEoPVsMomentum_RICH_pion", "Pion E/p vs momentum;Track momentum [GeV/c];Track E/p",
	       50, -50, 50, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_RICH_electron", new TH2F
	      ("LKrEoPVsMomentum_RICH_electron", "Pion E/p vs momentum;Track momentum [GeV/c];Track E/p",
	       50, -50, 50, 150, 0.0, 1.5));
    BookHisto("LKr/hRichHypothesisVsLKrEoP", new TH2F
	      ("RichHypothesisVsLKrEoP", "RICH hypothesis vs E/p;Track E/p;RICH hypothesis",
	       150, 0.0, 1.5, 6, -1.5, 4.5));
    BookHisto("LKr/hThetaPP_Mom_Hyp", new TH3F
	      ("ThetaPP_Mom_Hyp", "ThetaPP_Mom_Hyp;#Theta;Track momentum [GeV/c];RICH hypothesis",
	       10, 0, 0.02, 25, 0, 50, 6, -1.5, 4.5));
    BookHisto("LKr/hNSharedHits_Mom_Hyp", new TH3F
	      ("NSharedHits_Mom_Hyp", "NSharedHits_Mom_Hyp;Shared RICH hits;Track momentum [GeV/c];RICH hypothesis",
	       10, -0.5, 9.5, 25, 0, 50, 6, -1.5, 4.5));
    BookHisto("LKr/hThetaPP_Mom_Hyp90", new TH3F
	      ("ThetaPP_Mom_Hyp90", "ThetaPP_Mom_Hyp (E/p>0.90);#Theta;Track momentum [GeV/c];RICH hypothesis",
	       10, 0, 0.02, 25, 0, 50, 6, -1.5, 4.5));

    // Straw-GTK missing mass resolution studies
    BookHisto("selected/hMMiss", new TH2F
	      ("hMMiss", "Mmiss;From spectrometer [MeV];From recoil [MeV]",
	       100, 270, 370, 100, 270, 370));
    BookHisto("selected/hMMiss_res", new TH2F
	      ("hMMiss_res", "Mmiss_res;From spectrometer [MeV];From recoil - from spectrometer [MeV]",
	       50, 270, 370, 150, -15, 15));
    BookHisto("selected/hMMissGTK", new TH2F
	      ("hMMissGTK", "MmissGTK;From spectrometer [MeV];From recoil [MeV]",
	       100, 270, 370, 100, 270, 370));
    BookHisto("selected/hMMissGTK_res", new TH2F
	      ("hMMissGTK_res", "MmissGTK_res;From spectrometer [MeV];From recoil - from spectrometer [MeV]",
	       50, 270, 370, 150, -15, 15));

    /////////////////////////////
    // Trigger efficiency studies

    // L0-RICH efficiency measurement
    BookHisto("trigger/L0rich/L0_all_burst", new TH1F
	      ("_L0_all_burst", "_L0_all_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0rich/L0_all_run", new TH1F
	      ("_L0_all_run", "_L0_all_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));
    BookHisto("trigger/L0rich/L0_RICH_burst", new TH1F
	      ("_L0_RICH_burst", "_L0_RICH_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0rich/L0_RICH_run", new TH1F
	      ("_L0_RICH_run", "_L0_RICH_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));

    // L0-QX and L0-ELKr efficiency measurement
    BookHisto("trigger/L0/L0_all_burst", new TH1F
	      ("L0_all_burst", "L0_all_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0/L0_all_run", new TH1F
	      ("L0_all_run", "L0_all_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));
    BookHisto("trigger/L0/L0_all_ELKr", new TH1F
	      ("L0_all_ELKr", "L0_all_ELKr", 100, 0.0, 100.0));

    BookHisto("trigger/L0/L0_QX_burst", new TH1F
	      ("L0_QX_burst", "L0_QX_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0/L0_QX_run", new TH1F
	      ("L0_QX_run", "L0_QX_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));

    BookHisto("trigger/L0/L0_MultiTrack_burst", new TH1F
	      ("L0_MultiTrack_burst", "L0_MultiTrack_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0/L0_MultiTrack_run", new TH1F
	      ("L0_MultiTrack_run", "L0_MultiTrack_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));

    BookHisto("trigger/L0/L0_LKr20_burst", new TH1F
	      ("L0_LKr20_burst", "L0_LK20_burst", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L0/L0_LKr20_run", new TH1F
	      ("L0_LKr20_run", "L0_LK20_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));
    BookHisto("trigger/L0/L0_LKr20_ELKr", new TH1F
	      ("L0_LKr20_ELKr", "L0_LK20_ELKr", 100, 0.0, 100.0));

    // Emulated L0 efficiencies
    BookHisto("trigger/L0rich/L0_RICHemu_run", new TH1F
	      ("_L0_RICHemu_run", "_L0_RICHemu_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));
    BookHisto("trigger/L0/L0_QXemu_run", new TH1F
	      ("L0_QXemu_run", "L0_QXemu_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));
    BookHisto("trigger/L0/L0_MO1emu_run", new TH1F
	      ("L0_MO1emu_run", "L0_MO1emu_run", fMaxRunID-fMinRunID+1, fMinRunID-0.5, fMaxRunID+0.5));

    // L1 efficiencies
    BookHisto("trigger/L1/L1_all",       new TH1F("L1_all",       "L1_all",       fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L1/L1_KTAG_eff",  new TH1F("L1_KTAG_eff",  "L1_KTAG_eff",  fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L1/L1_LAV_eff",   new TH1F("L1_LAV_eff",   "L1_LAV_eff",   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("trigger/L1/L1_STRAW_eff", new TH1F("L1_STRAW_eff", "L1_STRAW_eff", fMaxNBursts, -0.5, fMaxNBursts-0.5));

    // Set up the online monitor
    CreateCanvas("K3piCanvas");
    PlacePlotOnCanvas("selected/hZvertex",  "K3piCanvas");
    PlacePlotOnCanvas("selected/hM3pi",     "K3piCanvas");
    PlacePlotOnCanvas("selected/hMomentum", "K3piCanvas");
    PlacePlotOnCanvas("selected/hdxdz",     "K3piCanvas");
    PlacePlotOnCanvas("selected/hdydz",     "K3piCanvas");
    PlacePlotOnCanvas("selected/hXYstart",  "K3piCanvas");
    SetUpdateInterval(50000);
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHZtrue    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "mctrue/Zvertex_true", true));
    fHMass     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/M3pi", true));
    fHMomentum = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/Momentum", true));
    fHdxdz     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/dxdz", true));
    fHdydz     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/dydz", true));
    fHx        = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/Xstart", true));
    fHy        = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/Ystart", true));
    fHBurstID  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/BurstID", true));
    fHEventsPerBurst =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/EventsPerBurst", true));
    fHPhysicsEventsPerBurst =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/PhysicsEventsPerBurst", true));
    fHControlEventsPerBurst =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/ControlEventsPerBurst", true));
    fHK3piEventsPerBurstControlTrigger =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/K3piEventsPerBurstControlTrigger", true));
    fHK3piEventsPerBurstControlTriggerQM0 =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/K3piEventsPerBurstControlTriggerQM0", true));
    fHK3piEventsPerBurstMultiTrackTrigger =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "selected/K3piEventsPerBurstMultiTrackTrigger", true));
    fHKaonRateKTAG = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/KaonRateKTAG", true));
    fHKaonRateArgonion = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "general/KaonRateArgonion", true));

    fTrigAllRICH   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0rich/_L0_all_burst", true));
    fTrigEffRICH   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0rich/_L0_RICH_burst", true));
    fTrigAllQX     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0/L0_all_burst", true));
    fTrigEffQX     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0/L0_QX_burst", true));
    fEmulatedL0QX  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0/L0_QXemu_run", true));
    fEmulatedL0All = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L0/L0_all_run", true));
    fTrigL1All     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L1/L1_all", true));
    fTrigL1KTAG    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L1/L1_KTAG_eff", true));
    fTrigL1LAV     = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L1/L1_LAV_eff", true));
    fTrigL1STRAW   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "trigger/L1/L1_STRAW_eff", true));
  }
}

void K3piSelection::InitOutput() {
  RegisterOutput("EventSelected",  &fEventSelected);
  RegisterOutput("VertexID",       &fVertexID);
  RegisterOutput("VertexPosition", &fVertexPosition);
  RegisterOutput("TotalMomentum",  &fTotalMomentum);
  RegisterOutput("M3pi",           &fM3pi);
  RegisterOutput("K3piTime",       &fK3piTime);
  RegisterOutput("TrackIndex0",    &fTrackIndex0);
  RegisterOutput("TrackIndex1",    &fTrackIndex1);
  RegisterOutput("TrackIndex2",    &fTrackIndex2);
}

void K3piSelection::Process(Int_t) {

  if (!fReadingData) return; // no action if reading own output in --histo mode

  // Initialize the outputs
  SetOutputState("EventSelected",  kOValid);
  SetOutputState("VertexPosition", kOValid);
  SetOutputState("TotalMomentum",  kOValid);
  SetOutputState("VertexID",       kOValid);
  SetOutputState("M3pi",           kOValid);
  SetOutputState("K3piTime",       kOValid);
  fEventSelected = false;
  fVertexID = -1;
  fVertexPosition = TVector3(0.0, 0.0, 0.0);
  fTotalMomentum  = TVector3(0.0, 0.0, 0.0);
  fM3pi = 0.0;
  fK3piTime = 0.0;
  fTrackIndex0 = -1;
  fTrackIndex1 = -1;
  fTrackIndex2 = -1;

  Double_t TimeInSpill = GetEventHeader()->GetTimeStamp()*1e-9*ClockPeriod;
  if (fTriggerConditions->IsControlTrigger(GetL0Data()) ||
      fTriggerConditions->IsPhysicsTrigger(GetL0Data())) {
    if (fMinTimeInSpill>TimeInSpill) fMinTimeInSpill = TimeInSpill;
    if (fMaxTimeInSpill<TimeInSpill) fMaxTimeInSpill = TimeInSpill;
  }

  Int_t BurstID = GetBurstID();
  FillHisto("general/hEventsPerBurst", BurstID);
  if (!GetEventHeader()->GetEventQualityMask() ||
      (GetEventHeader()->GetRunID()<6278 &&
       GetEventHeader()->GetEventQualityMask()==(0x1<<kGigaTracker))) {
    FillHisto("general/hEventsPerBurstQM0", BurstID);
  }

  Bool_t PhysicsTrigger = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  Bool_t ControlTrigger = fTriggerConditions->IsControlTrigger(GetL0Data());
  if (ControlTrigger) FillHisto("general/hControlEventsPerBurst", BurstID);
  if (PhysicsTrigger) FillHisto("general/hPhysicsEventsPerBurst", BurstID);

  //////////////////////////////////////////////////////
  // True beam properties at the GTK3 plane (z=102400mm)

  if (GetWithMC()) {
    Event *evt = GetMCEvent();
    if (evt->GetNKineParts()) {
      FillHisto("mctrue/hZvertex", 0.001*evt->GetKinePart(0)->GetEndPos().Z()); // [m]
      TVector3 Pos = evt->GetKinePart(0)->GetPosGigaTrackerExit(); // z=102400mm
      if (Pos.Z()>100000.0) { // has kaon has reached the checkpoint?
	TLorentzVector Mom = evt->GetKinePart(0)->GetMomGigaTrackerExit();
	Double_t dxdz   = Mom.X()/Mom.Z();
	Double_t dydz   = Mom.Y()/Mom.Z();
	Double_t Xstart = Pos.X();
	Double_t Ystart = Pos.Y();
	FillHisto("mctrue/hMomentum", 0.001*Mom.Vect().Mag()); // [GeV/c]
	for (Int_t i=1; i<evt->GetNKineParts(); i++) {
	  if (evt->GetKinePart(i)->GetParentIndex()==0) { // K+ daughter
	    FillHisto("mctrue/hTrackMomentum",
		      0.001*evt->GetKinePart(i)->GetInitialMomentum().Mag()); // [GeV/c]
	    FillHisto("mctrue/hTrackX1", evt->GetKinePart(i)->xAt(183508.));
	    FillHisto("mctrue/hTrackY1", evt->GetKinePart(i)->yAt(183508.));
	  }
	}
	FillHisto("mctrue/hdxdz", dxdz);
	FillHisto("mctrue/hdydz", dydz);
	FillHisto("mctrue/hXYstart", Xstart, Ystart);
	FillHisto("mctrue/hXstart", Xstart);
	FillHisto("mctrue/hYstart", Ystart);
	FillHisto("mctrue/hdxdzVsXstart", Xstart, dxdz);
	FillHisto("mctrue/hdydzVsXstart", Xstart, dydz);
	FillHisto("mctrue/hdxdzVsYstart", Ystart, dxdz);
	FillHisto("mctrue/hdydzVsYstart", Ystart, dydz);
	FillHisto("mctrue/pdxdzVsXstart", Xstart, dxdz);
	FillHisto("mctrue/pdydzVsXstart", Xstart, dydz);
	FillHisto("mctrue/pdxdzVsYstart", Ystart, dxdz);
	FillHisto("mctrue/pdydzVsYstart", Ystart, dydz);
      }

      ////////////////////////////////////////////////////////////////////////////////////
      // Angular dependence of the true 3pi mass: this is purely a blue field effect.
      // Acceptance (mainly chamber hole) is not taken into account, therefore
      // the observed phi-variation of the mass is smaller than in the reconstructed data.

      if (evt->GetNKineParts()==4) {
	if (evt->GetKinePart(1)->GetPosSpectrometerEntry().z()>1e5 &&
	    evt->GetKinePart(2)->GetPosSpectrometerEntry().z()>1e5 &&
	    evt->GetKinePart(3)->GetPosSpectrometerEntry().z()>1e5) { // all pions reached the checkpoint
	  TLorentzVector P3pi = TLorentzVector(0.0, 0.0, 0.0, 0.0);
	  for (Int_t i=1; i<=3; i++) P3pi += evt->GetKinePart(i)->GetMomSpectrometerEntry();
	  Double_t phi = evt->GetKinePart(1)->GetInitialMomentum().Phi() / TMath::Pi(); // odd pion
	  FillHisto("mctrue/hM3piVsPhi", phi, P3pi.M());
	  FillHisto("mctrue/pM3piVsPhi", phi, P3pi.M());
	}
      }
    }
  }

  /////////////////////////
  // Run the vertexing tool

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  std::vector<SpectrometerTrackVertex> Vertices =
    *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");

  // Compute the number of three-track vertices
  Int_t NThreeTrackVtx = 0;
  Int_t vtx_index = -1;
  for (UInt_t i=0; i<Vertices.size(); i++) {
    Int_t NTracks = Vertices[i].GetNTracks();
    if (NTracks==3) {
      FillHisto("general/hVertexTime", Vertices[i].GetTime()); // average STRAW leading time
      NThreeTrackVtx++;
      vtx_index = i;
    }
  }

  FillHisto("general/hNTracks", Tracks.size());
  FillHisto("general/hNVertices", NThreeTrackVtx);
  if (Tracks.size()>=3) FillHisto("general/hThreeTrackEventsPerBurst", BurstID);
  for (UInt_t i=0; i<Tracks.size(); i++) {
    FillHisto("general/hTrackXY_straw1", Tracks[i].xAt(183508.), Tracks[i].yAt(183508.));
    FillHisto("general/hTrackXY_straw2", Tracks[i].xAt(194066.), Tracks[i].yAt(194066.));
    FillHisto("general/hTrackXY_straw3", Tracks[i].xAt(204459.), Tracks[i].yAt(204459.));
    FillHisto("general/hTrackXY_straw4", Tracks[i].xAt(218885.), Tracks[i].yAt(218885.));
  }
  if (NThreeTrackVtx!=1) return; // require exactly one three-track vertex

  // Geometric acceptance for the tracks
  for (Int_t i=0; i<3; i++) {
    TRecoSpectrometerCandidate* Scand = Vertices[vtx_index].GetSpectrometerCandidate(i);
    //if (Scand->GetNChambers()!=4) return; // removed on 29/04/2018, acceptance change: 0.1192 --> 0.1536
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) return;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) return;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) return;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) return;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD))         return;
  }

  // Track separations in STRAW1 and LKr planes
  for (Int_t i=0; i<3; i++) {
    TRecoSpectrometerCandidate* Scand = Vertices[vtx_index].GetSpectrometerCandidate(i);
    double x1s = Scand->xAt(183508.0); // Straw1
    double y1s = Scand->yAt(183508.0);
    double x1c = Scand->xAt(241093.0); // LKr
    double y1c = Scand->yAt(241093.0);
    for (Int_t j=i+1; j<3; j++) {
      TRecoSpectrometerCandidate* Scand2 = Vertices[vtx_index].GetSpectrometerCandidate(j);
      double x2s = Scand2->xAt(183508.0); // Straw1
      double y2s = Scand2->yAt(183508.0);
      double x2c = Scand2->xAt(241093.0); // LKr
      double y2c = Scand2->yAt(241093.0);
      double rs  = sqrt((x1s-x2s)*(x1s-x2s)+(y1s-y2s)*(y1s-y2s)); // Straw1
      double rc  = sqrt((x1c-x2c)*(x1c-x2c)+(y1c-y2c)*(y1c-y2c)); // LKr
      FillHisto("general/DistanceStraw1", rs);
      FillHisto("general/DistanceLKr", rc);
    }
  }

  // Trigger reference time
  Double_t RefTime = GetEventHeader()->GetFineTime() * TdcCalib;

  // Compute the mean time of CHOD candidates associated to the vertex tracks.
  // Zero CHOD time is set for fast MC simulations (no CHOD detector).
  Double_t ChodTime = 0.0;
  if (!fFastSimulation) { // data or normal (not fast) MC: CHOD information exists
    Int_t Nchod = 0;
    for (Int_t i=0; i<3; i++) {
      Int_t iTrack = Vertices[vtx_index].GetTrackIndex(i);
      if (Tracks[iTrack].CHODAssociationExists()) {
	Nchod++;
	ChodTime += Tracks[iTrack].GetCHODTime();
      }
    }
    if (Nchod) ChodTime /= (1.0*Nchod);
    if (!Nchod) return; // need at least one track with defined CHOD time
  }

  if (ControlTrigger) FillHisto("general/hVertexTimeControl", ChodTime-RefTime);
  if (PhysicsTrigger) FillHisto("general/hVertexTimePhysics", ChodTime-RefTime);
  if (fabs(ChodTime-RefTime)>5.0) return;

  Int_t TotalCharge = Vertices[vtx_index].GetCharge();
  if (TotalCharge!=1) return;

  Double_t Chi2 = Vertices[vtx_index].GetChi2();
  FillHisto("general/hVertexChi2", Chi2);
  if (Chi2>25.0) return;

  Double_t Xvertex = Vertices[vtx_index].GetPosition().x();
  Double_t Yvertex = Vertices[vtx_index].GetPosition().y();
  Double_t Zvertex = Vertices[vtx_index].GetPosition().z();

  FillHisto("general/hZvertex0", 0.001*Zvertex); // [m]

  if (Zvertex<fZAcceptedMin || Zvertex>fZAcceptedMax) return;

  // Quantities ending with "0" are built from the original momenta, with
  // output of SpectrometerVertexBuider involving vertex fitting is not used

  TLorentzVector v[3], v0[3];
  Int_t index[3];
  for (Int_t i=0; i<3; i++) {
    index[i] = Vertices[vtx_index].GetTrackIndex(i);
    v[i].SetVectM(Vertices[vtx_index].GetTrackThreeMomentum(i), MPI);
    v0[i].SetVectM(Vertices[vtx_index].GetTrackThreeMomentum0(i), MPI);
  }

  TLorentzVector K3pi_FourMomentum  = v[0] + v[1] + v[2];
  TLorentzVector K3pi_FourMomentum0 = v0[0] + v0[1] + v0[2];
  TVector3       K3pi_Momentum      = K3pi_FourMomentum.Vect();
  Double_t       M3pi               = K3pi_FourMomentum.M();
  Double_t       M3pi0              = K3pi_FourMomentum0.M(); // vertex fitter not used
  Double_t       dxdz               = K3pi_Momentum.X()/K3pi_Momentum.Z();
  Double_t       dydz               = K3pi_Momentum.Y()/K3pi_Momentum.Z();

  // Longitudinal and transverse momenta with respect to the beam axis
  TVector3 BeamAxis = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  Double_t Pl       = (BeamAxis*K3pi_Momentum) / BeamAxis.Mag();
  Double_t Pt       = sqrt(K3pi_Momentum*K3pi_Momentum-Pl*Pl);

  FillHisto("general/hMomentum0", 0.001*K3pi_Momentum.Mag()); // [GeV/c]
  FillHisto("general/hPt0", Pt); // [MeV/c]
  if (fabs(K3pi_Momentum.Mag()-75000.0)>3000.0) return;

  ///////////////////////////////////////////////////////
  // Histogram the properties of the selected K3pi events

  FillHisto("selected/hM3pi",     M3pi);
  FillHisto("selected/hM3piNoBF", M3pi0); // vertex fitter not used
  if (ControlTrigger) {
    FillHisto("selected/hM3piControlTrigger",     M3pi);
    FillHisto("selected/hM3piNoBFControlTrigger", M3pi0);
  }
  if (M3pi<490.0 || M3pi>497.0) return; // last cut

  // K3pi yields per trigger: control and multi-track triggers.
  // Runs with variable or unknown control trigger downscaling cannot be processed: request DS>0.
  FillHisto("selected/hK3piEventsPerBurst", BurstID);
  if (ControlTrigger && fDownscaling1>0) {
    FillHisto("selected/hK3piEventsPerBurstControlTrigger", BurstID, 1.0*fDownscaling1);
    if (!GetEventHeader()->GetEventQualityMask() ||
        (GetEventHeader()->GetRunID()<6278 &&
	 GetEventHeader()->GetEventQualityMask()==(0x1<<kGigaTracker))) {
      FillHisto("selected/hK3piEventsPerBurstControlTriggerQM0", BurstID, 1.0*fDownscaling1);
    }
  }

  // Check that the event passes L0 and L1 triggers
  if (fTriggerConditions->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerMultiTrack) && fDownscaling2>0) {
    Int_t NAlgorithms = fTriggerConditions->GetNumberOfL1Algorithms
      (GetRunID(), GetL1Data(), fTriggerMultiTrack);
    Bool_t verdict = true;
    for (Int_t i=0; i<NAlgorithms; i++) {
      Bool_t verdict_of_algorithm =
	fTriggerConditions->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerMultiTrack, i) ||
	fTriggerConditions->L1TriggerInFlaggingMode(GetRunID(), GetL1Data(), fTriggerMultiTrack, i);
      verdict &= verdict_of_algorithm;
    }
    if (verdict) {
      FillHisto("selected/hK3piEventsPerBurstMultiTrackTrigger", BurstID, 1.0*fDownscaling2);
    }
  }

  Int_t OddTrackIndex = -1;
  for (Int_t i=0; i<3; i++) {
    if (Vertices[vtx_index].GetTrackCharge(i) != TotalCharge) OddTrackIndex = i;
  }
  Double_t phi  = v [OddTrackIndex].Vect().Phi() / TMath::Pi();
  Double_t phi0 = v0[OddTrackIndex].Vect().Phi() / TMath::Pi();

  FillHisto("selected/hZvertex", 0.001*Zvertex); // [m]
  FillHisto("selected/hMomentum", 0.001*K3pi_Momentum.Mag()); // [GeV/c]
  FillHisto("selected/hPt", Pt); // [MeV/c]
  if (ControlTrigger) {
    FillHisto("selected/hZvertexControlTrigger", 0.001*Zvertex); // [m]
    FillHisto("selected/hMomentumControlTrigger", 0.001*K3pi_Momentum.Mag()); // [GeV/c]
  }
  for (Int_t i=0; i<3; i++) {
    FillHisto("selected/hTrackMomentum", 0.001*Vertices[vtx_index].GetTrackThreeMomentum(i).Mag());
    FillHisto("selected/hTrackX1", Vertices[vtx_index].GetSpectrometerCandidate(i)->xAt(183508.));
    FillHisto("selected/hTrackY1", Vertices[vtx_index].GetSpectrometerCandidate(i)->yAt(183508.));
  }
  FillHisto("selected/hdxdz", dxdz);
  FillHisto("selected/hdydz", dydz);

  // Beam profile at the GTK3 plane: z = 102.4m
  Double_t Xstart = Xvertex + dxdz*(fZGTK3 - Zvertex);
  Double_t Ystart = Yvertex + dydz*(fZGTK3 - Zvertex);
  FillHisto("selected/hXYstart", Xstart, Ystart);
  FillHisto("selected/hXstart", Xstart);
  FillHisto("selected/hYstart", Ystart);
  if (fabs(Xstart)<35.0) FillHisto("selected/hXstartVsT", TimeInSpill, Xstart);
  if (fabs(Ystart)<20.0) FillHisto("selected/hYstartVsT", TimeInSpill, Ystart);

  // Beam profile at z = 180m
  Double_t X180m = Xvertex + dxdz*(180000.0 - Zvertex);
  Double_t Y180m = Yvertex + dydz*(180000.0 - Zvertex);
  FillHisto("selected/hXY180m",        X180m, Y180m);
  FillHisto("selected/hX180m",         X180m);
  FillHisto("selected/hY180m",         Y180m);
  FillHisto("selected/hPhi",           phi);
  FillHisto("selected/hdxdzVsPhi",     phi,    dxdz);
  FillHisto("selected/hdydzVsPhi",     phi,    dydz);
  FillHisto("selected/hdxdzVsXstart",  Xstart, dxdz);
  FillHisto("selected/hdydzVsXstart",  Xstart, dydz);
  FillHisto("selected/hdxdzVsYstart",  Ystart, dxdz);
  FillHisto("selected/hdydzVsYstart",  Ystart, dydz);
  FillHisto("selected/pdxdzVsXstart",  Xstart, dxdz);
  FillHisto("selected/pdydzVsXstart",  Xstart, dydz);
  FillHisto("selected/pdxdzVsYstart",  Ystart, dxdz);
  FillHisto("selected/pdydzVsYstart",  Ystart, dydz);
  FillHisto("selected/hM3piVsPhi",     phi,    M3pi);
  FillHisto("selected/hM3piNoBFVsPhi", phi0,   M3pi0);
  FillHisto("selected/pM3piVsPhi",     phi,    M3pi);
  FillHisto("selected/pM3piNoBFVsPhi", phi0,   M3pi0);

  // CHOD response studies
  TRecoCHODEvent* CHODevent = GetEvent<TRecoCHODEvent>();
  Int_t NCHODHits = CHODevent->GetNHits();
  FillHisto("selected/hNCHODHits", NCHODHits);

  // RICH response studies
  TRecoRICHEvent* RICHevent = GetEvent<TRecoRICHEvent>();
  Int_t NRICHHitsAll = RICHevent->GetNHits(); // including super-cells
  Int_t NRICHHits = 0;
  for (Int_t i=0; i<NRICHHitsAll; i++) {
    TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(RICHevent->GetHit(i));
    if (hit->GetOrSuperCellID()==0) NRICHHits++; // no super-cells
  }
  FillHisto("selected/hNRICHHits", NRICHHits);

  // Cedar/KTAG response studies: timing and number of sectors in coincidence
  Double_t CedarTime = -999.0, dT_CHOD_Cedar = 999.0;
  TRecoCedarEvent* CEDARevent = GetEvent<TRecoCedarEvent>();
  for (Int_t i=0; i<CEDARevent->GetNCandidates(); i++) {
    TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    Double_t dT = Ccand->GetTime() - ChodTime;
    FillHisto("selected/CedarTime", dT);
    if (fabs(dT)<2.0) FillHisto("selected/NCedarOctants", Ccand->GetNSectors());
    if (Ccand->GetNSectors()>=5 && fabs(dT)<fabs(dT_CHOD_Cedar)) {
      CedarTime = Ccand->GetTime();
      dT_CHOD_Cedar = dT;
    }
  }

  ////////////////////////////////////////////////////////
  // LKr and RICH response studies for a clean pion sample

  TRecoLKrEvent* LKRevent = GetEvent<TRecoLKrEvent>();
  Double_t TotalCellEnergy = 0.0; // [MeV]
  Double_t TotalInTimeClusterEnergy = 0.0; // [MeV]

  if (GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(0), kLKr) &&
      GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(1), kLKr) &&
      GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(2), kLKr) &&
      GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(0), kLAV, kLAV12) &&
      GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(1), kLAV, kLAV12) &&
      GeometricAcceptance::GetInstance()->InAcceptance
      (Vertices[vtx_index].GetSpectrometerCandidate(2), kLAV, kLAV12) &&
      fabs(K3pi_Momentum.Mag()-BeamParameters::GetInstance()->GetBeamThreeMomentum().Mag())<1500.0 &&
      Pt<30.0 && fabs(M3pi-MKCH)<2.0) {

    Int_t NCells = LKRevent->GetNHits();
    Int_t NClusters = LKRevent->GetNCandidates();
    FillHisto("LKr/hNLKrCells", NCells);
    FillHisto("LKr/hNLKrClusters", NClusters);

    for (Int_t i=0; i<NCells; i++) {
      TRecoLKrHit *hit = static_cast<TRecoLKrHit*>(LKRevent->GetHit(i));
      Double_t energy = hit->GetEnergy();
      if (energy>40.0) TotalCellEnergy += energy;
    }
    FillHisto("LKr/hLKrCellTotalEnergy", 0.001*TotalCellEnergy);
    FillHisto("LKr/hLKrCellClusterTotalEnergy", 0.001*TotalCellEnergy, 0.001*LKRevent->GetEnergyTotal());

    for (Int_t i=0; i<NClusters; i++) {
      TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKRevent->GetCandidate(i));
      FillHisto("LKr/hLKrClusterEnergy", 0.001*Lcand->GetClusterEnergy());
      FillHisto("LKr/hLKrClusterTime", Lcand->GetTime()-RefTime);
      if (fabs(Lcand->GetTime()-RefTime)<6.0)
	TotalInTimeClusterEnergy += Lcand->GetClusterEnergy();
    }

    if (fTriggerConditions->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerMultiTrack)) {
      for (Int_t i=0; i<3; i++) {
	FillHisto("LKr/hLKrNAssociatedClusters", Tracks[i].GetLKrNAssociatedClusters());
	FillHisto("LKr/hLKrDDeadCell", Tracks[i].GetLKrClusterDDeadCell());
      }

      // Track separations in STRAW and LKr planes
      Bool_t TrackSeparationsOK = true;
      for (Int_t i=0; i<3; i++) {
	Int_t iTrack = Vertices[vtx_index].GetTrackIndex(i);
	TRecoSpectrometerCandidate* Scand = Tracks[iTrack].GetSpectrometerCandidate();
	double x1s1 = Scand->xAt(183508.0); // Straw1
	double y1s1 = Scand->yAt(183508.0);
	double x1s2 = Scand->xAt(194066.0); // Straw2
	double y1s2 = Scand->yAt(194066.0);
	double x1s3 = Scand->xAt(204459.0); // Straw3
	double y1s3 = Scand->yAt(204459.0);
	double x1s4 = Scand->xAt(218885.0); // Straw4
	double y1s4 = Scand->yAt(218885.0);
	double x1c  = Scand->xAt(241093.0); // LKr
	double y1c  = Scand->yAt(241093.0);
	for (Int_t j=i+1; j<3; j++) {
	  Int_t jTrack = Vertices[vtx_index].GetTrackIndex(j);
	  TRecoSpectrometerCandidate* Scand2 = Tracks[jTrack].GetSpectrometerCandidate();
	  double x2s1 = Scand2->xAt(183508.0); // Straw1
	  double y2s1 = Scand2->yAt(183508.0);
	  double x2s2 = Scand2->xAt(194066.0); // Straw2
	  double y2s2 = Scand2->yAt(194066.0);
	  double x2s3 = Scand2->xAt(204459.0); // Straw3
	  double y2s3 = Scand2->yAt(204459.0);
	  double x2s4 = Scand2->xAt(218885.0); // Straw4
	  double y2s4 = Scand2->yAt(218885.0);
	  double x2c  = Scand2->xAt(241093.0); // LKr
	  double y2c  = Scand2->yAt(241093.0);
	  double rs1  = sqrt((x1s1-x2s1)*(x1s1-x2s1)+(y1s1-y2s1)*(y1s1-y2s1)); // Straw1
	  double rs2  = sqrt((x1s2-x2s2)*(x1s2-x2s2)+(y1s2-y2s2)*(y1s2-y2s2)); // Straw2
	  double rs3  = sqrt((x1s3-x2s3)*(x1s3-x2s3)+(y1s3-y2s3)*(y1s3-y2s3)); // Straw3
	  double rs4  = sqrt((x1s4-x2s4)*(x1s4-x2s4)+(y1s4-y2s4)*(y1s4-y2s4)); // Straw4
	  double rc   = sqrt((x1c-x2c)*(x1c-x2c)+(y1c-y2c)*(y1c-y2c)); // LKr
	  if (rs1<15.0 || rs2<15.0 || rs3<15.0 || rs4<15.0 || rc<200.0) TrackSeparationsOK = false;
	}
      }

      // The "CHOD veto" condition
      Bool_t CHODShower = Tracks[0].isCHODShowerLikeEvent();

      // The LAV veto condition
      TRecoLAVEvent* LAVevent = GetEvent<TRecoLAVEvent>();
      LAVMatching* pLAVMatching = *GetOutput<LAVMatching*>("PhotonVetoHandler.LAVMatching");
      pLAVMatching->SetReferenceTime(ChodTime);
      pLAVMatching->SetTimeCuts(4.0, 4.0);
      Bool_t LAVmatched = pLAVMatching->LAVHasTimeMatching(LAVevent);

      // The LKr veto condition
      /*
      Double_t LKR_largest_energy = 0.01;
      for (Int_t i=0; i<LKRevent->GetNCandidates(); i++) { // cluster loop
	TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKRevent->GetCandidate(i));
	Double_t LKrTime   = Lcand->GetTime() - ChodTime;
	Double_t LKrEnergy = Lcand->GetClusterEnergy();
	Double_t Rmin = 99999;
	for (Int_t j=0; j<3; j++) { // track loop
	  Int_t iTrack = Vertices[vtx_index].GetTrackIndex(j);
	  Double_t xtr = Tracks[iTrack].xAtAfterMagnet(241093.0);
	  Double_t ytr = Tracks[iTrack].yAtAfterMagnet(241093.0);
	  Double_t dx  = Lcand->GetClusterX() - xtr;
	  Double_t dy  = Lcand->GetClusterY() - ytr;
	  Double_t R   = sqrt(dx*dx+dy*dy);
	  if (R<Rmin) Rmin = R;
	}
	if (fabs(LKrTime)<10.0 && Rmin>200.0) {
	  if (LKrEnergy>LKR_largest_energy) LKR_largest_energy = LKrEnergy;
	}
      }
      Bool_t LKr_OK = (LKR_largest_energy<3000); // [MeV]
      */

      // Require limited CHOD activity, LAV matching, track separations in STRAW and LKr
      if (!CHODShower && !LAVmatched && TrackSeparationsOK) {

	// RICH hypotheses:
	// kRICHHypothesisBackground  0
	// kRICHHypothesisElectron    1
	// kRICHHypothesisMuon        2
	// kRICHHypothesisPion        3
	// kRICHHypothesisKaon        4
	// kRICHHypothesisMultiple   99

	Int_t q[3], hyp[3];
	Double_t qp[3], eop[3];
	TVector3 dir_after_magnet[3];
	for (Int_t i=0; i<3; i++) {
	  Int_t iTrack = Vertices[vtx_index].GetTrackIndex(i);
	  q[i]   = Tracks[iTrack].GetCharge();
	  qp[i]  = Tracks[iTrack].GetChargeTimesMomentum()*0.001; // [GeV/c]
	  eop[i] = Tracks[iTrack].GetLKrEoP();
	  hyp[i] = Tracks[iTrack].GetRICHMostLikelyHypothesis();
	  if (hyp[i]==kRICHHypothesisMultiple) hyp[i] = -1; // redefine (99 --> -1) for plotting
	  dir_after_magnet[i] = Tracks[iTrack].GetMomentumAfterMagnet();
	}

	// The angle between the two positive tracks in the RICH
	Double_t ThetaPP = 999;
	for (int i=0; i<3; i++) {
	  for (int j=i+1; j<3; j++) {
	    if (q[i]>0 && q[j]>0) {
	      Double_t CosThetaPP =
		(dir_after_magnet[i]*dir_after_magnet[j]) /
		(dir_after_magnet[i].Mag()*dir_after_magnet[j].Mag());
	      if (CosThetaPP>1.0) CosThetaPP = 1.0;
	      ThetaPP = acos(CosThetaPP);
	    }
	  }
	}

	// The number of shared RICH hits by the two positive tracks in the most likely hypotheses
	Int_t NSharedHits = 0;
	for (Int_t i=0; i<3; i++) {
	  for (Int_t j=i+1; j<3; j++) {
	    // two positive tracks identified by the RICH (multiple hypotheses excluded!)
	    if (q[i]>0 && q[j]>0 && hyp[i]>=0 && hyp[j]>=0) {
	      Int_t iTrack1 = Vertices[vtx_index].GetTrackIndex(i);
	      Int_t iTrack2 = Vertices[vtx_index].GetTrackIndex(j);
	      std::vector<TRecoRICHHit*> Hits1 = Tracks[iTrack1].GetRICHAssignedHits(hyp[i]);
	      std::vector<TRecoRICHHit*> Hits2 = Tracks[iTrack2].GetRICHAssignedHits(hyp[j]);
	      for (UInt_t ihit1=0; ihit1<Hits1.size(); ihit1++) {
		for (UInt_t ihit2=0; ihit2<Hits2.size(); ihit2++) {
		  if (Hits1[ihit1]==Hits2[ihit2]) NSharedHits++;
		}
	      }
	    }
	  }
	}
	FillHisto("selected/NSharedRICHHits", NSharedHits);
	if (NSharedHits>9) NSharedHits = 9; // to match the upper limit of the histogram

	// Fill histograms of LKr+RICH response to pions
	for (Int_t i=0; i<3; i++) {
	  Int_t iTrack = Vertices[vtx_index].GetTrackIndex(i);
	  if (Tracks[iTrack].GetChi2()<40.0) {
	    // This is the original E/p
	    FillHisto("LKr/hLKrEoP_All", eop[i]);
	    FillHisto("LKr/hLKrEoPVsMomentum_All", qp[i], eop[i]);
	    // E/p is redefined for tracks with multiple clusters associated
	    if (Tracks[iTrack].GetLKrNAssociatedClusters()>1) eop[i] = 1.495;
	    FillHisto("LKr/hLKrEoP", eop[i]);
	    FillHisto("LKr/hLKrEoPVsMomentum", qp[i], eop[i]);
	    if (!Tracks[iTrack].MUV3AssociationExists()) {
	      FillHisto("LKr/hLKrEoP_nomuv3", eop[i]);
	      FillHisto("LKr/hLKrEoPVsMomentum_nomuv3", qp[i], eop[i]);
	    }
	    if (Tracks[iTrack].GetLKrClusterDDeadCell()>20.0) {
	      FillHisto("LKr/hLKrEoP_DistDeadCell", eop[i]);
	      FillHisto("LKr/hLKrEoPVsMomentum_DistDeadCell", qp[i], eop[i]);
	    }
	    if (hyp[i]==kRICHHypothesisPion) {
	      FillHisto("LKr/hLKrEoPVsMomentum_RICH_pion", qp[i], eop[i]);
	    }
	    if (hyp[i]==kRICHHypothesisElectron) {
	      FillHisto("LKr/hLKrEoPVsMomentum_RICH_electron", qp[i], eop[i]);
	    }
	    if (q[i]>0) {
	      FillHisto("LKr/hRichHypothesisVsLKrEoP", eop[i], hyp[i]);
	      FillHisto("LKr/hThetaPP_Mom_Hyp", ThetaPP, qp[i], hyp[i]);
	      FillHisto("LKr/hNSharedHits_Mom_Hyp", NSharedHits, qp[i], hyp[i]);
	      if (eop[i]>0.9 && eop[i]<1.1) FillHisto("LKr/hThetaPP_Mom_Hyp90", ThetaPP, qp[i], hyp[i]);
	    }
	  }
	}
      }
    }
  }

  //////////////////////////////////
  // Missing mass resolution studies

  Double_t Mpipi = (v[0] + v[1]).M(); // two random pions
  TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  TLorentzVector Kaon;
  Kaon.SetVectM(KaonThreeMomentum, MKCH);
  Double_t MMiss = (Kaon-v[2]).M(); // the third pion
  FillHisto("selected/hMMiss",     Mpipi, MMiss);
  FillHisto("selected/hMMiss_res", Mpipi, MMiss-Mpipi);

  // Match the third STRAW pion track to a GTK track
  if (fabs(CedarTime)<5.0) { // matching Cedar candidate found
    TRecoGigaTrackerEvent* GTKevent = GetEvent<TRecoGigaTrackerEvent>();
    fSG->Match(GTKevent, Tracks[index[2]].GetSpectrometerCandidate(), CedarTime, kCedar);
    if (fSG->SecondBestGTKTrackFound() && fSG->GetBestDiscriminant()>0.01 && fSG->GetBestCDA()<7.0) {
      TLorentzVector KaonGTK; // beam kaon
      KaonGTK.SetVectM(fSG->GetBestCorrectedBeamMomentum(), MKCH);
      TLorentzVector PionGTK; // downstream pion
      PionGTK.SetVectM(fSG->GetBestCorrectedTrackMomentum(), MPI);
      Double_t MMissGTK = (KaonGTK-PionGTK).M();
      FillHisto("selected/hMMissGTK",     Mpipi, MMissGTK);
      FillHisto("selected/hMMissGTK_res", Mpipi, MMissGTK-Mpipi);
    }
  }

  /////////////////////////////////////
  // L0 trigger efficiency measurements

  if (ControlTrigger) {
    fPrimitiveHandler->SetData(GetL0Data(), GetRunID());
    //fPrimitiveHandler->PrintKnownBitNames(GetRunID());

    Bool_t RICHCondition    = fPrimitiveHandler->CheckCondition(kL0RICH);
    Bool_t NewCHODCondition = fPrimitiveHandler->CheckCondition(kL0NewCHOD);
    if (GetWithMC()) RICHCondition = NewCHODCondition = true;

    // Efficiencies of all L0 conditions except L0-RICH
    if (RICHCondition) {
      Int_t RichTime = fPrimitiveHandler->GetTriggerTime(kL0RICH);
      FillHisto("trigger/L0/L0_all_burst", BurstID);
      FillHisto("trigger/L0/L0_all_run", GetRunID());
      FillHisto("trigger/L0/L0_all_ELKr", 0.001*TotalInTimeClusterEnergy);

      // Real primitives (data only)
      Bool_t QX_ok    = fPrimitiveHandler->CheckPrimitives("QX", RichTime);
      Bool_t LKr20_ok = (GetRunID()<=6726) ?
	fPrimitiveHandler->CheckPrimitives("LKr", RichTime) : // 2016 data
	fPrimitiveHandler->CheckPrimitives("E20", RichTime);  // 2017-18 data

      // Emulated primitives (data and MC)
      Bool_t QX_ok_emu   = fPrimitiveHandler->CheckEmulatedPrimitives("QX",  RichTime);
      Bool_t MUV3_ok_emu = fPrimitiveHandler->CheckEmulatedPrimitives("MO1", RichTime);

      if (QX_ok) { // measured QX
	FillHisto("trigger/L0/L0_QX_burst", BurstID);
	FillHisto("trigger/L0/L0_QX_run", GetRunID());
	// check if L0TP sets the trigger bit if RICH and QX primitives are found
	if (fTriggerConditions->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerMultiTrack)) {
	  FillHisto("trigger/L0/L0_MultiTrack_burst", BurstID);
	  FillHisto("trigger/L0/L0_MultiTrack_run", GetRunID());
	}
      }
      if (LKr20_ok) { // measured LKr20
	FillHisto("trigger/L0/L0_LKr20_burst", BurstID);
	FillHisto("trigger/L0/L0_LKr20_run", GetRunID());
	FillHisto("trigger/L0/L0_LKr20_ELKr", 0.001*TotalInTimeClusterEnergy);
      }
      if (QX_ok_emu) { // emulated QX
	FillHisto("trigger/L0/L0_QXemu_run", GetRunID());
      }
      if (MUV3_ok_emu) { // emulated MO1
	FillHisto("trigger/L0/L0_MO1emu_run", GetRunID());
      }
    }

    // L0-RICH efficiency measurement
    if (NewCHODCondition) {
      FillHisto("trigger/L0rich/L0_all_burst", BurstID);
      FillHisto("trigger/L0rich/L0_all_run", GetRunID());

      Int_t NewChodTime  = fPrimitiveHandler->GetTriggerTime(kL0NewCHOD);
      Bool_t RICH_ok     = fPrimitiveHandler->CheckPrimitives("RICH", NewChodTime);
      Bool_t RICH_ok_emu = fPrimitiveHandler->CheckEmulatedPrimitives("RICH", NewChodTime); 

      // Real primitives (data only)
      if (RICH_ok) {
	FillHisto("trigger/L0rich/L0_RICH_burst", BurstID);
	FillHisto("trigger/L0rich/L0_RICH_run", GetRunID());
      }

      // Emulated primitives (data and MC)
      if (RICH_ok_emu) {
	FillHisto("trigger/L0rich/L0_RICHemu_run", GetRunID());
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////
  // L1 trigger efficiency measurement based on L0 multi-track, L1 autopass events;
  // the L1 chain for K3pi decays is KTAG*(LAV*)STRAW. This works for data only.

  if (fTriggerConditions->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerMultiTrack) &&
      fTriggerConditions->L1TriggerAutopass(GetEventHeader())) {

    Int_t NAlgo = fTriggerConditions->GetNumberOfL1Algorithms(GetRunID(), GetL1Data(), fTriggerMultiTrack);
    FillHisto("trigger/L1/L1_all", BurstID);
    if (fTriggerConditions->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerMultiTrack, 0)) {
      FillHisto("trigger/L1/L1_KTAG_eff", BurstID);
      if (NAlgo==3) { // KTAG*LAV*STRAW; LAV can be in flagging mode (so 100% efficient)
	if (fTriggerConditions->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerMultiTrack, 1) ||
	    fTriggerConditions->L1TriggerInFlaggingMode(GetRunID(), GetL1Data(), fTriggerMultiTrack, 1)) {
	  FillHisto("trigger/L1/L1_LAV_eff", BurstID);
	  if (fTriggerConditions->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerMultiTrack, 2)) {
	    FillHisto("trigger/L1/L1_STRAW_eff", BurstID);
	  }
	}
      }
      else if (NAlgo==2) { // KTAG*STRAW
	FillHisto("trigger/L1/L1_LAV_eff", BurstID); // LAV is disabled: put an artificial 100% efficiency
	if (fTriggerConditions->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerMultiTrack, 1)) {
	  FillHisto("trigger/L1/L1_STRAW_eff", BurstID);
	}
      }
    }
  }

  // Save the outputs of successful selection
  fEventSelected  = true;
  fVertexID       = vtx_index;
  fVertexPosition = TVector3(Xvertex, Yvertex, Zvertex);
  fTotalMomentum  = K3pi_Momentum;
  fM3pi           = M3pi;
  fK3piTime       = ChodTime;
  fTrackIndex0    = index[0];
  fTrackIndex1    = index[1];
  fTrackIndex2    = index[2];
}

void K3piSelection::EndOfJobUser() {

  if (fReadingData) { // Data mode: save output
    for (Int_t i=1; i<=fMaxNBursts; i++) {
      fHisto.GetHisto("general/hKaonRateArgonion")->SetBinError(i, 0);
      fHisto.GetHisto("general/hMinTimeInSpill")->SetBinError(i, 0);
      fHisto.GetHisto("general/hMaxTimeInSpill")->SetBinError(i, 0);
    }
    SaveAllPlots();
    return;
  }
  if (!fHMomentum) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }

  /////////////////////////////////////
  // Histo mode: analyze the histograms

  fFMomentum = new TF1("fFMomentum", "gaus", 74.0, 76.0);
  fHMomentum->Fit(fFMomentum, "R0Q");
  fFdxdz = new TF1("fFdxdz", "gaus", 0.0009, 0.0015); // nominal beam axis: dx/dz=0.0012
  fHdxdz->Fit(fFdxdz, "R0Q");
  fFdydz = new TF1("fFdydz", "gaus", -0.0002, 0.0002); // nominal beam axis: dy/dz=0
  fHdydz->Fit(fFdydz, "R0Q");

  // Set ranges where beam mean x and y coordinates at z=102.4m are computed
  fHx->GetXaxis()->SetRangeUser(-35.0, 35.0);
  fHy->GetXaxis()->SetRangeUser(-20.0, 20.0);

  Int_t RunID = GetRunID();

  ofstream BeamParsFile;
  BeamParsFile.open(Form("BeamParameters.run%06d_0000-run%06d_9999.dat", RunID, RunID));
  BeamParsFile << Form("%06d %5d %6.3fe-3 %6.3fe-3 %5.2f %5.2f\n", RunID, (Int_t)(1e3*fFMomentum->GetParameter(1)),
      1.e3*fFdxdz->GetParameter(1), 1.e3*fFdydz->GetParameter(1), fHx->GetMean(), fHy->GetMean());
  BeamParsFile.close();

  cout << user_normal() << "RunID, N(K3pi), beam momentum [MeV/c], dx/dz, dy/dz, x [mm], y [mm]" << endl;
  cout << user_normal() << Form
    ("FinalResult %5d %8d %5d %6.3fe-3 %6.3fe-3 %5.2f %5.2f\n",
     RunID, (Int_t)fHMomentum->Integral(), (Int_t)(1e3*fFMomentum->GetParameter(1)),
     1e3*fFdxdz->GetParameter(1), 1e3*fFdydz->GetParameter(1),
     fHx->GetMean(), fHy->GetMean());

  //////////////////////////////////////////////////////////////////
  // Ndec is the number of kaon decays in the fiducial decay region.
  // NK is the number of kaons at the entrance to the decay volume.
  // POT is the number of protons on target.

  if (fPrintFlux) {
    cout << user_normal() << "-------------------------------------------------" << endl;
    cout << user_normal() << "RunID BurstID TotalTriggers N(K decays in FV) POT" << endl;
  }

  Int_t NumberOfBursts = (Int_t)fHBurstID->Integral();
  Int_t NumberOfNonEmptyBursts = 0;
  Int_t NumberOfBurstsWithPhysicsTriggers = 0;
  Int_t NumberOfBurstsWithControlTriggers = 0;
  Int_t NumberOfBurstsWithControlK3piEvents = 0;
  Int_t NumberOfBurstsWithControlK3piEventsQM0 = 0; // events with quality mask = 0
  Double_t CTL_Ndec_PerBurst[5000], CTL_NK_PerBurst[5000], CTL_POT_PerBurst[5000];
  Double_t CTL_dNdec_PerBurst[5000];
  Double_t MUL_Ndec_PerBurst[5000], MUL_NK_PerBurst[5000], MUL_POT_PerBurst[5000];
  Double_t MUL_dNdec_PerBurst[5000];
  Double_t NK_PerBurst_fromKTAG[5000], Ndec_PerBurst_fromKTAG[5000];
  Double_t NK_PerBurst_fromArgo[5000], Ndec_PerBurst_fromArgo[5000];

  Int_t MaxNonEmptyBurstID = 0;
  for (Int_t i=0; i<fMaxNBursts; i++) {
    if (fHEventsPerBurst->GetBinContent(i+1)>0) {
      NumberOfNonEmptyBursts++;
      MaxNonEmptyBurstID = i;
    }
    if (fHPhysicsEventsPerBurst->GetBinContent(i+1)>0) NumberOfBurstsWithPhysicsTriggers++;
    if (fHControlEventsPerBurst->GetBinContent(i+1)>0) NumberOfBurstsWithControlTriggers++;
    if (fHK3piEventsPerBurstControlTrigger->GetBinContent(i+1)>0)
      NumberOfBurstsWithControlK3piEvents++;
    if (fHK3piEventsPerBurstControlTriggerQM0->GetBinContent(i+1)>0)
      NumberOfBurstsWithControlK3piEventsQM0++; // events with quality mask = 0

    // Integrated kaon flux evaluated with control triggers.
    // Zero for runs with disabled control triggers or variable downscaling of control trigger.
    CTL_Ndec_PerBurst[i]  = fHK3piEventsPerBurstControlTrigger->GetBinContent(i+1) / fBRK3pi / fAcceptance;
    CTL_dNdec_PerBurst[i] = sqrt(fDownscaling1 * fHK3piEventsPerBurstControlTrigger->GetBinContent(i+1)) / fBRK3pi / fAcceptance;
    CTL_NK_PerBurst[i]    = CTL_Ndec_PerBurst[i] / fDecayProb; // number of kaons entering the decay volume
    CTL_POT_PerBurst[i]   = CTL_NK_PerBurst[i] * fPOT_to_Kaon;

    // Integrated kaon flux evaluated with control triggers, quality mask = 0
    //CTL_QM0_Ndec_PerBurst[i] = fHK3piEventsPerBurstControlTriggerQM0->GetBinContent(i+1) / fBRK3pi / fAcceptance;
    //CTL_QM0_NK_PerBurst[i]   = CTL_QM0_Ndec_PerBurst[i] / fDecayProb; // number of kaons entering the decay volume
    //CTL_QM0_POT_PerBurst[i]  = CTL_QM0_NK_PerBurst[i] * fPOT_to_Kaon;

    // Integrated kaon flux evaluated with multi-track
    MUL_Ndec_PerBurst[i] = fHK3piEventsPerBurstMultiTrackTrigger->GetBinContent(i+1) / fBRK3pi / fAcceptance;
    MUL_dNdec_PerBurst[i] = sqrt(fDownscaling2 * fHK3piEventsPerBurstControlTrigger->GetBinContent(i+1)) / fBRK3pi / fAcceptance;
    MUL_NK_PerBurst[i]   = MUL_Ndec_PerBurst[i] / fDecayProb; // number of kaons entering the decay volume
    MUL_POT_PerBurst[i]  = MUL_NK_PerBurst[i] * fPOT_to_Kaon;

    // Number of kaons entering the decay volume computed from KTAG signal rate
    if(fHKaonRateKTAG) NK_PerBurst_fromKTAG[i] = fSpillLength * fHKaonRateKTAG->GetBinContent(i+1) * 1e6;
    Ndec_PerBurst_fromKTAG[i] = NK_PerBurst_fromKTAG[i] * fDecayProb;

    // Number of kaons entering the decay volume computed from Argonion count
    if(fHKaonRateArgonion) NK_PerBurst_fromArgo[i] = fSpillLength * fHKaonRateArgonion->GetBinContent(i+1) * 1e6;
    Ndec_PerBurst_fromArgo[i] = NK_PerBurst_fromArgo[i] * fDecayProb;

    if (CTL_NK_PerBurst[i]<1.0 && MUL_NK_PerBurst[i]<1.0) continue; // discard bursts with no selected K3pi events

    if (fPrintFlux) {
      cout << user_normal() << Form
	("PerBurst %05d %04d %6d %6.3fe+06 %5.3fe+06 %6.3fe+12 %5.3fe+06 %6.3fe+06 %5.3fe+12 %6.3fe+06 %6.3fe+06\n",
	 RunID, i, (Int_t)fHEventsPerBurst->GetBinContent(i+1),
	 1e-6*CTL_Ndec_PerBurst[i], 1e-6*CTL_NK_PerBurst[i], 1e-12*CTL_POT_PerBurst[i],
	 1e-6*MUL_Ndec_PerBurst[i], 1e-6*MUL_NK_PerBurst[i], 1e-12*MUL_POT_PerBurst[i],
	 1e-6*Ndec_PerBurst_fromKTAG[i], 1e-6*Ndec_PerBurst_fromArgo[i]);
    }
  }

  if (fPrintFlux) cout << user_normal() << "-------------------------------------------------" << endl;

  // K flux with control trigger
  Double_t CTL_Ndec = fHK3piEventsPerBurstControlTrigger->Integral() / fBRK3pi / fAcceptance;
  Double_t CTL_NK   = CTL_Ndec / fDecayProb;
  Double_t CTL_POT  = CTL_NK * fPOT_to_Kaon;

  // K flux with control trigger, quality mask = 0
  Double_t CTL_QM0_Ndec = fHK3piEventsPerBurstControlTriggerQM0->Integral() / fBRK3pi / fAcceptance;
  Double_t CTL_QM0_NK   = CTL_QM0_Ndec / fDecayProb;
  Double_t CTL_QM0_POT  = CTL_QM0_NK * fPOT_to_Kaon;

  // K flux with multi-track trigger
  Double_t MUL_Ndec = fHK3piEventsPerBurstMultiTrackTrigger->Integral() / fBRK3pi / fAcceptance;
  Double_t MUL_NK   = MUL_Ndec / fDecayProb;
  Double_t MUL_POT  = MUL_NK * fPOT_to_Kaon;

  // Number of kaons entering the decay volume [mln]
  Double_t NK_fromKTAG = 0.0;
  Double_t NK_fromArgo = 0.0;
  if (fHKaonRateKTAG)     NK_fromKTAG = fSpillLength * fHKaonRateKTAG->Integral()     * 1e6;
  if (fHKaonRateArgonion) NK_fromArgo = fSpillLength * fHKaonRateArgonion->Integral() * 1e6;
  Double_t POT_fromKTAG = NK_fromKTAG * fPOT_to_Kaon;
  Double_t POT_fromArgo = NK_fromArgo * fPOT_to_Kaon;

  cout << user_normal() << "N(trig)= " << fHEventsPerBurst->Integral() << endl;
  cout << user_normal() << "N(CTL_decays_in_FV) NK= " << CTL_Ndec <<
    " N0= " << CTL_NK << " POT= " << CTL_POT << endl;
  cout << user_normal() << "N(CTL_decays_in_FV_QualityMaskOK) NK= " << CTL_QM0_Ndec <<
    " N0= " << CTL_QM0_NK << " POT= " << CTL_QM0_POT << endl;
  cout << user_normal() << "N(MUL_decays_in_FV) NK= " << MUL_Ndec <<
    " N0= " << MUL_NK << " POT= " << MUL_POT << endl;
  cout << user_normal() << "N(KTAG_decays_in_FV) NK= " << NK_fromKTAG*fDecayProb <<
    " N0= " << NK_fromKTAG << " POT= " << POT_fromKTAG << endl;
  cout << user_normal() << "N(Argonion_decays_in_FV) NK= " << NK_fromArgo*fDecayProb <<
    " N0= " << NK_fromArgo << " POT= " << POT_fromArgo << endl;

  // Write all the info in a .dat file
  ofstream K3piInfoFile;
  TString K3piInfoFileName = "K3piInfo.AllBursts.dat";
  if(!Configuration::ConfigSettings::CLI::fNoSkipBadBurst) K3piInfoFileName = "K3piInfo.NoBadBursts.dat";  // skipping bad bursts
  K3piInfoFile.open(K3piInfoFileName);
  K3piInfoFile << "# Format:" << endl;
  K3piInfoFile << "# Line 1: CTL_QM0  RunID StartOfRunTime NK3pi(CTL_QM0) NK(CTL_decays_in_FV_QM0) N0(CTL_decays_in_FV_QM0) POT(CTL_decays_in_FV_QM0)" << endl;
  K3piInfoFile << "# Line 2: CTL_all  RunID StartOfRunTime NK3pi(CTL_all) NK(CTL_decays_in_FV_all) N0(CTL_decays_in_FV_all) POT(CTL_decays_in_FV_all)" << endl;
  K3piInfoFile << "# Line 3: MUL_all  RunID StartOfRunTime NK3pi(MUL_all) NK(MUL_decays_in_FV_all) N0(MUL_decays_in_FV_all) POT(MUL_decays_in_FV_all)" << endl;
  K3piInfoFile << Form("CTL_QM0  %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHK3piEventsPerBurstControlTriggerQM0->Integral(),CTL_QM0_Ndec,CTL_QM0_NK,CTL_QM0_POT) << endl;
  K3piInfoFile << Form("CTL_all  %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHK3piEventsPerBurstControlTrigger->Integral(),CTL_Ndec,CTL_NK,CTL_POT) << endl;
  K3piInfoFile << Form("MUL_all  %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHK3piEventsPerBurstMultiTrackTrigger->Integral(),MUL_Ndec,MUL_NK,MUL_POT) << endl;
  K3piInfoFile.close();

  ofstream NKInfoFile;
  TString NKInfoFileName = "NKInfo.AllBursts.dat";
  if (!Configuration::ConfigSettings::CLI::fNoSkipBadBurst) NKInfoFileName = "NKInfo.NoBadBursts.dat";  // skipping bad bursts
  NKInfoFile.open(NKInfoFileName);
  NKInfoFile << "# Format:" << endl;
  NKInfoFile << "# Line 1: KTAG_all RunID StartOfRunTime N(trig) NK(KTAG_decays_in_FV_all) N0(KTAG_decays_in_FV_all) POT(KTAG_decays_in_FV_all)" << endl;
  NKInfoFile << "# Line 2: Argn_all RunID StartOfRunTime N(trig) NK(Argonion_decays_in_FV_all) N0(Argonion_decays_in_FV_all) POT(Argonion_decays_in_FV_all)" << endl;
  NKInfoFile << Form("KTAG_all %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHEventsPerBurst->Integral(),NK_fromKTAG*fDecayProb,NK_fromKTAG,POT_fromKTAG) << endl;
  NKInfoFile << Form("Argn_all %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHEventsPerBurst->Integral(),NK_fromArgo*fDecayProb,NK_fromArgo,POT_fromArgo) << endl;
  NKInfoFile.close();

  ///////////////////////////////////////////////////////////////////
  // Print number and kaon decays and POT for all enabled L0 triggers

  cout << user_normal() << "-------------------------------------------------" << endl;
  cout << user_normal() << "Enabled L0 triggers; numbers of K decays & POT for each trigger:" << endl;
  // Control trigger (if enabled)
  if (fTriggerConditions->ControlTriggerEnabled(RunID)) {
    Int_t DS = fTriggerConditions->GetControlTriggerDownscaling(RunID);
    Double_t Ndec_DS = (DS>0) ? CTL_Ndec/DS : 0.0;
    Double_t POT_DS  = (DS>0) ? CTL_POT/DS  : 0.0;
    cout << user_normal() << "Decays_POT_per_trigger " << RunID << " Control DS=" <<
      DS << " " << Ndec_DS << " " << POT_DS << endl;
  }
  // All enabled regular L0 triggers
  for (Int_t TrigID=0; TrigID<fTriggerConditions->GetNumberOfL0Conditions(); TrigID++) {
    if (fTriggerConditions->L0TriggerEnabled(RunID, TrigID)) {
      Int_t DS = fTriggerConditions->GetL0TriggerDownscaling(RunID, TrigID);
      Double_t Ndec_DS = (DS>0) ? CTL_Ndec/DS : 0.0; // D=-1 means variable downscaling; this trigger is ignored
      Double_t POT_DS  = (DS>0) ? CTL_POT/DS  : 0.0;
      cout << user_normal() << "Decays_POT_per_trigger " << RunID << " " <<
	fTriggerConditions->GetL0ConditionName(TrigID) << " DS=" << DS << " " <<
	Ndec_DS << " " << POT_DS << endl;
    }
  }
  cout << user_normal() << "-------------------------------------------------" << endl;

  cout << user_normal() << "TotalBursts= " << NumberOfBursts <<
    " NonEmpty= " << NumberOfNonEmptyBursts <<
    " WithPhysicsTriggers= " << NumberOfBurstsWithPhysicsTriggers <<
    " WithControlTriggers= " << NumberOfBurstsWithControlTriggers <<
    " WithControlTriggersAndK3pi= " << NumberOfBurstsWithControlK3piEvents << endl;

  // Trigger efficiency computation and printout
  Double_t EffRICH = 0., dEffRICH = 0.;
  if (fTrigEffRICH && fTrigAllRICH) {
    EffRICH  = fTrigEffRICH->Integral()/fTrigAllRICH->Integral();
    dEffRICH = BinomialError(fTrigEffRICH->Integral(), fTrigAllRICH->Integral());
  }
  Double_t EffQX = 0., dEffQX = 0.;
  if (fTrigEffQX && fTrigAllQX) {
    EffQX  = fTrigEffQX->Integral()/fTrigAllQX->Integral();
    dEffQX = BinomialError(fTrigEffQX->Integral(), fTrigAllQX->Integral());
  }
  Double_t EffKTAG = 0., dEffKTAG = 0.;
  if (fTrigL1KTAG && fTrigL1All) {
    EffKTAG  = fTrigL1KTAG->Integral()/fTrigL1All->Integral();
    dEffKTAG = BinomialError(fTrigL1KTAG->Integral(), fTrigL1All->Integral());
  }
  Double_t EffLAV = 0., dEffLAV = 0.;
  if (fTrigL1LAV && fTrigL1KTAG) {
    EffLAV  = fTrigL1LAV->Integral()/fTrigL1KTAG->Integral();
    dEffLAV = BinomialError(fTrigL1LAV->Integral(), fTrigL1KTAG->Integral());
  }
  Double_t EffSTRAW = 0., dEffSTRAW = 0.;
  if (fTrigL1STRAW && fTrigL1LAV) {
    EffSTRAW  = fTrigL1STRAW->Integral()/fTrigL1LAV->Integral();
    dEffSTRAW = BinomialError(fTrigL1STRAW->Integral(), fTrigL1LAV->Integral());
  }

  Double_t EffTot  = EffRICH * EffQX * EffKTAG * EffLAV * EffSTRAW;
  Double_t dEffTot = EffTot * sqrt(dEffRICH*dEffRICH/EffRICH/EffRICH +
				   dEffQX*dEffQX/EffQX/EffQX +
				   dEffKTAG*dEffKTAG/EffKTAG/EffKTAG +
				   dEffLAV*dEffLAV/EffLAV/EffLAV +
				   dEffSTRAW*dEffSTRAW/EffSTRAW/EffSTRAW);

  // Emulated QX efficiency
  Double_t EmulatedL0QXEff = 0.0;
  if (fEmulatedL0QX && fEmulatedL0All)
    EmulatedL0QXEff = fEmulatedL0QX->Integral()/fEmulatedL0All->Integral();
  cout << user_normal() << "Emulated QX efficiency: " << EmulatedL0QXEff << endl;

  cout << user_normal() << Form("TrigEff(L0)  RICH %6.4f +- %6.4f QX    %6.4f +- %6.4f\n",
	       EffRICH, dEffRICH, EffQX, dEffQX);
  cout << user_normal() << Form("TrigEff(L1)  KTAG %6.4f +- %6.4f LAV   %6.4f +- %6.4f STRAW %6.4f +- %6.4f\n",
	       EffKTAG, dEffKTAG, EffLAV, dEffLAV, EffSTRAW, dEffSTRAW);
  cout << user_normal() << Form("TrigEff(tot) %6.4f +- %6.4f\n", EffTot, dEffTot);

  if (fHZtrue) { // MC input
    Double_t n = fHMomentum->Integral();
    Double_t N = fHZtrue->Integral(106,180); // 105 m < Ztrue < 180 m
    Double_t Acc  = n/N;
    Double_t dAcc = sqrt(Acc*(1.0-Acc)/N);
    cout << user_normal() << Form("MC events read: %d\n", (Int_t)fHZtrue->Integral());
    cout << user_normal() << Form("MC acceptance = %d/%d = %7.5f +- %7.5f\n",
				  (Int_t)n, (Int_t)N, Acc, dAcc);
}

  // Save histograms into the output file
  fHMass->Write(); fHMomentum->Write(); fFMomentum->Write();
  fHdxdz->Write(); fFdxdz->Write();
  fHdydz->Write(); fFdydz->Write();

  ////////////////////////////////
  // Produce the K3pi count graphs

  Double_t x[5000], dx[5000];
  for (Int_t i=0; i<fMaxNBursts; i++) {
    x[i] = i;
    dx[i] = 0.0;
    // this is to make points with no counts invisible in the graphs
    if (CTL_Ndec_PerBurst[i]==0.0) {
      CTL_Ndec_PerBurst[i] = -1.0;
      CTL_dNdec_PerBurst[i] = 0.01;
    }
    if (MUL_Ndec_PerBurst[i]==0.0) {
      MUL_Ndec_PerBurst[i] = -1.0;
      MUL_dNdec_PerBurst[i] = 0.01;
    }
    if (Ndec_PerBurst_fromKTAG[i]==0.0) Ndec_PerBurst_fromKTAG[i] = -1.0;
    if (Ndec_PerBurst_fromArgo[i]==0.0) Ndec_PerBurst_fromArgo[i] = -1.0;
  }
  fGraph_K3pi_CTL  = new TGraphErrors(MaxNonEmptyBurstID+1, x, CTL_Ndec_PerBurst, dx, CTL_dNdec_PerBurst);
  fGraph_K3pi_MUL  = new TGraphErrors(MaxNonEmptyBurstID+1, x, MUL_Ndec_PerBurst, dx, MUL_dNdec_PerBurst);
  fGraph_K3pi_KTAG = new TGraph(MaxNonEmptyBurstID+1, x, Ndec_PerBurst_fromKTAG);
  fGraph_K3pi_Argo = new TGraph(MaxNonEmptyBurstID+1, x, Ndec_PerBurst_fromArgo);

  //////////////////////////////////////////////////////
  // Produce L0 and L1 trigger efficiency vs burst plots

  if (fTrigEffRICH && fTrigAllRICH) {
    fTrigEffRICH->Sumw2();
    fTrigAllRICH->Sumw2();
    fTrigEfficiencyRICH = (TH1F*)fTrigEffRICH->Clone();
    fTrigEfficiencyRICH->Divide(fTrigEffRICH, fTrigAllRICH, 1.0, 1.0, "B");
  }
  
  if (fTrigEffQX && fTrigAllQX) {
    fTrigEffQX->Sumw2();
    fTrigAllQX->Sumw2();
    fTrigEfficiencyQX = (TH1F*)fTrigEffQX->Clone();
    fTrigEfficiencyQX->Divide(fTrigEffQX, fTrigAllQX, 1.0, 1.0, "B");
  }

  if (fTrigL1All && fTrigL1KTAG && fTrigL1STRAW) {
    fTrigL1All->Sumw2();
    fTrigL1KTAG->Sumw2();
    fTrigL1STRAW->Sumw2();
    fTrigL1EfficiencyKTAG = (TH1F*)fTrigL1KTAG->Clone();
    fTrigL1EfficiencyKTAG->Divide(fTrigL1KTAG, fTrigL1All, 1.0, 1.0, "B");
    fTrigL1EfficiencySTRAW = (TH1F*)fTrigL1STRAW->Clone();
    fTrigL1EfficiencySTRAW->Divide(fTrigL1STRAW, fTrigL1KTAG, 1.0, 1.0, "B");
  }

  // Produce the PDF report
  BuildPDFReport();
}

void K3piSelection::BuildPDFReport() {
  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(11);

  TCanvas *Canvas = new TCanvas("K3piCanvas");
  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open output file

  //////////////////////////////////////////
  // Page 1: mass, momentum, dz/dz and dy/dz

  Canvas->Divide(2,2);
  for (Int_t i=1; i<=4; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.04);
    Canvas->GetPad(i)->SetRightMargin(0.01);
    Canvas->GetPad(i)->SetTopMargin(0.06);
    Canvas->GetPad(i)->SetBottomMargin(0.10);
  }

  fHMass->SetLineColor(kBlue);
  fHMass->SetFillColor(kYellow);
  fHMomentum->SetLineColor(kBlue);
  fHMomentum->SetFillColor(kYellow);
  fHdxdz->SetLineColor(kBlue);
  fHdxdz->SetFillColor(kYellow);
  fHdydz->SetLineColor(kBlue);
  fHdydz->SetFillColor(kYellow);

  Canvas->cd(1);
  fHMass->GetXaxis()->SetRangeUser(485, 505);
  fHMass->Draw();
  Canvas->cd(2);
  fHMomentum->GetXaxis()->SetRangeUser(60, 90);
  fHMomentum->Draw(); fFMomentum->Draw("same");
  Canvas->cd(3);
  fHdxdz->GetXaxis()->SetRangeUser(0, 0.002);
  fHdxdz->Draw(); fFdxdz->Draw("same");  
  Canvas->cd(4);
  fHdydz->GetXaxis()->SetRangeUser(-0.001, 0.001);
  fHdydz->Draw(); fFdydz->Draw("same");
  Canvas->Print(OutputPDFFileName, "pdf");

  /////////////////////////////////////////////////////////////////////////////////////////////
  // Page 2: x and y at GTK3 (z=102.4m); no fits as beam parameters are computed as mean values

  Canvas->Clear();
  Canvas->Divide(1,2);
  for (Int_t i=1; i<=2; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.04);
    Canvas->GetPad(i)->SetRightMargin(0.01);
    Canvas->GetPad(i)->SetTopMargin(0.06);
    Canvas->GetPad(i)->SetBottomMargin(0.10);
  }

  fHx->SetLineColor(kBlue);
  fHx->SetFillColor(kYellow);
  fHy->SetLineColor(kBlue);
  fHy->SetFillColor(kYellow);

  TArrow *l = new TArrow(); l->SetLineColor(kRed); l->SetLineWidth(2);
  Canvas->cd(1);
  fHx->Draw();
  l->DrawArrow(fHx->GetMean(), 0.3*fHx->GetMaximum(), fHx->GetMean(), 0.0);
  Canvas->cd(2);
  fHy->Draw();
  l->DrawArrow(fHy->GetMean(), 0.3*fHy->GetMaximum(), fHy->GetMean(), 0.0);
  Canvas->Print(OutputPDFFileName, "pdf");

  ////////////////////////////
  // Page 3: kaon decay counts

  Canvas->Clear();
  gStyle->SetOptStat(0);
  Canvas->SetLeftMargin(0.05);
  Canvas->SetRightMargin(0.01);
  Canvas->SetTopMargin(0.06);
  Canvas->SetBottomMargin(0.10);
  fGraph_K3pi_CTL->SetMarkerColor(kRed); fGraph_K3pi_CTL->SetLineColor(kRed);
  fGraph_K3pi_MUL->SetMarkerColor(kBlue); fGraph_K3pi_MUL->SetLineColor(kBlue);
  fGraph_K3pi_KTAG->SetMarkerColor(kGreen+2); fGraph_K3pi_KTAG->SetLineColor(kGreen+2);
  fGraph_K3pi_Argo->SetMarkerColor(kBlack); fGraph_K3pi_Argo->SetLineColor(kBlack);
  fGraph_K3pi_CTL->SetMarkerStyle(kFullCircle); fGraph_K3pi_CTL->SetMarkerSize(0.5);
  fGraph_K3pi_MUL->SetMarkerStyle(kFullCircle); fGraph_K3pi_MUL->SetMarkerSize(0.5);
  fGraph_K3pi_KTAG->SetMarkerStyle(kFullCircle); fGraph_K3pi_KTAG->SetMarkerSize(0.5);
  fGraph_K3pi_Argo->SetMarkerStyle(kFullCircle); fGraph_K3pi_Argo->SetMarkerSize(0.5);

  TString Name1 = Form("Run %d: Kaon decays in FV vs burst ID", GetRunID());
  TH2F *h1 = new TH2F("h1", Name1, 1, -0.5, fGraph_K3pi_CTL->GetN()-0.5, 1, 0, 2e7);
  h1->GetXaxis()->SetTitle("Burst ID");
  h1->Draw();
  fGraph_K3pi_CTL->Draw("pe same");
  fGraph_K3pi_MUL->Draw("pe same");
  fGraph_K3pi_KTAG->Draw("p same");
  fGraph_K3pi_Argo->Draw("p same");

  TLegend* Legend = new TLegend(0.10, 0.75, 0.35, 0.90);
  Legend->SetFillColor(kWhite);
  Legend->AddEntry(fGraph_K3pi_CTL, "K_{3#pi}, control trigger", "pl");
  Legend->AddEntry(fGraph_K3pi_MUL, "K_{3#pi}, multi-track trigger", "pl");
  Legend->AddEntry(fGraph_K3pi_KTAG, "From KTAG counts", "pl");
  Legend->AddEntry(fGraph_K3pi_Argo, "From Argonion counts", "pl");
  Legend->Draw();
  Canvas->Print(OutputPDFFileName, "pdf");

  ///////////////////////////////
  // Page 4: trigger efficiencies

  Canvas->Clear();
  Canvas->Divide(1,2);
  for (Int_t i=1; i<=2; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.04);
    Canvas->GetPad(i)->SetRightMargin(0.01);
    Canvas->GetPad(i)->SetTopMargin(0.07);
    Canvas->GetPad(i)->SetBottomMargin(0.07);
  }

  TString Name2a = Form("Run %d: L0 trigger efficiencies vs burst ID", GetRunID());
  TString Name2b = Form("Run %d: L1 trigger efficiencies vs burst ID", GetRunID());
  TH2F *h2a = new TH2F("h2a", Name2a, 1, -0.5, fGraph_K3pi_CTL->GetN()-0.5, 1, 0.5, 1);
  TH2F *h2b = new TH2F("h2b", Name2b, 1, -0.5, fGraph_K3pi_CTL->GetN()-0.5, 1, 0.5, 1);
  h2a->GetXaxis()->SetTitle("Burst ID");
  h2b->GetXaxis()->SetTitle("Burst ID");

  if (fTrigEfficiencyRICH) {
    fTrigEfficiencyRICH->SetLineColor(kBlue); fTrigEfficiencyRICH->SetMarkerColor(kBlue);
    fTrigEfficiencyRICH->SetMarkerStyle(kFullCircle); fTrigEfficiencyRICH->SetMarkerSize(0.5);
  }
  if (fTrigEfficiencyQX) {
    fTrigEfficiencyQX->SetLineColor(kRed); fTrigEfficiencyQX->SetMarkerColor(kRed);
    fTrigEfficiencyQX->SetMarkerStyle(kFullCircle); fTrigEfficiencyQX->SetMarkerSize(0.5);
  }
  if (fTrigL1EfficiencyKTAG) {
    fTrigL1EfficiencyKTAG->SetLineColor(kBlue); fTrigL1EfficiencyKTAG->SetMarkerColor(kBlue);
    fTrigL1EfficiencyKTAG->SetMarkerStyle(kFullCircle); fTrigL1EfficiencyKTAG->SetMarkerSize(0.5);
  }
  if (fTrigL1EfficiencySTRAW) {
    fTrigL1EfficiencySTRAW->SetLineColor(kRed); fTrigL1EfficiencySTRAW->SetMarkerColor(kRed);
    fTrigL1EfficiencySTRAW->SetMarkerStyle(kFullCircle); fTrigL1EfficiencySTRAW->SetMarkerSize(0.5);
  }

  Canvas->cd(1);
  h2a->Draw();
  if(fTrigEfficiencyRICH) fTrigEfficiencyRICH->Draw("pe same");
  if(fTrigEfficiencyQX)   fTrigEfficiencyQX->Draw("pe same");
  TLegend* Legend1 = new TLegend(0.05, 0.15, 0.20, 0.30);
  Legend1->SetFillColor(kWhite);
  if(fTrigEfficiencyRICH) Legend1->AddEntry(fTrigEfficiencyRICH, "RICH", "pl");
  if(fTrigEfficiencyQX)   Legend1->AddEntry(fTrigEfficiencyQX, "QX", "pl");
  Legend1->Draw();

  Canvas->cd(2);
  h2b->Draw();
  if(fTrigL1EfficiencyKTAG)  fTrigL1EfficiencyKTAG->Draw("pe same");
  if(fTrigL1EfficiencySTRAW) fTrigL1EfficiencySTRAW->Draw("pe same");
  TLegend* Legend2 = new TLegend(0.05, 0.15, 0.20, 0.30);
  Legend2->SetFillColor(kWhite);
  if(fTrigL1EfficiencyKTAG)  Legend2->AddEntry(fTrigL1EfficiencyKTAG, "KTAG", "pl");
  if(fTrigL1EfficiencySTRAW) Legend2->AddEntry(fTrigL1EfficiencySTRAW, "STRAW", "pl");
  Legend2->Draw();

  Canvas->Print(OutputPDFFileName, "pdf");

  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  delete Legend;
  delete Canvas;
  gErrorIgnoreLevel = -1; // restore the default
}

Double_t K3piSelection::BinomialError(Double_t n, Double_t N) {
  if (N<0.5) return 0.0;
  Double_t e = n/N;
  return sqrt(e*(1.0-e)/N);
}
