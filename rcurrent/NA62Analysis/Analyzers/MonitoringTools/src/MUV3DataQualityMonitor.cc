// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-06-25
//
// ---------------------------------------------------------------

#include "MUV3DataQualityMonitor.hh"
#include "MUV3DataQualityPlotter.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class MUV3DataQualityMonitor
/// \Brief
/// Convert MUV3 monitoring histograms into a PDF report
/// \EndBrief
/// \Detailed
/// Runs in the "--histo" mode over the output files of the reconstruction.
/// The MUV3DataQualityPlotter class of NA62Reconstruction is called
/// to produce the standard monitoring plots.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

MUV3DataQualityMonitor::MUV3DataQualityMonitor(Core::BaseAnalysis *ba) :
  Analyzer(ba, "MUV3DataQualityMonitor"),
  fHNEventsProcessedPerBurst(nullptr),
  fHChannelProfile(nullptr),
  fHChannelProfileEOB(nullptr),
  fHTileOR(nullptr),
  fHTileAND(nullptr),
  fHCandidateTimeWrtReferenceNoTileT0(nullptr),
  fHCandidateTimeWrtReference(nullptr),
  fHCandidateAvgTimeWrtReference(nullptr),
  fHChannelProfileVsBurst(nullptr),
  fHChannelProfileVsBurstEOB(nullptr),
  fHNCandidatesVsL0TriggerBit(nullptr),
  fHNCandidatesVsNoL0TriggerBit(nullptr),
  fHTightCandidateProfileVsL0TriggerBit(nullptr),
  fHTightCandidateProfileVsNoL0TriggerBit(nullptr),
  fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0(nullptr),
  fHRecoHitTimeWrtReferenceVsReadoutChannel(nullptr),
  fHCandidateTimeWrtReferenceNoTileT0VsTile(nullptr),
  fHCandidateTimeWrtReferenceVsTile(nullptr),
  fHCandidateAvgTimeWrtReferenceVsTile(nullptr),
  fHTotalPrimitiveCountsEOB(nullptr),
  fHErrorCountsEOB(nullptr)
{
  fDirName = "MUV3Monitor";
  AddParam("fOutDirName", &fOutDirName, ".");
}

void MUV3DataQualityMonitor::InitHist() {

  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  // For good burst lists and normalisation
  fHNEventsProcessedPerBurst = static_cast<TH1F*>(RequestHistogram("/",      "EventsPerBurst",           true));
  fHNRecoHitsPerBurst        = static_cast<TH1F*>(RequestHistogram(fDirName, "NRecoHitsPerBurst",        true));
  fHNLooseCandidatesPerBurst = static_cast<TH1F*>(RequestHistogram(fDirName, "NLooseCandidatesPerBurst", true));
  fHNTightCandidatesPerBurst = static_cast<TH1F*>(RequestHistogram(fDirName, "NTightCandidatesPerBurst", true));

  // Channel profile and number of OR, AND in tiles
  fHChannelProfile              = static_cast<TH1F*>(RequestHistogram(fDirName, "ChannelProfile",    true));
  fHChannelProfileEOB           = static_cast<TH1F*>(RequestHistogram(fDirName, "ChannelProfileEOB", true));
  fHTileOR                      = static_cast<TH1F*>(RequestHistogram(fDirName, "TileOR",            true));
  fHTileAND                     = static_cast<TH1F*>(RequestHistogram(fDirName, "TileAND",           true));
  fHChannelProfileVsBurst       = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfileVsBurst",    true));
  fHChannelProfileVsBurstEOB    = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfileVsBurstEOB", true));
  fHNCandidatesVsL0TriggerBit   = static_cast<TH2F*>(RequestHistogram(fDirName, "NCandidatesVsL0TriggerBit", true));
  fHNCandidatesVsNoL0TriggerBit = static_cast<TH2F*>(RequestHistogram(fDirName, "NCandidatesVsNoL0TriggerBit", true));
  fHTightCandidateProfileVsL0TriggerBit =
    static_cast<TH2F*>(RequestHistogram(fDirName, "TightCandidateProfileVsL0TriggerBit", true));
  fHTightCandidateProfileVsNoL0TriggerBit =
    static_cast<TH2F*>(RequestHistogram(fDirName, "TightCandidateProfileVsNoL0TriggerBit", true));

  // Channel times
  fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0 =
    static_cast<TH2F*>(RequestHistogram(fDirName, "RecoHitTimeWrtReferenceVsReadoutChannelNoT0", true));
  fHRecoHitTimeWrtReferenceVsReadoutChannel =
    static_cast<TH2F*>(RequestHistogram(fDirName, "RecoHitTimeWrtReferenceVsReadoutChannel", true));

  // Candidate times
  fHCandidateAvgTimeWrtReference =
    static_cast<TH1F*>(RequestHistogram(fDirName, "CandidateAvgTimeWrtReference", true));
  fHCandidateTimeWrtReference =
    static_cast<TH1F*>(RequestHistogram(fDirName, "CandidateTimeWrtReference", true));
  fHCandidateTimeWrtReferenceNoTileT0 =
    static_cast<TH1F*>(RequestHistogram(fDirName, "CandidateTimeWrtReferenceNoTileT0", true));

  fHCandidateAvgTimeWrtReferenceVsTile =
    static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateAvgTimeWrtReferenceVsTile", true));
  fHCandidateTimeWrtReferenceNoTileT0VsTile =
    static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateTimeWrtReferenceNoTileT0VsTile", true));
  fHCandidateTimeWrtReferenceVsTile =
    static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateTimeWrtReferenceVsTile", true));

  // 2D count-maps
  fHList.resize(18);

  // Offline: RecoHits
  fHList[0] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM0",             true));
  fHList[1] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2DInner_PM0",        true));
  fHList[2] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM1",             true));
  fHList[3] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2DInner_PM1",        true));

  // Offline: candidates
  fHList[6] = static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateProfile2D",               true));
  fHList[7] = static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateProfile2DInner",          true));
  fHList[8] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightCandidateProfile2D",          true));
  fHList[9] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightCandidateProfile2DInner",     true));

  // EOB: channel counts
  fHList[10] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM0 EOB",         true));
  fHList[11] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2DInner_PM0 EOB",    true));
  fHList[12] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM1 EOB",         true));
  fHList[13] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2DInner_PM1 EOB",    true));

  // EOB: L0 primitives
  fHList[16] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightPrimitiveProfile2D EOB",      true));
  fHList[17] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightPrimitiveProfile2DInner EOB", true));

  // SL EOB data
  fHTotalPrimitiveCountsEOB = static_cast<TH1F*>(RequestHistogram(fDirName, "TotalPrimitiveCountsEOB", true));
  fHErrorCountsEOB          = static_cast<TH1F*>(RequestHistogram(fDirName, "ErrorCountsEOB",          true));
}

void MUV3DataQualityMonitor::EndOfJobUser() {

  if (!fHChannelProfile) {
    cout << user_normal() << "Input not found, no report generated" << endl;
    return;
  }

  ///////////////////////////////////////////
  // Save the histograms into the output file

  fHCandidateAvgTimeWrtReference->Write();
  fHCandidateTimeWrtReference->Write();
  fHCandidateTimeWrtReferenceNoTileT0->Write();

  //////////////////////////////////////////
  // Send the histograms to the plotter tool

  if (fHList[2])  fHList[4]  = (TH2F*)fHList[2]->Clone("ChannelProfile2D_PM1-PM0");
  if (fHList[3])  fHList[5]  = (TH2F*)fHList[3]->Clone("ChannelProfile2DInner_PM1-PM0");
  if (fHList[12]) fHList[14] = (TH2F*)fHList[12]->Clone("ChannelProfile2D_PM0-PM1");
  if (fHList[13]) fHList[15] = (TH2F*)fHList[13]->Clone("ChannelProfile2DInner_PM0-PM1");

  std::vector<TH1*> List;
  List.push_back(fHNEventsProcessedPerBurst);
  List.push_back(fHChannelProfile);
  List.push_back(fHChannelProfileEOB);
  List.push_back(fHTileOR);
  List.push_back(fHTileAND);
  List.push_back(fHNCandidatesVsL0TriggerBit);
  List.push_back(fHNCandidatesVsNoL0TriggerBit);
  List.push_back(fHTightCandidateProfileVsL0TriggerBit);
  List.push_back(fHTightCandidateProfileVsNoL0TriggerBit);
  List.push_back(fHCandidateTimeWrtReferenceNoTileT0);
  List.push_back(fHCandidateTimeWrtReference);
  List.push_back(fHCandidateAvgTimeWrtReference);
  List.push_back(fHTotalPrimitiveCountsEOB);
  List.push_back(fHErrorCountsEOB);
  List.push_back(fHChannelProfileVsBurst);
  List.push_back(fHChannelProfileVsBurstEOB);
  List.push_back(fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0);
  List.push_back(fHRecoHitTimeWrtReferenceVsReadoutChannel);
  List.push_back(fHCandidateTimeWrtReferenceNoTileT0VsTile);
  List.push_back(fHCandidateTimeWrtReferenceVsTile);
  List.push_back(fHCandidateAvgTimeWrtReferenceVsTile);
  for (int i=0; i<18; i++) List.push_back(fHList[i]);

  MUV3DataQualityPlotter *Plotter = new MUV3DataQualityPlotter(List, fOutDirName + "/"+fAnalyzerName+".pdf");
  Plotter->BuildPDF();
  delete Plotter;
}
