// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-09
//
// ---------------------------------------------------------------

#include "NewCHODDataQualityMonitor.hh"
#include "NewCHODDataQualityPlotter.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class NewCHODDataQualityMonitor
/// \Brief
/// Convert NewCHOD monitoring histograms into a PDF report
/// \EndBrief
/// \Detailed
/// The histograms are read from the reconstruction output file in the --histo mode,
/// and passed to NewCHODDataQualityPlotter in NA62Reconstruction for plotting.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

NewCHODDataQualityMonitor::NewCHODDataQualityMonitor(Core::BaseAnalysis *ba) :
  Analyzer(ba, "NewCHODDataQualityMonitor"),
  fHChannelProfile(nullptr),
  fHChannelProfileEOB(nullptr),
  fHTileOR(nullptr),
  fHTileAND(nullptr),
  fHNRecoHitsVsL0TriggerBit(nullptr),
  fHNRecoHitsVsNoL0TriggerBit(nullptr),
  fHTightRecoHitProfileVsL0TriggerBit(nullptr),
  fHTightRecoHitProfileVsNoL0TriggerBit(nullptr),
  fHChannelProfileVsBurst(nullptr),
  fHChannelProfileVsBurstEOB(nullptr),
  fHTimeWrtReferenceVsReadoutChannelNoT0(nullptr),
  fHTimeWrtReferenceVsReadoutChannel(nullptr),
  fHNEventsProcessedPerBurst(nullptr),
  fHTileAsymmetry(nullptr),
  fHTileAsymmetryEOB(nullptr),
  fHTotalPrimitiveCountsEOB(nullptr),
  fHErrorCountsEOB(nullptr)
{
  fDirName = "NewCHODMonitor";
}

void NewCHODDataQualityMonitor::InitHist() {

  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  // Channel profile and number of OR, AND in tiles
  fHNEventsProcessedPerBurst  = static_cast<TH1F*>(RequestHistogram("/",      "EventsPerBurst",    true));
  fHChannelProfile            = static_cast<TH1F*>(RequestHistogram(fDirName, "ChannelProfile",    true));
  fHChannelProfileEOB         = static_cast<TH1F*>(RequestHistogram(fDirName, "ChannelProfileEOB", true));
  fHTileOR                    = static_cast<TH1F*>(RequestHistogram(fDirName, "TileOR",            true));
  fHTileAND                   = static_cast<TH1F*>(RequestHistogram(fDirName, "TileAND",           true));
  fHChannelProfileVsBurst     = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfileVsBurst",    true));
  fHChannelProfileVsBurstEOB  = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfileVsBurstEOB", true));
  fHNRecoHitsVsL0TriggerBit   = static_cast<TH2F*>(RequestHistogram(fDirName, "NRecoHitsVsL0TriggerBit",   true));
  fHNRecoHitsVsNoL0TriggerBit = static_cast<TH2F*>(RequestHistogram(fDirName, "NRecoHitsVsNoL0TriggerBit", true));
  fHTightRecoHitProfileVsL0TriggerBit =
    static_cast<TH2F*>(RequestHistogram(fDirName, "TightRecoHitProfileVsL0TriggerBit", true));
  fHTightRecoHitProfileVsNoL0TriggerBit =
    static_cast<TH2F*>(RequestHistogram(fDirName, "TightRecoHitProfileVsNoL0TriggerBit", true));

  // Digi times: the histos are called "RecoHit..." because it's the standard for T0 evalution
  fHTimeWrtReferenceVsReadoutChannelNoT0 =
    static_cast<TH2F*>(RequestHistogram(fDirName, "RecoHitTimeWrtReferenceVsReadoutChannelNoT0", true));
  fHTimeWrtReferenceVsReadoutChannel =
    static_cast<TH2F*>(RequestHistogram(fDirName, "RecoHitTimeWrtReferenceVsReadoutChannel", true));

  // RecoHit times
  fHTightRecoHitTimeWrtReferenceVsTile =
    static_cast<TH2F*>(RequestHistogram(fDirName, "TightRecoHitTimeWrtReferenceVsTile", true));
  fHLooseRecoHitTimeWrtReferenceVsTile =
    static_cast<TH2F*>(RequestHistogram(fDirName, "LooseRecoHitTimeWrtReferenceVsTile", true));
  fHTightRecoHitTimeWrtReference =
    static_cast<TH1F*>(RequestHistogram(fDirName, "TightRecoHitTimeWrtReference", true));
  fHLooseRecoHitTimeWrtReference =
    static_cast<TH1F*>(RequestHistogram(fDirName, "LooseRecoHitTimeWrtReference", true));

  // 2D count-maps: the elementary "brick" size is 134*107 mm^2
  fHList.resize(9);

  // Offline: RecoHits
  fHList[0] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM0", true));
  fHList[1] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM1", true));
  if (fHList[1]) fHList[2] = (TH2F*)fHList[1]->Clone("ChannelProfile2D_PM1-PM0");

  // Offline: candidates [no longer defined]
  fHList[3] = static_cast<TH2F*>(RequestHistogram(fDirName, "CandidateProfile2D",      true));
  fHList[4] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightCandidateProfile2D", true));

  // EOB: channel counts
  fHList[5] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM0 EOB", true));
  fHList[6] = static_cast<TH2F*>(RequestHistogram(fDirName, "ChannelProfile2D_PM1 EOB", true));
  if (fHList[6]) fHList[7] = (TH2F*)fHList[6]->Clone("ChannelProfile2D_PM0-PM1");

  // EOB: L0 primitives
  fHList[8] = static_cast<TH2F*>(RequestHistogram(fDirName, "TightPrimitiveProfile2D EOB", true));

  // SL EOB data
  fHTotalPrimitiveCountsEOB = static_cast<TH1F*>(RequestHistogram(fDirName, "TotalPrimitiveCountsEOB", true));
  fHErrorCountsEOB          = static_cast<TH1F*>(RequestHistogram(fDirName, "ErrorCountsEOB",          true));
}

void NewCHODDataQualityMonitor::EndOfJobUser() {

  if (!fHChannelProfile) {
    cout << user_normal() << "Input not found, no report generated" << endl;
    return;
  }

  ///////////////////////////////////////////////////////////////////////
  // Both "named" and "fHList" histograms are packed into a single vector
  // and passed to NewCHODDataQualityPlotter for plotting.

  std::vector<TH1*> List;
  List.push_back(fHNEventsProcessedPerBurst);
  List.push_back(fHChannelProfile);
  List.push_back(fHChannelProfileEOB);
  List.push_back(fHTileOR);
  List.push_back(fHTileAND);
  List.push_back(fHNRecoHitsVsL0TriggerBit);
  List.push_back(fHNRecoHitsVsNoL0TriggerBit);
  List.push_back(fHTightRecoHitProfileVsL0TriggerBit);
  List.push_back(fHTightRecoHitProfileVsNoL0TriggerBit);
  List.push_back(fHChannelProfileVsBurst);
  List.push_back(fHChannelProfileVsBurstEOB);
  List.push_back(fHTimeWrtReferenceVsReadoutChannelNoT0);
  List.push_back(fHTimeWrtReferenceVsReadoutChannel);
  List.push_back(fHTightRecoHitTimeWrtReferenceVsTile);
  List.push_back(fHLooseRecoHitTimeWrtReferenceVsTile);
  List.push_back(fHTightRecoHitTimeWrtReference);
  List.push_back(fHLooseRecoHitTimeWrtReference);
  for (int i=0; i<9; i++) List.push_back(fHList[i]);

  NewCHODDataQualityPlotter *Plotter = new NewCHODDataQualityPlotter(List, "./"+fAnalyzerName+".pdf");

  /////////////////////////////////////////////////////////////////////////////////////////
  // One can replace contents of the two 2D histograms (low & high channels) to be plotted.
  // Two examples below: reproduce the NIM plot, or build the channel numbering sketch.

  //LoadNIMPaperRatesAndAcceptances();
  //Plotter->FillOccupancyPlotWithChannelIDs(fHChannelProfile);
  //Plotter->ConvertProfileToTwoDimensionalMap(fHChannelProfile, fHList[0], 0); // low channel map
  //Plotter->ConvertProfileToTwoDimensionalMap(fHChannelProfile, fHList[1], 1); // high channel map

  Plotter->BuildPDF();
  delete Plotter;
}

//////////////////////////////////////////////////////////////////////////////
// Replace the contents of the two 1D occupancy histograms with the values of
// rates and Kpinn acceptance per tile: this allows reproducing the figure
// from the NA62 detector paper. This is not very elegant but very useful.
// These can be then converted into 2D plots using
// NewCHODDataQualityPlotter::ConvertProfileToTwoDimensionalMap.

void NewCHODDataQualityMonitor::LoadNIMPaperRatesAndAcceptances() {

  // Nominal rates in tiles [MHz]: they are loaded into the low PMT occupancy plot.
  fHChannelProfile->SetBinContent(1, 0.9199659);
  fHChannelProfile->SetBinContent(2, 0.4697041);
  fHChannelProfile->SetBinContent(3, 0.2859553);
  fHChannelProfile->SetBinContent(4, 0.3190861);
  fHChannelProfile->SetBinContent(5, 0.163285);
  fHChannelProfile->SetBinContent(6, 0.6861124);
  fHChannelProfile->SetBinContent(7, 0.6133484);
  fHChannelProfile->SetBinContent(8, 0.38462);
  fHChannelProfile->SetBinContent(9, 0.24296);
  fHChannelProfile->SetBinContent(10, 0.2448181);
  fHChannelProfile->SetBinContent(11, 0.15841871);
  fHChannelProfile->SetBinContent(12, 0.5237542);
  fHChannelProfile->SetBinContent(13, 0.4481221);
  fHChannelProfile->SetBinContent(14, 0.4702965);
  fHChannelProfile->SetBinContent(15, 0.2563611);
  fHChannelProfile->SetBinContent(16, 0.15054632);
  fHChannelProfile->SetBinContent(17, 0.3489171);
  fHChannelProfile->SetBinContent(18, 0.2904391);
  fHChannelProfile->SetBinContent(19, 0.3538403);
  fHChannelProfile->SetBinContent(20, 0.23669697);
  fHChannelProfile->SetBinContent(21, 0.089767364);
  fHChannelProfile->SetBinContent(22, 0.4881785);
  fHChannelProfile->SetBinContent(23, 0.3745877);
  fHChannelProfile->SetBinContent(24, 0.2249917);
  fHChannelProfile->SetBinContent(25, 0.104586027);
  fHChannelProfile->SetBinContent(26, 0.34095);
  fHChannelProfile->SetBinContent(27, 0.24096484);
  fHChannelProfile->SetBinContent(28, 0.16910698);
  fHChannelProfile->SetBinContent(29, 0.044816495);
  fHChannelProfile->SetBinContent(30, 0.27890931);
  fHChannelProfile->SetBinContent(31, 0.21512002);
  fHChannelProfile->SetBinContent(32, 0.13346832);
  fHChannelProfile->SetBinContent(33, 0.21985187);
  fHChannelProfile->SetBinContent(34, 0.14844977);
  fHChannelProfile->SetBinContent(35, 0.11227209);
  fHChannelProfile->SetBinContent(36, 0.16687538);
  fHChannelProfile->SetBinContent(37, 0.19926773);
  fHChannelProfile->SetBinContent(38, 0.11099902);
  fHChannelProfile->SetBinContent(101, 0.8640627);
  fHChannelProfile->SetBinContent(102, 0.4562876);
  fHChannelProfile->SetBinContent(103, 0.2980073);
  fHChannelProfile->SetBinContent(104, 0.3311913);
  fHChannelProfile->SetBinContent(105, 0.15270268);
  fHChannelProfile->SetBinContent(106, 0.852323);
  fHChannelProfile->SetBinContent(107, 0.6038415);
  fHChannelProfile->SetBinContent(108, 0.4019715);
  fHChannelProfile->SetBinContent(109, 0.2699849);
  fHChannelProfile->SetBinContent(110, 0.3297998);
  fHChannelProfile->SetBinContent(111, 0.17129891);
  fHChannelProfile->SetBinContent(112, 0.5420772);
  fHChannelProfile->SetBinContent(113, 0.3949476);
  fHChannelProfile->SetBinContent(114, 0.4552159);
  fHChannelProfile->SetBinContent(115, 0.21998243);
  fHChannelProfile->SetBinContent(116, 0.12606388);
  fHChannelProfile->SetBinContent(117, 0.3624613);
  fHChannelProfile->SetBinContent(118, 0.3169082);
  fHChannelProfile->SetBinContent(119, 0.3945664);
  fHChannelProfile->SetBinContent(120, 0.2219344);
  fHChannelProfile->SetBinContent(121, 0.128652082);
  fHChannelProfile->SetBinContent(122, 0.4644963);
  fHChannelProfile->SetBinContent(123, 0.2820383);
  fHChannelProfile->SetBinContent(124, 0.18354501);
  fHChannelProfile->SetBinContent(125, 0.083149113);
  fHChannelProfile->SetBinContent(126, 0.3783994);
  fHChannelProfile->SetBinContent(127, 0.26754058);
  fHChannelProfile->SetBinContent(128, 0.19762177);
  fHChannelProfile->SetBinContent(129, 0.080049982);
  fHChannelProfile->SetBinContent(130, 0.21149588);
  fHChannelProfile->SetBinContent(131, 0.17809785);
  fHChannelProfile->SetBinContent(132, 0.15128445);
  fHChannelProfile->SetBinContent(133, 0.23763936);
  fHChannelProfile->SetBinContent(134, 0.17085315);
  fHChannelProfile->SetBinContent(135, 0.108395649);
  fHChannelProfile->SetBinContent(136, 0.1594975);
  fHChannelProfile->SetBinContent(137, 0.16797845);
  fHChannelProfile->SetBinContent(138, 0.1328866);
  fHChannelProfile->SetBinContent(201, 0.7986985);
  fHChannelProfile->SetBinContent(202, 0.4555452);
  fHChannelProfile->SetBinContent(203, 0.2724073);
  fHChannelProfile->SetBinContent(204, 0.2952394);
  fHChannelProfile->SetBinContent(205, 0.15997161);
  fHChannelProfile->SetBinContent(206, 0.7100203);
  fHChannelProfile->SetBinContent(207, 0.543966);
  fHChannelProfile->SetBinContent(208, 0.3717228);
  fHChannelProfile->SetBinContent(209, 0.2802266);
  fHChannelProfile->SetBinContent(210, 0.3217942);
  fHChannelProfile->SetBinContent(211, 0.1659847);
  fHChannelProfile->SetBinContent(212, 0.5156062);
  fHChannelProfile->SetBinContent(213, 0.4024426);
  fHChannelProfile->SetBinContent(214, 0.4609129);
  fHChannelProfile->SetBinContent(215, 0.27420047);
  fHChannelProfile->SetBinContent(216, 0.11368959);
  fHChannelProfile->SetBinContent(217, 0.3701904);
  fHChannelProfile->SetBinContent(218, 0.279607);
  fHChannelProfile->SetBinContent(219, 0.4097428);
  fHChannelProfile->SetBinContent(220, 0.22693623);
  fHChannelProfile->SetBinContent(221, 0.11680092);
  fHChannelProfile->SetBinContent(222, 0.4938436);
  fHChannelProfile->SetBinContent(223, 0.3124787);
  fHChannelProfile->SetBinContent(224, 0.19538715);
  fHChannelProfile->SetBinContent(225, 0.06828545);
  fHChannelProfile->SetBinContent(226, 0.342157);
  fHChannelProfile->SetBinContent(227, 0.29042872);
  fHChannelProfile->SetBinContent(228, 0.17713656);
  fHChannelProfile->SetBinContent(229, 0.04579851);
  fHChannelProfile->SetBinContent(230, 0.27158074);
  fHChannelProfile->SetBinContent(231, 0.16644108);
  fHChannelProfile->SetBinContent(232, 0.12787917);
  fHChannelProfile->SetBinContent(233, 0.20873927);
  fHChannelProfile->SetBinContent(234, 0.15133836);
  fHChannelProfile->SetBinContent(235, 0.10471767);
  fHChannelProfile->SetBinContent(236, 0.15217519);
  fHChannelProfile->SetBinContent(237, 0.1674496);
  fHChannelProfile->SetBinContent(238, 0.11546263);
  fHChannelProfile->SetBinContent(301, 0.8124842);
  fHChannelProfile->SetBinContent(302, 0.4577255);
  fHChannelProfile->SetBinContent(303, 0.2609172);
  fHChannelProfile->SetBinContent(304, 0.3384207);
  fHChannelProfile->SetBinContent(305, 0.14532476);
  fHChannelProfile->SetBinContent(306, 0.6983289);
  fHChannelProfile->SetBinContent(307, 0.5621696);
  fHChannelProfile->SetBinContent(308, 0.3504489);
  fHChannelProfile->SetBinContent(309, 0.2378091);
  fHChannelProfile->SetBinContent(310, 0.3051651);
  fHChannelProfile->SetBinContent(311, 0.14408575);
  fHChannelProfile->SetBinContent(312, 0.5240985);
  fHChannelProfile->SetBinContent(313, 0.434728);
  fHChannelProfile->SetBinContent(314, 0.4906144);
  fHChannelProfile->SetBinContent(315, 0.293537);
  fHChannelProfile->SetBinContent(316, 0.15719219);
  fHChannelProfile->SetBinContent(317, 0.3361993);
  fHChannelProfile->SetBinContent(318, 0.2845095);
  fHChannelProfile->SetBinContent(319, 0.4127078);
  fHChannelProfile->SetBinContent(320, 0.20873153);
  fHChannelProfile->SetBinContent(321, 0.10081403);
  fHChannelProfile->SetBinContent(322, 0.4723497);
  fHChannelProfile->SetBinContent(323, 0.3257033);
  fHChannelProfile->SetBinContent(324, 0.25693134);
  fHChannelProfile->SetBinContent(325, 0.11226882);
  fHChannelProfile->SetBinContent(326, 0.3461105);
  fHChannelProfile->SetBinContent(327, 0.21357958);
  fHChannelProfile->SetBinContent(328, 0.17152777);
  fHChannelProfile->SetBinContent(329, 0.05597577);
  fHChannelProfile->SetBinContent(330, 0.2797932);
  fHChannelProfile->SetBinContent(331, 0.23854321);
  fHChannelProfile->SetBinContent(332, 0.16638216);
  fHChannelProfile->SetBinContent(333, 0.18716902);
  fHChannelProfile->SetBinContent(334, 0.1611088);
  fHChannelProfile->SetBinContent(335, 0.11637052);
  fHChannelProfile->SetBinContent(336, 0.18596727);
  fHChannelProfile->SetBinContent(337, 0.19434136);
  fHChannelProfile->SetBinContent(338, 0.11685794);

  // K-->pinn acceptances [%] for decays produced with 105m<z<165m and 15GeV/c<p<65GeV/c.
  // They are loaded into the high PMT occupancy plot.
  fHChannelProfile->SetBinContent(51, 0.99);
  fHChannelProfile->SetBinContent(52, 1.09);
  fHChannelProfile->SetBinContent(53, 0.98);
  fHChannelProfile->SetBinContent(54, 0.61);
  fHChannelProfile->SetBinContent(55, 0.2);
  fHChannelProfile->SetBinContent(56, 0.62);
  fHChannelProfile->SetBinContent(57, 0.98);
  fHChannelProfile->SetBinContent(58, 1.02);
  fHChannelProfile->SetBinContent(59, 0.76);
  fHChannelProfile->SetBinContent(60, 0.56);
  fHChannelProfile->SetBinContent(61, 0.17);
  fHChannelProfile->SetBinContent(62, 1.06);
  fHChannelProfile->SetBinContent(63, 1.17);
  fHChannelProfile->SetBinContent(64, 1.6);
  fHChannelProfile->SetBinContent(65, 0.48);
  fHChannelProfile->SetBinContent(66, 0.21);
  fHChannelProfile->SetBinContent(67, 1.2);
  fHChannelProfile->SetBinContent(68, 1.05);
  fHChannelProfile->SetBinContent(69, 1.39);
  fHChannelProfile->SetBinContent(70, 0.41);
  fHChannelProfile->SetBinContent(71, 0.14);
  fHChannelProfile->SetBinContent(72, 2.33);
  fHChannelProfile->SetBinContent(73, 1.2);
  fHChannelProfile->SetBinContent(74, 0.32);
  fHChannelProfile->SetBinContent(75, 0.13);
  fHChannelProfile->SetBinContent(76, 1.82);
  fHChannelProfile->SetBinContent(77, 0.93);
  fHChannelProfile->SetBinContent(78, 0.36);
  fHChannelProfile->SetBinContent(79, 0.05);
  fHChannelProfile->SetBinContent(80, 1.44);
  fHChannelProfile->SetBinContent(81, 0.63);
  fHChannelProfile->SetBinContent(82, 0.23);
  fHChannelProfile->SetBinContent(83, 0.72);
  fHChannelProfile->SetBinContent(84, 0.35);
  fHChannelProfile->SetBinContent(85, 0.11);
  fHChannelProfile->SetBinContent(86, 0.57);
  fHChannelProfile->SetBinContent(87, 0.31);
  fHChannelProfile->SetBinContent(88, 0.29);
  fHChannelProfile->SetBinContent(151, 0.9);
  fHChannelProfile->SetBinContent(152, 1.01);
  fHChannelProfile->SetBinContent(153, 0.71);
  fHChannelProfile->SetBinContent(154, 0.7);
  fHChannelProfile->SetBinContent(155, 0.16);
  fHChannelProfile->SetBinContent(156, 0.68);
  fHChannelProfile->SetBinContent(157, 1.13);
  fHChannelProfile->SetBinContent(158, 1.05);
  fHChannelProfile->SetBinContent(159, 0.75);
  fHChannelProfile->SetBinContent(160, 0.67);
  fHChannelProfile->SetBinContent(161, 0.18);
  fHChannelProfile->SetBinContent(162, 1.04);
  fHChannelProfile->SetBinContent(163, 0.98);
  fHChannelProfile->SetBinContent(164, 1.67);
  fHChannelProfile->SetBinContent(165, 0.5);
  fHChannelProfile->SetBinContent(166, 0.16);
  fHChannelProfile->SetBinContent(167, 1.17);
  fHChannelProfile->SetBinContent(168, 1.14);
  fHChannelProfile->SetBinContent(169, 1.43);
  fHChannelProfile->SetBinContent(170, 0.32);
  fHChannelProfile->SetBinContent(171, 0.15);
  fHChannelProfile->SetBinContent(172, 2.07);
  fHChannelProfile->SetBinContent(173, 1.22);
  fHChannelProfile->SetBinContent(174, 0.33);
  fHChannelProfile->SetBinContent(175, 0.11);
  fHChannelProfile->SetBinContent(176, 1.65);
  fHChannelProfile->SetBinContent(177, 0.95);
  fHChannelProfile->SetBinContent(178, 0.32);
  fHChannelProfile->SetBinContent(179, 0.09);
  fHChannelProfile->SetBinContent(180, 1.26);
  fHChannelProfile->SetBinContent(181, 0.61);
  fHChannelProfile->SetBinContent(182, 0.28);
  fHChannelProfile->SetBinContent(183, 1.01);
  fHChannelProfile->SetBinContent(184, 0.41);
  fHChannelProfile->SetBinContent(185, 0.13);
  fHChannelProfile->SetBinContent(186, 0.43);
  fHChannelProfile->SetBinContent(187, 0.29);
  fHChannelProfile->SetBinContent(188, 0.34);
  fHChannelProfile->SetBinContent(251, 0.44);
  fHChannelProfile->SetBinContent(252, 0.34);
  fHChannelProfile->SetBinContent(253, 0.57);
  fHChannelProfile->SetBinContent(254, 1.6);
  fHChannelProfile->SetBinContent(255, 1.71);
  fHChannelProfile->SetBinContent(256, 0.68);
  fHChannelProfile->SetBinContent(257, 0.53);
  fHChannelProfile->SetBinContent(258, 0.51);
  fHChannelProfile->SetBinContent(259, 0.64);
  fHChannelProfile->SetBinContent(260, 1.81);
  fHChannelProfile->SetBinContent(261, 1.95);
  fHChannelProfile->SetBinContent(262, 0.75);
  fHChannelProfile->SetBinContent(263, 0.71);
  fHChannelProfile->SetBinContent(264, 1.3);
  fHChannelProfile->SetBinContent(265, 1.89);
  fHChannelProfile->SetBinContent(266, 1.37);
  fHChannelProfile->SetBinContent(267, 0.91);
  fHChannelProfile->SetBinContent(268, 0.84);
  fHChannelProfile->SetBinContent(269, 1.9);
  fHChannelProfile->SetBinContent(270, 1.88);
  fHChannelProfile->SetBinContent(271, 1.41);
  fHChannelProfile->SetBinContent(272, 2.15);
  fHChannelProfile->SetBinContent(273, 1.91);
  fHChannelProfile->SetBinContent(274, 1.94);
  fHChannelProfile->SetBinContent(275, 0.85);
  fHChannelProfile->SetBinContent(276, 2.11);
  fHChannelProfile->SetBinContent(277, 2.04);
  fHChannelProfile->SetBinContent(278, 1.72);
  fHChannelProfile->SetBinContent(279, 0.53);
  fHChannelProfile->SetBinContent(280, 1.72);
  fHChannelProfile->SetBinContent(281, 1.5);
  fHChannelProfile->SetBinContent(282, 1.05);
  fHChannelProfile->SetBinContent(283, 1.09);
  fHChannelProfile->SetBinContent(284, 1.31);
  fHChannelProfile->SetBinContent(285, 0.75);
  fHChannelProfile->SetBinContent(286, 0.73);
  fHChannelProfile->SetBinContent(287, 0.97);
  fHChannelProfile->SetBinContent(288, 0.47);
  fHChannelProfile->SetBinContent(351, 0.47);
  fHChannelProfile->SetBinContent(352, 0.44);
  fHChannelProfile->SetBinContent(353, 0.55);
  fHChannelProfile->SetBinContent(354, 1.72);
  fHChannelProfile->SetBinContent(355, 1.78);
  fHChannelProfile->SetBinContent(356, 0.43);
  fHChannelProfile->SetBinContent(357, 0.37);
  fHChannelProfile->SetBinContent(358, 0.3);
  fHChannelProfile->SetBinContent(359, 0.44);
  fHChannelProfile->SetBinContent(360, 1.32);
  fHChannelProfile->SetBinContent(361, 1.54);
  fHChannelProfile->SetBinContent(362, 0.83);
  fHChannelProfile->SetBinContent(363, 0.8);
  fHChannelProfile->SetBinContent(364, 1.33);
  fHChannelProfile->SetBinContent(365, 2);
  fHChannelProfile->SetBinContent(366, 1.47);
  fHChannelProfile->SetBinContent(367, 0.94);
  fHChannelProfile->SetBinContent(368, 0.96);
  fHChannelProfile->SetBinContent(369, 1.63);
  fHChannelProfile->SetBinContent(370, 1.93);
  fHChannelProfile->SetBinContent(371, 1.02);
  fHChannelProfile->SetBinContent(372, 2.05);
  fHChannelProfile->SetBinContent(373, 1.89);
  fHChannelProfile->SetBinContent(374, 1.83);
  fHChannelProfile->SetBinContent(375, 0.76);
  fHChannelProfile->SetBinContent(376, 1.87);
  fHChannelProfile->SetBinContent(377, 2.07);
  fHChannelProfile->SetBinContent(378, 1.51);
  fHChannelProfile->SetBinContent(379, 0.57);
  fHChannelProfile->SetBinContent(380, 1.54);
  fHChannelProfile->SetBinContent(381, 1.84);
  fHChannelProfile->SetBinContent(382, 1.44);
  fHChannelProfile->SetBinContent(383, 1.39);
  fHChannelProfile->SetBinContent(384, 1.13);
  fHChannelProfile->SetBinContent(385, 0.8);
  fHChannelProfile->SetBinContent(386, 0.91);
  fHChannelProfile->SetBinContent(387, 0.96);
  fHChannelProfile->SetBinContent(388, 0.38);
}
