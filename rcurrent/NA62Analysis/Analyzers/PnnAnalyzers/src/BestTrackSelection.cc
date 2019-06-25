#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "BestTrackSelection.hh"
#include "BeamParameters.hh"
#include "TwoLinesCDA.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "SpectrometerTrackCorrections.hh"
#include "BlueTubeTracker.hh"
#include "TriggerConditions.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "CalorimeterCluster.hh"
#include "NA62ConditionsService.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

BestTrackSelection::BestTrackSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "BestTrackSelection")
{
  RequestTree(new TRecoLKrEvent);
  RequestTree(new TRecoCedarEvent);
  RequestTree(new TRecoCHODEvent);
  RequestTree(new TRecoSpectrometerEvent);
  RequestTree(new TRecoGigaTrackerEvent);
  RequestBeamData();

  AddParam("UseGTK", "bool", &UseGTK, false); //false
  AddParam("CutNStrawChambers", "int", &fCutNStrawChambers, 4);
  AddParam("CutTrackChi2", "double", &fCutTrackChi2, 20.);
  AddParam("CutMomDiffBefAftFit", "double", &fCutMomDiffBefAftFit, 20000.);
  AddParam("WantNoMultitracks", "bool", &fWantNoMultitracks, true);
  AddParam("RejectBroadMultitracks", "bool", &fRejectBroadMultitracks, true);
  AddParam("RejectFullMultitracks", "bool", &fRejectFullMultitracks, false);
  AddParam("CutFullMultitrackCDA", "double", &fCutFullMultitrackCDA, 30.); //15., RG use 1/2CDA
  AddParam("CutBroadMultitrackCDA", "double", &fCutBroadMultitrackCDA, 60.); //30., RG use 1/2CDA
  AddParam("CutMultitrackMinZ", "double", &fCutMultitrackMinZ, 60000.);
  AddParam("CutMultitrackMaxZ", "double", &fCutMultitrackMaxZ, 200000.);
  AddParam("CutMultitrackTimeDiff", "double", &fCutMultitrackTimeDiff, 50.);
  AddParam("CutTimeDiffSTRAWCHOD", "double", &fCutTimeDiffSTRAWCHOD, 20.);
  AddParam("CHODAssocSigmaX", "double", &fCHODAssocSigmaX, 13.);
  AddParam("CHODAssocSigmaT", "double", &fCHODAssocSigmaT, 5.6);
  AddParam("CutDCHOD", "double", &fCutDCHOD, 15.);
  //2016 25ns, 2017 5ns
  AddParam("CutTimeDiffTriggerCHOD", "double", &fCutTimeDiffTriggerCHOD, 25.);
  AddParam("CutTimeDiffNewCHODHitCHOD", "double", &fCutTimeDiffNewCHODHitCHOD, 5.);
  AddParam("SigmaPosNewCHOD", "double", &fSigmaPosNewCHOD, 3.*16.);
  AddParam("SigmaTimeNewCHOD", "double", &fSigmaTimeNewCHOD, 2.*7.); //2*5.8
  AddParam("CutDiscriminantNewCHOD", "double", &fCutDiscriminantNewCHOD, 10.);
  AddParam("CutTimeDiffCHODNewCHOD", "double", &fCutTimeDiffCHODNewCHOD, 5.);
  AddParam("CutTimeDiffRICHCHOD", "double", &fCutTimeDiffRICHCHOD, 2.);
  AddParam("OffsetLKrStandard", "double", &fOffsetLKrStandard, -1.1); //[ns]
  AddParam("CutMaxDistLKrStandard", "double", &fCutMaxDistLKrStandard, 150.);
  AddParam("CutTimeDiffLKrStandardSTRAW", "double", &fCutTimeDiffLKrStandardSTRAW, 30.);
  AddParam("OffsetLKrCell", "double", &fOffsetLKrCell, -2.); //[ns]
  AddParam("CutMaxDistLKrCell", "double", &fCutMaxDistLKrCell, 100.);
  AddParam("CutTimeDiffLKrCellSTRAW", "double", &fCutTimeDiffLKrCellSTRAW, 20.);
  AddParam("CutMinEnergy", "double", &fCutMinEnergy, 40.);
  AddParam("CutTimeDiffLKrClusterSTRAW", "double", &fCutTimeDiffLKrClusterSTRAW, 20.);
  AddParam("CutMaxDistLKr", "double", &fCutMaxDistLKr, 100.);
  AddParam("CutTimeDiffLKrSTRAW", "double", &fCutTimeDiffLKrSTRAW, 20.);
  AddParam("CutTimeDiffLKrCHOD", "double", &fCutTimeDiffLKrCHOD, 6.);
  AddParam("WeightCHOD", "double", &fWeightCHOD, 0.5);
  AddParam("WeightLKr", "double", &fWeightLKr, 1.);
  AddParam("WeightRICH", "double", &fWeightRICH, 0.2);
  AddParam("WeightSTRAW", "double", &fWeightSTRAW, 10.);
  AddParam("CutMinNSectorsKTAG", "int", &fCutMinNSectorsKTAG, 5);
  AddParam("CutTimeDiffCedarDownstreamData", "double", &fCutTimeDiffCedarDownstreamData, 1.);
  AddParam("CutTimeDiffCedarDownstreamMC", "double", &fCutTimeDiffCedarDownstreamMC, 2.);
  AddParam("Verbosity", "bool", &verb, false);

  fMatchingRG = new MatchingRG(ba, this, "MatchingRG");
  fMatchingRG->Init("");

  fGTKReco = new GigaTrackerRecoAlgorithm(ba, this, "GTKRecoAlgo");
  fGTKReco->SetRedoXYCorr(1);

  ftracker = BlueTubeTracker::GetInstance();
}

void BestTrackSelection::InitOutput(){
  RegisterOutput("BestTracks", &fBestTracks);
  RegisterOutput("BestTrackTime", &fBestTrackTime);
  RegisterOutput("BestTrackSTRAWTime", &fBestTrackSTRAWTime);
  RegisterOutput("BestTrackCHODTime", &fBestTrackCHODTime);
  RegisterOutput("BestTrackNewCHODTime", &fBestTrackNewCHODTime);
  RegisterOutput("CHODAssocID", &fCHODAssocID);
  RegisterOutput("NewCHODAssocID", &fNewCHODAssocID);
  RegisterOutput("RICHStandardAssoc", &fRICHStandardAssoc);
  RegisterOutput("RICHSingleAssoc", &fRICHSingleAssoc);
  RegisterOutput("BestTrackRICHTime", &fBestTrackRICHTime);
  RegisterOutput("LKrStandardAssoc", &fLKrStandardAssoc);
  RegisterOutput("LKrChargedAssoc", &fLKrChargedAssoc);
  RegisterOutput("BestTrackLKrTime", &fBestTrackLKrTime);
  RegisterOutput("LKrAssocID", &fLKrAssocID);
  RegisterOutput("LKrAssocEnergy", &fLKrAssocEnergy);
  RegisterOutput("LKrAssocSeedEnergy", &fLKrAssocSeedEnergy);
  RegisterOutput("LKrAssocNCells", &fLKrAssocNCells);
  RegisterOutput("LKrAssocPosition", &fLKrAssocPosition);
  RegisterOutput("BestTrackKTAGTime", &fBestTrackKTAGTime);
  RegisterOutput("GTKAssocID", &fGTKAssocID);
  RegisterOutput("BestTrackGTKTime", &fBestTrackGTKTime);
  RegisterOutput("GTKAssocVertex", &fGTKAssocVertex);
  RegisterOutput("GTKAllAssocVertices", &fGTKAllAssocVertices);
  RegisterOutput("GTKAssocQuality1", &fGTKAssocQuality1);
  RegisterOutput("GTKAssocQuality2", &fGTKAssocQuality2);
  RegisterOutput("GTKAssocMomentum", &fGTKAssocMomentum);
  RegisterOutput("GTKAssocTrackMomentum", &fGTKAssocTrackMomentum);
  RegisterOutput("GTKNAssocs", &fGTKNAssocs);
  RegisterOutput("NomVertex", &fNomVertex);
  RegisterOutput("BestTrackNomMomentum", &fBestTrackNomMomentum);
  RegisterOutput("NomKaonMomentum", &fNomKaonMomentum);
}

void BestTrackSelection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH1I("hNKTAGCandidates", "hNKTAGCandidates", 50, 0, 50));
    BookHisto(new TH1I("hNKTAGCandidatesAtLeast5Sectors", "hNKTAGCandidatesAtLeast5Sectors", 10, 0, 10));
    BookHisto(new TH1I("hNSTRAWChambers", "hNSTRAWChambers", 5, 0, 5));
    BookHisto(new TH1D("hTrackChi2", "hTrackChi2", 100, 0., 100.));
    BookHisto(new TH1D("hTrackMomentum", "hTrackMomentum", 850, 0., 85000.));
    BookHisto(new TH1D("hTrackMomentumBeforeFit", "hTrackMomentumBeforeFit", 850, 0., 85000.));
    BookHisto(new TH1D("hTrackMomentumDifference", "hTrackMomentumDifference", 300, 0., 30000.));
    BookHisto(new TH1I("hMultitrack", "hMultitrack", 2, 0, 2));
    BookHisto(new TH2D("hAtSTRAW1YvsX", "hAtSTRAW1YvsX", 240, -1200., 1200., 240, -1200., 1200.));
    BookHisto(new TH2D("hAtSTRAW2YvsX", "hAtSTRAW2YvsX", 240, -1200., 1200., 240, -1200., 1200.));
    BookHisto(new TH2D("hAtSTRAW3YvsX", "hAtSTRAW3YvsX", 240, -1200., 1200., 240, -1200., 1200.));
    BookHisto(new TH2D("hAtSTRAW4YvsX", "hAtSTRAW4YvsX", 240, -1200., 1200., 240, -1200., 1200.));
    BookHisto(new TH2D("hAtRICHFrontPlaneYvsX", "hAtRICHFrontPlaneYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtRICHBackPlaneYvsX", "hAtRICHBackPlaneYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtNewCHODYvsX", "hAtNewCHODYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtHPlaneCHODYvsX", "hAtCHODYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtVPlaneCHODYvsX", "hAtCHODYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtLKrYvsX", "hAtLKrYvsX", 440, -2200., 2200., 440, -2200., 2200.));
    BookHisto(new TH2D("hAtMUV1YvsX", "hAtMUV1YvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH2D("hAtMUV2YvsX", "hAtMUV2YvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH2D("hAtMUV3YvsX", "hAtMUV3YvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH2D("hAtLAV12FrontYvsX", "hAtLAV12YvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH2D("hAtLAV12BackYvsX", "hAtLAV12YvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH2D("hAtIRCYvsX", "hAtIRCYvsX", 500, -2500., 2500., 500, -2500., 2500.));
    BookHisto(new TH1I("hInAcceptance", "hInAcceptance", 2, 0, 2));
    BookHisto(new TH1I("hNAssocRecCHOD", "hNAssocRecCHOD", 10, 0, 10));
    BookHisto(new TH1D("hTimeDiffBestAssocCHODSTRAW", "hTimeDiffBestAssocCHODSTRAW", 100, -25., 25.));
    BookHisto(new TH1D("hDchod", "hDchod", 100, 0., 100.));
    BookHisto(new TH1D("hDistanceTrackCHODAssociation", "hDistanceTrackCHODAssociation", 100, 0., 1000.));
    BookHisto(new TH1D("hTimeDiffBestAssocCHODTrigger", "hTimeDiffBestAssocCHODTrigger", 500, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestCHODTrigger", "hTimeDiffBestCHODTrigger", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestCHODSTRAW", "hTimeDiffBestCHODSTRAW", 200, -25., 25.));
    BookHisto(new TH1I("hNAssocRecNewCHOD", "hNAssocRecNewCHOD", 10, 0, 10));
    BookHisto(new TH1D("hPosDiffAtNewCHOD", "hPosDiffAtNewCHOD", 100, 0., 500.));
    BookHisto(new TH2D("hPosDiffAtNewCHODYvsX", "hPosDiffAtNewCHODYvsX", 100, -500., 500., 100, -500., 500.));
    BookHisto(new TH1D("hTimeDiffNewCHODSTRAW", "hTimeDiffNewCHODSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hDiscriminantNewCHOD", "hDiscriminantNewCHOD", 50, 0., 50.));
    BookHisto(new TH1D("hTimeDiffBestNewCHODSTRAW", "hTimeDiffBestNewCHODSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestNewCHODTrigger", "hTimeDiffBestNewCHODTrigger", 200, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestNewCHODCHOD", "hTimeDiffBestNewCHODCHOD", 200, -25., 25.));
    BookHisto(new TH1I("hValidAssocRICH", "hValidAssocRICH", 2, 0, 2));
    BookHisto(new TH1D("hTimeDiffStandardRICHCHOD", "hTimeDiffStandardRICHCHOD", 100, -5., 5.));
    BookHisto(new TH1I("hIsAssocRICHSingleRing", "hIsAssocRICHSingleRIng", 2, 0, 2));
    BookHisto(new TH1D("hTimeDiffBestAssocRICHCHOD", "hTimeDiffBestAssocRICHCHOD", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestRICHCHOD", "hTimeDiffBestRICHCHOD", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestRICHSTRAW", "hTimeDiffBestRICHSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestRICHTrigger", "hTimeDiffBestRICHTrigger", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestRICHNewCHOD", "hTimeDiffBestRICHNewCHOD", 100, -5., 5.));
    BookHisto(new TH1D("hLKrAssocDistLKrTrack", "hLKrAssocDistLKrTrack", 500, 0., 2500.));
    BookHisto(new TH1D("hLKrAssocTimeDiffLKrSTRAW", "hLKrAssocTimeDiffLKrSTRAW", 400, -50., 50.));
    BookHisto(new TH1I("hLKrMatch1Found", "hLKrMatch1Found", 2, 0, 2));
    BookHisto(new TH1D("hDistLKrHitTrack", "hDistLKrHitTrack", 500, 0., 2500.));
    BookHisto(new TH1D("hTimeDiffLKrHitSTRAW", "hTimeDiffLKrHitSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hLKrAssocClusterMaxHitEnergy", "hLKrAssocClusterMaxHitEnergy", 400, 0., 2000.));
    BookHisto(new TH1D("hLKrAssocClusterTimeDiffClusterSTRAW", "hLKrAssocClusterTimeDiffClusterSTRAW", 200, -25., 25.));
    BookHisto(new TH1I("hLKrMatch2Found", "hLKrMatch2Found", 2, 0, 2));
    BookHisto(new TH1I("hLKrMatchFound", "hLKrMatchFound", 2, 1, 3));
    BookHisto(new TH1D("hTimeDiffBestLKrSTRAW", "hTimeDiffBestLKrSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestLKrCHOD", "hTimeDiffBestLKrCHOD", 200, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestLKrNewCHOD", "hTimeDiffBestLKrNewCHOD", 200, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestLKrRICH", "hTimeDiffBestLKrRICH", 200, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestLKrTrigger", "hTimeDiffBestLKrTrigger", 200, -5., 5.));
    BookHisto(new TH2D("hEoverPvsTimeDiffLKrCHOD", "hEoverPvsTimeDiffLKrCHOD", 200, -5., 5., 500, 0., 1.));
    BookHisto(new TH1D("hTimeCHOD", "hTimeCHOD", 500, -50., 50.));
    BookHisto(new TH1D("hTimeLKr", "hTimeLKr", 500, -50., 50.));
    BookHisto(new TH1D("hTimeRICH", "hTimeRICH", 500, -50., 50.));
    BookHisto(new TH1D("hTimeSTRAW", "hTimeSTRAW", 500, -50., 50.));
    BookHisto(new TH1D("hTimeCHODWeighted", "hTimeCHODWeighted", 1000, -50., 50.));
    BookHisto(new TH1D("hTimeLKrWeighted", "hTimeLKrWeighted", 1000, -50., 50.));
    BookHisto(new TH1D("hTimeRICHWeighted", "hTimeRICHWeighted", 1000, -50., 50.));
    BookHisto(new TH1D("hTimeSTRAWWeighted", "hTimeSTRAWWeighted", 1000, -50., 50.));
    BookHisto(new TH1D("hTrackTime", "hTrackTime", 500, -50., 50.));
    BookHisto(new TH1D("hTimeDiffKTAGDownstream", "hTimeDiffKTAGDownstream", 100, -5., 5.));
    BookHisto(new TH1I("hKTAGMatchFound", "hKTAGMatchFound", 2, 0, 2));
    BookHisto(new TH1I("hKTAGNsectors", "hKTAGNsectors", 9, 0, 9));
    BookHisto(new TH1D("hTimeDiffGoodKTAGDownstream", "hTimeDiffGoodKTAGDownstream", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGDownstream", "hTimeDiffBestKTAGDownstream", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGCHOD", "hTimeDiffBestKTAGCHOD", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGRICH", "hTimeDiffBestKTAGRICH", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGLKr", "hTimeDiffBestKTAGLKr", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGSTRAW", "hTimeDiffBestKTAGSTRAW", 200, -25., 25.));
    BookHisto(new TH1D("hTimeDiffBestKTAGCHODWeighted", "hTimeDiffBestKTAGCHODWeighted", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGRICHWeighted", "hTimeDiffBestKTAGRICHWeighted", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGLKrWeighted", "hTimeDiffBestKTAGLKrWeighted", 100, -5., 5.));
    BookHisto(new TH1D("hTimeDiffBestKTAGSTRAWWeighted", "hTimeDiffBestKTAGSTRAWWeighted", 100, -5., 5.));
    BookHisto(new TH1I("hGTKMatchFound", "hGTKMatchFound", 2, 0, 2));
    BookHisto(new TH1D("hDiscriminant1DiffBestTwo", "hDiscriminant1DiffBestTwo", 100, 0., 1.));
    BookHisto(new TH1D("hMatchedGTKQuality1", "hMatchedGTKQuality1", 100, 0., 1.));
    BookHisto(new TH2D("hTimeDiffMatchedGTKRICHvsMatchedGTKQuality2", "hTimeDiffMatchedGTKRICHvsMatchedGTKQuality2", 100, 0., 1., 400, -2., 2.));
    BookHisto(new TH2D("hTimeDiffMatchedGTKKTAGvsMatchedGTKQuality1", "hTimeDiffMatchedGTKKTAGvsMatchedGTKQuality1", 100, 0., 1., 400, -2., 2.));
    BookHisto(new TH2D("hTrackGTKDistAtVertexVsMatchedGTKQuality1", "hTrackGTKDistAtVertexVsMatchedGTKQuality1", 100, 0., 1., 100, 0., 10.));
    BookHisto(new TH2D("hTrackGTKDistXAtVertexVsMatchedGTKQuality1", "hTrackGTKDistXAtVertexVsMatchedGTKQuality1", 100, 0., 1., 200, -10., 10.));
    BookHisto(new TH2D("hTrackGTKDistYAtVertexVsMatchedGTKQuality1", "hTrackGTKDistYAtVertexVsMatchedGTKQuality1", 100, 0., 1., 200, -10., 10.));
    BookHisto(new TH2D("hTrackGTKDistAtVertexVsMatchedGTKQuality2", "hTrackGTKDistAtVertexVsMatchedGTKQuality2", 100, 0., 1., 100, 0., 10.));
    BookHisto(new TH2D("hTrackGTKDistXAtVertexVsMatchedGTKQuality2", "hTrackGTKDistXAtVertexVsMatchedGTKQuality2", 100, 0., 1., 200, -10., 10.));
    BookHisto(new TH2D("hTrackGTKDistYAtVertexVsMatchedGTKQuality2", "hTrackGTKDistYAtVertexVsMatchedGTKQuality2", 100, 0., 1., 200, -10., 10.));
    BookHisto(new TH1D("hTimeDiffMatchedGTKRICH", "hTimeDiffMatchedGTKRICH", 400, -2., 2.));
    BookHisto(new TH1D("hTimeDiffMatchedGTKKTAG", "hTimeDiffMatchedGTKKTAG", 400, -2., 2.));
    BookHisto(new TH1D("hTrackGTKDistAtVertex", "hTrackGTKDistAtVertex", 500, 0., 50.));
    BookHisto(new TH2D("hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH", "hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH", 200, -2., 2., 200, 0., 50.));
    BookHisto(new TH2D("hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG", "hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG", 200, -2., 2., 200, 0., 50.));
    BookHisto(new TH1I("hNBestTracks", "hNBestTracks", 20, 0, 20));
    BookHisto(new TH1I("hNSTRAWChambers_bestTracks", "hNSTRAWChambers_bestTracks", 5, 0, 5));
  };
}

void BestTrackSelection::DefineMCSimple(){}

void BestTrackSelection::StartOfRunUser(){}

void BestTrackSelection::StartOfBurstUser(){}

void BestTrackSelection::Process(int iEvent){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"BestTrackSelection"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  (void)iEvent;

  PrepareOutputs();

  OutputState state;
  auto preselectedEvent =
    *(bool*)GetOutput("Preselection.PreselectedEvent", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++; //3

  if(verb) cout<<"Is event preselected? "<<preselectedEvent<<endl;
  if(!preselectedEvent){
    if(verb) cout<<"Event is not preselected"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto fakeTracks =
    *(std::vector<bool>*)GetOutput("FakeTrackSelection.FakeTracks", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  if(verb) cout<<"N fake tracks read = "<<std::count(fakeTracks.begin(), fakeTracks.end(), true)<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  tTrigger =
    *(double*)GetOutput("CheckTrigger.TriggerTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  if(verb) cout<<"Trigger time read = "<<tTrigger<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  fSpecCHOD =
    *(std::vector<SpectrometerCHODAssociationOutput>*)GetOutput("SpectrometerCHODAssociation.Output", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++; //7

  fSpecNewCHOD =
    *(std::vector<SpectrometerNewCHODAssociationOutput>*)GetOutput("SpectrometerNewCHODAssociation.Output", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto SpecRICH =
    *(std::vector<SpectrometerRICHAssociationOutput>*)GetOutput("SpectrometerRICHAssociation.Output", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto SpecRICHSingleRing =
    *(std::vector<SpectrometerRICHAssociationOutputSingleRing>*)GetOutput("SpectrometerRICHAssociationSingleRing.Output", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
   return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"all requested outputs are valid"<<endl;

  fSpecCalo = (TClonesArray*)GetOutput("SpectrometerCalorimetersAssociation.MatchedClusters");
  fLKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoCedarEvent* CedarEvent = GetEvent<TRecoCedarEvent>();
  fCHODEvent = GetEvent<TRecoCHODEvent>();
  TRecoGigaTrackerEvent *GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  fSTRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand;
  TRecoCedarCandidate *CedarCand;
  FillHisto("hNKTAGCandidates", CedarEvent->GetNCandidates());
  int n5sec = 0;
  for(int m=0; m<CedarEvent->GetNCandidates(); m++){
    CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(m));
    if(CedarCand->GetNSectors()>4) n5sec++;
  };
  FillHisto("hNKTAGCandidatesAtLeast5Sectors", n5sec);

  double tDownstream = 0.;
  if(verb) cout<<"N STRAW candidates "<<fSTRAWEvent->GetNCandidates()<<endl;
  if(verb) cout<<endl;
  for(int i=0; i<fSTRAWEvent->GetNCandidates(); i++){
    if(verb) cout<<"+++++ Straw candidate "<<i<<" +++++"<<endl;

    RestoreDefaultOutputs();

    STRAWCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(i));
    double tSTRAW = STRAWCand->GetTime();
    if(verb) cout<<"Straw time = "<<tSTRAW<<endl;

    if(verb){
      cout<<endl;
      cout<<"----Basic conditions----"<<endl;
    };
    FillHisto("hNSTRAWChambers", STRAWCand->GetNChambers());
    if(verb) cout<<"Track N chambers: "<<STRAWCand->GetNChambers()<<" >= "<<fCutNStrawChambers<<endl;
    if(STRAWCand->GetNChambers()<fCutNStrawChambers) continue;

    FillHisto("hTrackChi2", STRAWCand->GetChi2());
    if(verb) cout<<"Track Chi2: "<<STRAWCand->GetChi2()<<" <= "<<fCutTrackChi2<<endl;
    if(STRAWCand->GetChi2()>fCutTrackChi2) continue;

    FillHisto("hTrackMomentum", STRAWCand->GetMomentum());
    FillHisto("hTrackMomentumBeforeFit", STRAWCand->GetMomentumBeforeFit());
    FillHisto("hTrackMomentumDifference", fabs(STRAWCand->GetMomentum()-STRAWCand->GetMomentumBeforeFit()));
    if(verb){
      cout<<"Track momentum: "<<STRAWCand->GetThreeMomentumBeforeMagnet().X()<<" "<<STRAWCand->GetThreeMomentumBeforeMagnet().Y()<<" "<<STRAWCand->GetThreeMomentumBeforeMagnet().Z()<<endl;
      cout<<"Track Momentum Difference: "<<fabs(STRAWCand->GetMomentum() - STRAWCand->GetMomentumBeforeFit())<<" <= "<<fCutMomDiffBefAftFit<<endl;
    };
    if(fabs(STRAWCand->GetMomentum() - STRAWCand->GetMomentumBeforeFit())>fCutMomDiffBefAftFit) continue;
    if(verb) cout<<"----passed basic conditions-----"<<endl;

    if(verb){
      cout<<endl;
      cout<<"----Fake/multitrack conditions----"<<endl;
    };
    if(verb) cout<<"Is fake track? "<<fakeTracks.at(i)<<endl;
    if(fakeTracks.at(i)==true) continue;
    if(verb) cout<<"is not fake"<<endl;

    if(verb) cout<<"Want no multitracks? "<<fWantNoMultitracks<<endl;
    if(verb) cout<<"Broad? "<<fRejectBroadMultitracks<<" Full? "<<fRejectFullMultitracks<<endl;
    if(fWantNoMultitracks){
      std::pair<bool, bool> ismulti = is_multi_track(i, fakeTracks);
      bool ismultitrack = (fRejectFullMultitracks && ismulti.first) || (fRejectBroadMultitracks && ismulti.second);
      if(verb) cout<<"Is multitrack? Broad: "<<ismulti.second<<" Full: "<<ismulti.first<<endl;
      FillHisto("hMultitrack", (int)ismultitrack);
      if(ismultitrack) continue;
      if(verb) cout<<"is not multitrack"<<endl;
    };
    if(verb) cout<<"----passed fake/multitrack conditions-----"<<endl;

    //acceptance
    if(verb){
      cout<<endl;
      cout<<"----Acceptance conditions----"<<endl;
    };
    bool acceptanceOK = AcceptanceOK(STRAWCand);
    FillHisto("hInAcceptance", (int)acceptanceOK);
    if(verb) cout<<"Track in acceptance? "<<acceptanceOK<<endl;
    if(!acceptanceOK) continue;
    if(verb) cout<<"is in acceptance"<<endl;
    if(verb) cout<<"----passed acceptance conditions-----"<<endl;

    // CHOD assoc
    if(verb){
      cout<<endl;
      cout<<"----CHOD association----"<<endl;
    };
    double tCHOD = -999999.;
    int CHODID = -1;
    bool hasCHOD = HasCHODAssoc(tSTRAW, i, tCHOD, CHODID);
    if(!hasCHOD) continue;
    if(verb) cout<<"----has CHOD association----"<<endl;

    //newCHOD assoc - RG search through all hits associated to any track (SelectNewCHODCandidate), this is different
    if(verb){
      cout<<endl;
      cout<<"----NewCHOD association----"<<endl;
    };
    double tNewCHOD = -9999999.;
    int newCHODID = -1;
    TVector2 trackAtNewCHOD(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZNewCHOD()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZNewCHOD()));
    bool hasNewCHOD = HasNewCHODAssoc(i, trackAtNewCHOD, tSTRAW, tCHOD, tNewCHOD, newCHODID);
    if(!hasNewCHOD) continue;
    FillHisto("hTimeDiffBestNewCHODSTRAW", tNewCHOD-tSTRAW);
    FillHisto("hTimeDiffBestNewCHODTrigger", tNewCHOD-tTrigger);
    FillHisto("hTimeDiffBestNewCHODCHOD", tNewCHOD-tCHOD);
    if(verb) cout<<"----has newCHOD association----"<<endl;

    //RICH assoc
    if(verb){
      cout<<endl;
      cout<<"----RICH association----"<<endl;
    };
    double tRICH = -999999.;
    bool hasStandardRICH = false;
    bool hasSingleRICH = false;
    hasStandardRICH = SpecRICH[i].isValid();
    FillHisto("hValidAssocRICH", (int)hasStandardRICH);
    if(verb) cout<<"Has standard RICH association? "<<hasStandardRICH<<endl;
    FillHisto("hTimeDiffStandardRICHCHOD", SpecRICH[i].GetRingTime(3)-tCHOD);
    if(!hasStandardRICH) continue;
    SpectrometerRICHAssociationOutputSingleRing srAssoc = SpecRICHSingleRing.at(i);
    hasSingleRICH =  srAssoc.isAssociated();
    FillHisto("hIsAssocRICHSingleRing", hasSingleRICH);
    if(verb) cout<<"Has Single ring RICH association? "<<hasSingleRICH<<endl;
    if(!hasSingleRICH) continue;
    double srChi2 = srAssoc.GetRingChi2();
    if(verb) cout<<"ring chi2 = "<<srChi2<<" > 0.01"<<endl;
    if(srChi2<=0.01){
      hasSingleRICH = false;
      continue;
    };
    tRICH = srAssoc.GetRingTime();
    if(verb){
      cout<<"tRICH "<<tRICH<<endl;
      cout<<"tCHOD "<<tCHOD<<endl;
    };
    FillHisto("hTimeDiffBestAssocRICHCHOD", tRICH-tCHOD);
    if(verb) cout<<"time diff RICH CHOD: "<<fabs(tRICH-tCHOD)<<" < "<<fCutTimeDiffRICHCHOD<<endl;
    if(fabs(tRICH-tCHOD)>fCutTimeDiffRICHCHOD) continue;
    FillHisto("hTimeDiffBestRICHCHOD", tRICH-tCHOD);
    FillHisto("hTimeDiffBestRICHSTRAW", tRICH-tSTRAW);
    FillHisto("hTimeDiffBestRICHTrigger", tRICH-tTrigger);
    FillHisto("hTimeDiffBestRICHNewCHOD", tRICH-tNewCHOD);
    if(verb) cout<<"----has RICH association----"<<endl;

    //LKr assoc
    if(verb){
      cout<<endl;
      cout<<"----LKr association----"<<endl;
    };
    TVector2 trackAtLKr(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZLKr()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZLKr()));
    double tLKr = -9999.;
    double bestLKrEnergy = 0.;
    int nCells = 0;
    double seedEnergy = 0.;
    TVector2 clusterPos(0., 0.);
    int lkrID = -1;
    if(verb) cout<<"track at LKr "<<trackAtLKr.X()<<" "<<trackAtLKr.Y()<<endl;
    if(verb){
      cout<<"Standard association:"<<endl;
      cout<<"N LKr candidates "<<fLKrEvent->GetNCandidates()<<endl;
      cout<<endl;
    };
    double tLKrS = -9999.;
    double totalEnergyS = 0.;
    int nCellsS = 0;
    double seedEnergyS = 0.;
    TVector2 clusterPosS(0., 0.);
    int lkrIDS = -1;
    bool hasStandardLKr = HasLKrStandardAssoc(trackAtLKr, tSTRAW, tLKrS, totalEnergyS, seedEnergyS, nCellsS, clusterPosS, lkrIDS);
    FillHisto("hLKrMatch1Found", (hasStandardLKr ? 1 : 0));
    if(verb){
      cout<<"Has standard association? "<<hasStandardLKr<<endl;
      cout<<endl;
      cout<<"Cell association: "<<endl;
      cout<<"N LKr hits: "<<fLKrEvent->GetNHits()<<endl;
      cout<<endl;
    };
    double tLKrC = -9999.;
    double totalEnergyC = 0.;
    int nCellsC = 0;
    double seedEnergyC = 0.;
    TVector2 clusterPosC(0., 0.);
    bool hasChargedLKr = HasLKrCellAssoc(trackAtLKr, tSTRAW, tLKrC, totalEnergyC, seedEnergyC, nCellsC, clusterPosC);
    if(verb){
      cout<<"Has charged association? "<<hasChargedLKr<<endl;
      cout<<endl;
      cout<<"Calo association:"<<endl;
      cout<<endl;
    };
    CalorimeterCluster *clus = static_cast<CalorimeterCluster*>(fSpecCalo->ConstructedAt(i));
    bool hasCalo = false;
    if(clus->IsLKrAssociated()) hasCalo = true;
    if(verb){
      cout<<"Has calo association? "<<hasCalo<<endl;
      cout<<endl;
    };

    if(verb) cout<<endl;
    if(verb) cout<<"standard "<<hasStandardLKr<<" charged "<<hasChargedLKr<<" calo "<<hasCalo<<endl;
    if(!hasChargedLKr || !hasCalo) continue;
    if(hasStandardLKr){
      lkrID = lkrIDS;
      tLKr = tLKrS;
      seedEnergy = seedEnergyS;
      nCells = nCellsS;
      bestLKrEnergy = totalEnergyS;
      clusterPos = clusterPosS;
    }else if(hasChargedLKr){
      tLKr = tLKrC;
      seedEnergy = seedEnergyC;
      nCells = nCellsC;
      bestLKrEnergy = totalEnergyC;
      clusterPos = clusterPosC;
    };
    if(verb){
      cout<<"Test selected cluster:"<<endl;
      cout<<"cluster distance from track at LKr = "<<(clusterPos-trackAtLKr).Mod()<<" <= "<<fCutMaxDistLKr<<endl;
      cout<<"time diff LKr - STRAW = "<<fabs(tLKr - tSTRAW)<<" <= "<<fCutTimeDiffLKrSTRAW<<endl;
      cout<<"time diff LKr - CHOD = "<<fabs(tLKr - tCHOD)<<" <= "<<fCutTimeDiffLKrCHOD<<endl;
    };

    if(((clusterPos-trackAtLKr).Mod()>fCutMaxDistLKr) || (fabs(tLKr - tSTRAW)>fCutTimeDiffLKrSTRAW) || (fabs(tLKr - tCHOD)>fCutTimeDiffLKrCHOD) || (bestLKrEnergy<=0.)) continue;
    if(!hasChargedLKr && !hasStandardLKr) continue;
    if(verb){
      cout<<"LKr ID = "<<lkrID<<endl;
      cout<<"best time = "<<tLKr<<endl;
      cout<<"seed energy = "<<seedEnergy<<endl;
      cout<<"n cells = "<<nCells<<endl;
      cout<<"best energy = "<<bestLKrEnergy<<endl;
      cout<<"cluster position = "<<clusterPos.X()<<" "<<clusterPos.Y()<<endl;
    };
    FillHisto("hLKrMatchFound", hasStandardLKr ? 1 : 2);
    FillHisto("hTimeDiffBestLKrSTRAW", tLKr-tSTRAW);
    FillHisto("hTimeDiffBestLKrCHOD", tLKr-tCHOD);
    FillHisto("hTimeDiffBestLKrNewCHOD", tLKr-tNewCHOD);
    FillHisto("hTimeDiffBestLKrRICH", tLKr-tRICH);
    FillHisto("hTimeDiffBestLKrTrigger", tLKr-tTrigger);
    FillHisto("hEoverPvsTimeDiffLKrCHOD", tLKr-tCHOD, bestLKrEnergy/STRAWCand->GetMomentum());
    if(verb) cout<<"----has LKr association of type "<<(hasStandardLKr ? 1 : 2)<<"----"<<endl;

    //calculate track time
    if(verb){
      cout<<endl;
      cout<<"----Downstream time----"<<endl;
    };
    FillHisto("hTimeCHOD", tCHOD);
    FillHisto("hTimeLKr", tLKr);
    FillHisto("hTimeRICH", tRICH);
    FillHisto("hTimeSTRAW", tSTRAW);
    FillHisto("hTimeCHODWeighted", tCHOD/fWeightCHOD);
    FillHisto("hTimeLKrWeighted", tLKr/fWeightLKr);
    FillHisto("hTimeRICHWeighted", tRICH/fWeightRICH);
    FillHisto("hTimeSTRAWWeighted", tSTRAW/fWeightSTRAW);
    tDownstream = ((tCHOD/fWeightCHOD) + (tLKr/fWeightLKr) + (tRICH/fWeightRICH) + (tSTRAW/fWeightSTRAW))/((1./fWeightCHOD) + (1./fWeightLKr) + (1./fWeightRICH) + (1./fWeightSTRAW));
    FillHisto("hTrackTime", tDownstream);
    if(verb){
      cout<<"TRICH = "<<tRICH<<endl;
      cout<<"TLKr = "<<tLKr<<endl;
      cout<<"TCHOD = "<<tCHOD<<endl;
      cout<<"TSTRAW = "<<tSTRAW<<endl;
      cout<<"Ttrigger = "<<tTrigger<<endl;
      cout<<"----Tdownstream = "<<tDownstream<<" ----"<<endl;
    };

    //KTAG matching track
    if(verb){
      cout<<endl;
      cout<<"----KTAG association----"<<endl;
    };
    double minDT = 1000.;
    int bestKtag = -1;
    if(verb) cout<<"N KTAG candidates "<<CedarEvent->GetNCandidates()<<endl;
    if(verb) cout<<endl;
    for(int m=0; m<CedarEvent->GetNCandidates(); m++){
      if(verb) cout<<"candidate "<<m<<endl;
      CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(m));
      FillHisto("hTimeDiffKTAGDownstream", CedarCand->GetTime() - tDownstream);
      if(verb){
	cout<<"tKTAG = "<<CedarCand->GetTime()<<endl;
	cout<<"Is best KTAG? tKTAG - tDownstream = "<<fabs(CedarCand->GetTime()-tDownstream)<<" < "<<minDT<<endl;
      };
      if(fabs(CedarCand->GetTime()-tDownstream)<minDT){
	minDT = fabs(CedarCand->GetTime()-tDownstream);
	bestKtag = m;
	if(verb) cout<<"current best KTAG "<<bestKtag<<endl;
      };
      if(verb) cout<<endl;
    };
    if(bestKtag==-1){
      if(verb) cout<<"KTAG match not found."<<endl;
      FillHisto("hKTAGMatchFound", 0);
      continue;
    };
    CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(bestKtag));
    double tKTAG = CedarCand->GetTime();
    FillHisto("hKTAGNsectors", CedarCand->GetNSectors());
    FillHisto("hTimeDiffGoodKTAGDownstream", tKTAG - tDownstream);
    if(verb){
      if(!GetWithMC()) cout<<"tKTAG - tDownstream = "<<fabs(tKTAG - tDownstream)<<" < "<<fCutTimeDiffCedarDownstreamData<<endl;
      if(GetWithMC()) cout<<"tKTAG - tDownstream = "<<fabs(tKTAG - tDownstream)<<" < "<<fCutTimeDiffCedarDownstreamMC<<endl;
      cout<<"best KTAG N sectors "<<CedarCand->GetNSectors()<<" > "<<fCutMinNSectorsKTAG<<endl;
    };
    if((!GetWithMC() && fabs(tKTAG - tDownstream)>fCutTimeDiffCedarDownstreamData) || (GetWithMC() && fabs(tKTAG - tDownstream)>fCutTimeDiffCedarDownstreamMC) || (CedarCand->GetNSectors()<fCutMinNSectorsKTAG)){
      if(verb) cout<<"KTAG match not found (too few sectors or too different time)."<<endl;
      FillHisto("hKTAGMatchFound", 0);
      continue;
    };
    FillHisto("hKTAGMatchFound", 1);
    if(verb) cout<<"----has KTAG association----"<<endl;
    FillHisto("hTimeDiffBestKTAGDownstream", tKTAG - tDownstream);
    FillHisto("hTimeDiffBestKTAGCHOD", tKTAG - tCHOD);
    FillHisto("hTimeDiffBestKTAGRICH", tKTAG - tRICH);
    FillHisto("hTimeDiffBestKTAGLKr", tKTAG - tLKr);
    FillHisto("hTimeDiffBestKTAGSTRAW", tKTAG - tSTRAW);
    FillHisto("hTimeDiffBestKTAGCHODWeighted", tKTAG - tCHOD/fWeightCHOD);
    FillHisto("hTimeDiffBestKTAGRICHWeighted", tKTAG - tRICH/fWeightRICH);
    FillHisto("hTimeDiffBestKTAGLKrWeighted", tKTAG - tLKr/fWeightLKr);
    FillHisto("hTimeDiffBestKTAGSTRAWWeighted", tKTAG - tSTRAW/fWeightSTRAW);

    //STRAW-kaon matching
    if(verb){
      cout<<endl;
      cout<<"----STRAW-GTK association----"<<endl;
    };
    std::vector<int> matchedGTKIDs;
    std::vector<double> matchedGTKTimes;
    std::vector<double> matchedGTKQuality1;
    std::vector<double> matchedGTKQuality2;
    std::vector<TVector3> matchedGTKVertices;
    std::vector<TVector3> matchedGTKMomenta;
    std::vector<TVector3> matchedGTKTrackMomenta;
    std::vector<TVector3> matchedGTKPositions;
    std::vector<TVector3> matchedGTKTrackPositions;
    TVector3 simpleVert;
    TVector3 nomKaonMomAtNomVertex(0., 0., 0.);
    TVector3 nomKaonPosAtNomVertex(0., 0., 0.);
    TVector3 trackMomAtNomVertex(0., 0., 0.);
    TVector3 trackPosAtNomVertex(0., 0., 0.);
    TVector3 nomVertex(0., 0., 0.);
    bool goodvertex = false;
    if(verb) cout<<"Use GTK? "<<UseGTK<<endl;
    if(UseGTK){
      //GTK kaon
      if(verb) cout<<"Prepare GTK candidates with ref time = "<<tKTAG<<endl;
      fGTKReco->Process(GTKEvent, tKTAG);
      if(verb) cout<<"Prepared."<<endl;
      if(verb) cout<<"Start matching procedure"<<endl;
      if(verb) cout<<"Using RG matching procedure"<<endl;
      fMatchingRG->Process(GTKEvent, STRAWCand, tKTAG, tKTAG, tRICH, verb, 1, "");
      if(verb) cout<<"Matching procedure RG finished"<<endl;
      matchedGTKIDs = fMatchingRG->GetMatchedGTKIDs();
      matchedGTKTimes = fMatchingRG->GetGTKTimes();
      matchedGTKQuality1 = fMatchingRG->GetMatchingQuality1();
      matchedGTKQuality2 = fMatchingRG->GetMatchingQuality2();
      matchedGTKVertices = fMatchingRG->GetVertices();
      matchedGTKMomenta = fMatchingRG->GetGTKMomentaAtVertices();
      matchedGTKTrackMomenta = fMatchingRG->GetTrackMomentaAtVertices();
      matchedGTKPositions = fMatchingRG->GetGTKPositionsAtVertices();
      matchedGTKTrackPositions = fMatchingRG->GetTrackPositionsAtVertices();

      FillHisto("hGTKMatchFound", (matchedGTKIDs.at(0)==-1 ? 0 : 1));
      if(verb) cout<<"found matching GTK for this track? "<<(matchedGTKIDs.at(0)==-1 ? 0 : 1)<<endl;
      if(verb) cout<<"Matched GTK ID "<<matchedGTKIDs.at(0)<<endl;
      if(matchedGTKIDs.at(0)==-1) continue;
      if(verb) cout<<"----has GTK association----"<<endl;
      if(matchedGTKIDs.size()>1){
	FillHisto("hDiscriminant1DiffBestTwo", fabs(matchedGTKQuality1.at(1) - matchedGTKQuality1.at(0)));
      };
      FillHisto("hMatchedGTKQuality1", matchedGTKQuality1.at(0));
      FillHisto("hTimeDiffMatchedGTKRICHvsMatchedGTKQuality2", matchedGTKQuality2.at(0), matchedGTKTimes.at(0) - tRICH);
      FillHisto("hTimeDiffMatchedGTKKTAGvsMatchedGTKQuality1", matchedGTKQuality1.at(0), matchedGTKTimes.at(0) - CedarCand->GetTime());
      FillHisto("hTrackGTKDistAtVertexVsMatchedGTKQuality1", matchedGTKQuality1.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Mag());
      FillHisto("hTrackGTKDistXAtVertexVsMatchedGTKQuality1", matchedGTKQuality1.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).X());
      FillHisto("hTrackGTKDistYAtVertexVsMatchedGTKQuality1", matchedGTKQuality1.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Y());
      FillHisto("hTrackGTKDistAtVertexVsMatchedGTKQuality2", matchedGTKQuality2.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Mag());
      FillHisto("hTrackGTKDistXAtVertexVsMatchedGTKQuality2", matchedGTKQuality2.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).X());
      FillHisto("hTrackGTKDistYAtVertexVsMatchedGTKQuality2", matchedGTKQuality2.at(0), (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Y());
      FillHisto("hTimeDiffMatchedGTKRICH", matchedGTKTimes.at(0) - tRICH);
      FillHisto("hTimeDiffMatchedGTKKTAG", matchedGTKTimes.at(0) - tKTAG);
      FillHisto("hTrackGTKDistAtVertex", (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Mag());
      FillHisto("hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH", matchedGTKTimes.at(0) - tRICH, (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Mag());
      FillHisto("hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG", matchedGTKTimes.at(0) - tKTAG, (matchedGTKPositions.at(0) - matchedGTKTrackPositions.at(0)).Mag());
    };

    //NOMINAL kaon
    if(verb){
      cout<<endl;
      cout<<"----Nominal kaon association----"<<endl;
    };
    TVector3 nomKaonMom = BeamParameters::GetInstance()->GetBeamThreeMomentum();
    TVector3 nomKaonPos(0., 0., GeometricAcceptance::GetInstance()->GetZGTK3());
    TVector3 trackMom = STRAWCand->GetThreeMomentumBeforeMagnet();
    TVector3 trackPos = STRAWCand->GetPositionBeforeMagnet();
    nomVertex = GetIterativeVertex(STRAWCand->GetCharge(), trackMom, trackPos, 1, nomKaonMom, nomKaonPos, &nomKaonMomAtNomVertex, &nomKaonPosAtNomVertex, &trackMomAtNomVertex, &trackPosAtNomVertex);
    if(nomVertex.Mag()>0.) goodvertex = true;
    if(verb){
      cout<<"Found vertex with nominal kaon? "<<goodvertex<<endl;
      cout<<"----has nominal kaon association----"<<endl;
    };
    if(!UseGTK && !goodvertex){
      if(verb) cout<<"Do not want to use GTK but no good nominal vertex found."<<endl;
      continue;
    };

    //output
    fBestTracks.at(i) = true;
    fBestTrackTime.at(i) = tDownstream;
    fBestTrackSTRAWTime.at(i) = tSTRAW;
    fBestTrackCHODTime.at(i) = tCHOD;
    fCHODAssocID.at(i) = CHODID;
    fNewCHODAssocID.at(i) = newCHODID;
    fBestTrackNewCHODTime.at(i) = tNewCHOD;
    fRICHStandardAssoc.at(i) =  hasStandardRICH;
    fRICHSingleAssoc.at(i) =  hasSingleRICH;
    fBestTrackRICHTime.at(i) = tRICH;
    fLKrStandardAssoc.at(i) = hasStandardLKr;
    fLKrChargedAssoc.at(i) = hasChargedLKr;
    fBestTrackLKrTime.at(i) = tLKr;
    fLKrAssocID.at(i) = lkrID;
    fLKrAssocEnergy.at(i) = bestLKrEnergy;
    fLKrAssocSeedEnergy.at(i) = seedEnergy;
    fLKrAssocNCells.at(i) = nCells;
    fLKrAssocPosition.at(i) = clusterPos;
    fBestTrackKTAGTime.at(i) = tKTAG;
    if(UseGTK) fGTKAssocID.at(i) = matchedGTKIDs.at(0);
    if(UseGTK) fBestTrackGTKTime.at(i) = matchedGTKTimes.at(0);
    if(UseGTK) fGTKAssocVertex.at(i) = matchedGTKVertices.at(0);
    if(UseGTK) fGTKAllAssocVertices.at(i) = matchedGTKVertices;
    if(UseGTK) fGTKAssocQuality1.at(i) = matchedGTKQuality1.at(0);
    if(UseGTK) fGTKAssocQuality2.at(i) = matchedGTKQuality2.at(0);
    if(UseGTK) fGTKAssocMomentum.at(i) = matchedGTKMomenta.at(0); //at vertex
    if(UseGTK) fGTKAssocTrackMomentum.at(i) = matchedGTKTrackMomenta.at(0); //at vertex
    if(UseGTK) fGTKNAssocs.at(i) = matchedGTKIDs.size();
    fNomVertex.at(i) = nomVertex;
    fBestTrackNomMomentum.at(i) = trackMomAtNomVertex;    //at vertex
    fNomKaonMomentum.at(i) = nomKaonMomAtNomVertex;   //at vertex
    if(verb) cout<<endl;
    if(verb) cout<<"+++++ Saving best track = "<<i<<" +++++"<<endl;
    if(verb) cout<<endl;
    FillHisto("hNSTRAWChambers_bestTracks", STRAWCand->GetNChambers());
  };
  FillHisto("hCut", cutID);
  FillHisto("hNBestTracks", std::count(fBestTracks.begin(), fBestTracks.end(), true));

  if(verb){
    cout<<endl;
    cout<<"+++++ N best tracks = "<<std::count(fBestTracks.begin(), fBestTracks.end(), true)<<" +++++"<<endl;
  };

  ValidateOutputs();
}

void BestTrackSelection::PostProcess(){
}

void BestTrackSelection::EndOfBurstUser(){
}

void BestTrackSelection::EndOfRunUser(){}

void BestTrackSelection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
    fMatchingRG->SaveAllPlots();
  };
}

void BestTrackSelection::DrawPlot(){
}

BestTrackSelection::~BestTrackSelection(){
  delete fGTKReco;

  fGTKReco=nullptr;
}

void BestTrackSelection::PrepareOutputs(){
  fBestTracks.clear();
  fBestTrackTime.clear();
  fBestTrackSTRAWTime.clear();
  fBestTrackCHODTime.clear();
  fBestTrackNewCHODTime.clear();
  fCHODAssocID.clear();
  fNewCHODAssocID.clear();
  fRICHStandardAssoc.clear();
  fRICHSingleAssoc.clear();
  fBestTrackRICHTime.clear();
  fLKrStandardAssoc.clear();
  fLKrChargedAssoc.clear();
  fBestTrackLKrTime.clear();
  fLKrAssocID.clear();
  fLKrAssocEnergy.clear();
  fLKrAssocSeedEnergy.clear();
  fLKrAssocNCells.clear();
  fLKrAssocPosition.clear();
  fBestTrackKTAGTime.clear();
  fGTKAssocID.clear();
  fBestTrackGTKTime.clear();
  fGTKAssocVertex.clear();
  fGTKAllAssocVertices.clear();
  fGTKAssocQuality1.clear();
  fGTKAssocQuality2.clear();
  fGTKAssocMomentum.clear();
  fGTKAssocTrackMomentum.clear();
  fGTKNAssocs.clear();
  fNomVertex.clear();
  fBestTrackNomMomentum.clear();
  fNomKaonMomentum.clear();
  SetOutputState("BestTracks", kOInvalid);
  SetOutputState("BestTrackTime", kOInvalid);
  SetOutputState("BestTrackSTRAWTime", kOInvalid);
  SetOutputState("BestTrackCHODTime", kOInvalid);
  SetOutputState("BestTrackNewCHODTime", kOInvalid);
  SetOutputState("CHODAssocID", kOInvalid);
  SetOutputState("NewCHODAssocID", kOInvalid);
  SetOutputState("RICHStandardAssoc", kOInvalid);
  SetOutputState("RICHSingleAssoc", kOInvalid);
  SetOutputState("BestTrackRICHTime", kOInvalid);
  SetOutputState("LKrStandardAssoc", kOInvalid);
  SetOutputState("LKrChargedAssoc", kOInvalid);
  SetOutputState("BestTrackLKrTime", kOInvalid);
  SetOutputState("LKrAssocID", kOInvalid);
  SetOutputState("LKrAssocEnergy", kOInvalid);
  SetOutputState("LKrAssocSeedEnergy", kOInvalid);
  SetOutputState("LKrAssocNCells", kOInvalid);
  SetOutputState("LKrAssocPosition", kOInvalid);
  SetOutputState("BestTrackKTAGTime", kOInvalid);
  SetOutputState("GTKAssocID", kOInvalid);
  SetOutputState("BestTrackGTKTime", kOInvalid);
  SetOutputState("GTKAssocVertex", kOInvalid);
  SetOutputState("GTKAllAssocVertices", kOInvalid);
  SetOutputState("GTKAssocQuality1", kOInvalid);
  SetOutputState("GTKAssocQuality2", kOInvalid);
  SetOutputState("GTKAssocMomentum", kOInvalid);
  SetOutputState("GTKAssocTrackMomentum", kOInvalid);
  SetOutputState("GTKNAssocs", kOInvalid);
  SetOutputState("NomVertex", kOInvalid);
  SetOutputState("BestTrackNomMomentum", kOInvalid);
  SetOutputState("NomKaonMomentum", kOInvalid);
}

void BestTrackSelection::ValidateOutputs(){
  SetOutputState("BestTracks", kOValid);
  SetOutputState("BestTrackTime", kOValid);
  SetOutputState("BestTrackSTRAWTime", kOValid);
  SetOutputState("BestTrackCHODTime", kOValid);
  SetOutputState("CHODAssocID", kOValid);
  SetOutputState("NewCHODAssocID", kOValid);
  SetOutputState("BestTrackNewCHODTime", kOValid);
  SetOutputState("RICHStandardAssoc", kOValid);
  SetOutputState("RICHSingleAssoc", kOValid);
  SetOutputState("BestTrackRICHTime", kOValid);
  SetOutputState("LKrStandardAssoc", kOValid);
  SetOutputState("LKrChargedAssoc", kOValid);
  SetOutputState("BestTrackLKrTime", kOValid);
  SetOutputState("LKrAssocID", kOValid);
  SetOutputState("LKrAssocEnergy", kOValid);
  SetOutputState("LKrAssocSeedEnergy", kOValid);
  SetOutputState("LKrAssocNCells", kOValid);
  SetOutputState("LKrAssocPosition", kOValid);
  SetOutputState("BestTrackKTAGTime", kOValid);
  SetOutputState("GTKAssocID", kOValid);
  SetOutputState("BestTrackGTKTime", kOValid);
  SetOutputState("GTKAssocVertex", kOValid);
  SetOutputState("GTKAllAssocVertices", kOValid);
  SetOutputState("GTKAssocQuality1", kOValid);
  SetOutputState("GTKAssocQuality2", kOValid);
  SetOutputState("GTKAssocMomentum", kOValid);
  SetOutputState("GTKAssocTrackMomentum", kOValid);
  SetOutputState("GTKNAssocs", kOValid);
  SetOutputState("NomVertex", kOValid);
  SetOutputState("BestTrackNomMomentum", kOValid);
  SetOutputState("NomKaonMomentum", kOValid);
}

void BestTrackSelection::RestoreDefaultOutputs(){
    TVector3 v(0., 0., 0.);
    std::vector<TVector3> vv;
    vv.push_back(v);
    fBestTracks.push_back(false);
    fBestTrackTime.push_back(-9999.);
    fBestTrackSTRAWTime.push_back(-9999.);
    fBestTrackCHODTime.push_back(-9999.);
    fBestTrackNewCHODTime.push_back(-9999.);
    fCHODAssocID.push_back(-1);
    fNewCHODAssocID.push_back(-1);
    fRICHStandardAssoc.push_back(false);
    fRICHSingleAssoc.push_back(false);
    fBestTrackRICHTime.push_back(-9999.);
    fLKrStandardAssoc.push_back(false);
    fLKrChargedAssoc.push_back(false);
    fBestTrackLKrTime.push_back(-9999.);
    fLKrAssocID.push_back(-1);
    fLKrAssocEnergy.push_back(0.);
    fLKrAssocSeedEnergy.push_back(0.);
    fLKrAssocNCells.push_back(0);
    fLKrAssocPosition.push_back(v.XYvector());
    fBestTrackKTAGTime.push_back(-9999.);
    fGTKAssocID.push_back(-1);
    fBestTrackGTKTime.push_back(-9999.);
    fGTKAssocVertex.push_back(v);
    fGTKAllAssocVertices.push_back(vv);
    fGTKAssocQuality1.push_back(9999999.);
    fGTKAssocQuality2.push_back(9999999.);
    fGTKAssocMomentum.push_back(v);
    fGTKAssocTrackMomentum.push_back(v);
    fGTKNAssocs.push_back(0);
    fNomVertex.push_back(v);
    fBestTrackNomMomentum.push_back(v);
    fNomKaonMomentum.push_back(v);
}

std::pair<bool, bool> BestTrackSelection::is_multi_track(int STRAWCandID, std::vector<bool> &fakeTracks){
  std::pair<bool, bool> isMultiTrack;
  bool goodCDA = false;
  bool inTime = false;
  bool isMultiFull = false;
  bool isMultiBroad = false;
  TRecoSpectrometerCandidate* STRAWCand1 = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(STRAWCandID));
  TRecoSpectrometerCandidate* STRAWCand2;
  for(int j=0; j<fSTRAWEvent->GetNCandidates(); j++){
    if(j==STRAWCandID){
      if(verb) cout<<"This is my candidate, skip."<<endl;
      continue;
    };
    if(fakeTracks.at(j)){
      if(verb) cout<<"This is fake track, skip."<<endl;
      continue;
    };
    STRAWCand2 = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(j));
    if(verb) cout<<"test candidates "<<STRAWCandID<<" and "<<j<<endl;
    if(verb) cout<<"candidate "<<STRAWCandID<<" starting at position "<<STRAWCand1->GetPositionBeforeMagnet().X()<<" "<<STRAWCand1->GetPositionBeforeMagnet().Y()<<" "<<STRAWCand1->GetPositionBeforeMagnet().Z()<<endl;
    if(verb) cout<<"candidate "<<j<<" starting at position "<<STRAWCand2->GetPositionBeforeMagnet().X()<<" "<<STRAWCand2->GetPositionBeforeMagnet().Y()<<" "<<STRAWCand2->GetPositionBeforeMagnet().Z()<<endl;
    double CDA = GetCDA(STRAWCand1, STRAWCand2);
    if(verb) cout<<"CDA = "<<CDA<<" < "<<fCutBroadMultitrackCDA<<" to be broad multitrack"<<endl;
    if(CDA<fCutBroadMultitrackCDA) isMultiBroad = true;
    TVector3 vertex = GetVertexCDA(STRAWCand1->GetThreeMomentumBeforeMagnet(), STRAWCand1->GetPositionBeforeMagnet(), STRAWCand2->GetThreeMomentumBeforeMagnet(), STRAWCand2->GetPositionBeforeMagnet());
    if(verb) cout<<"vertex found at "<<vertex.X()<<" "<<vertex.Y()<<" "<<vertex.Z()<<endl;
    double vertexZ = vertex.Z();
    if(verb) cout<<"CDA = "<<CDA<<" < "<<fCutFullMultitrackCDA<<" to be full multitrack"<<endl;
    if(verb) cout<<"vertex Z = "<<vertexZ<<" > "<<fCutMultitrackMinZ<<" to be full multitrack"<<endl;
    if(verb) cout<<"vertex Z = "<<vertexZ<<" < "<<fCutMultitrackMaxZ<<" to be full multitrack"<<endl;
    if(verb) cout<<"candidates deltaT  = "<<fabs(STRAWCand1->GetTime() - STRAWCand2->GetTime())<<" < "<<fCutMultitrackTimeDiff<<" to be full multitrack"<<endl;
    if((CDA<fCutFullMultitrackCDA) && (vertexZ>fCutMultitrackMinZ) && (vertexZ<fCutMultitrackMaxZ)) goodCDA = true;
    if(fabs(STRAWCand1->GetTime() - STRAWCand2->GetTime())<fCutMultitrackTimeDiff) inTime = true;
    if(goodCDA && inTime) isMultiFull = true;
  };
  isMultiTrack = make_pair(isMultiFull, isMultiBroad);
  return isMultiTrack;
}

bool BestTrackSelection::AcceptanceOK(TRecoSpectrometerCandidate *STRAWCand){
  FillHisto("hAtSTRAW1YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZStraw(0)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZStraw(0)));
  FillHisto("hAtSTRAW2YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZStraw(1)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZStraw(1)));
  FillHisto("hAtSTRAW3YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZStraw(2)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZStraw(2)));
  FillHisto("hAtSTRAW4YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZStraw(3)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZStraw(3)));
  bool inSTRAW[4] = {false, false, false, false};
  for(int k=0; k<4; k++){
    inSTRAW[k] = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kSpectrometer, k, 75., 1000.);
    if(verb) cout<<"Track in STRAW "<<k<<" acceptance? "<<inSTRAW[k]<<endl;
  };

  FillHisto("hAtRICHFrontPlaneYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZRICHFrontPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZRICHFrontPlane()));
  FillHisto("hAtRICHBackPlaneYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZRICHBackPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZRICHBackPlane()));
  bool inRICH = false;
  // TVector2 posRICHf(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZRICHFrontPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZRICHFrontPlane()));
  // TVector2 posRICHb(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZRICHBackPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZRICHBackPlane()));
  // double distF = sqrt(pow((posRICHf.X()-34.), 2) + pow((posRICHf.Y()-0.), 2));
  // double distB = sqrt(pow((posRICHb.X()-2.), 2) + pow((posRICHb.Y()-0.), 2));
  // if(distF>101. && distB>101. && distF<1100. && distB<1100.) inRICH = true;
  inRICH = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kRICH, 0, 101., -1.);
  if(verb) cout<<"Track in RICH acceptance? "<<inRICH<<endl;

  FillHisto("hAtNewCHODYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZNewCHOD()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZNewCHOD()));
  bool inNewCHOD = false;
  inNewCHOD = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kNewCHOD, 0, 140., 1070.);
  if(verb) cout<<"Track in NewCHOD acceptance? "<<inNewCHOD<<endl;

  FillHisto("hAtHPlaneCHODYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZCHODHPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZCHODHPlane()));
  FillHisto("hAtVPlaneCHODYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZCHODVPlane()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZCHODVPlane()));
  bool inCHOD = false;
  inCHOD = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kCHOD, 0, 125., 1100.);
  if(verb) cout<<"Track in CHOD acceptance? "<<inCHOD<<endl;

  FillHisto("hAtLKrYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZLKr()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZLKr()));
  bool inLKr = false;
  inLKr = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kLKr, 0, 150., 1130.); //1070.
  if(verb) cout<<"Track in LKr acceptance? "<<inLKr<<endl;

  FillHisto("hAtMUV1YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZMUV1()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZMUV1()));
  FillHisto("hAtMUV2YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZMUV2()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZMUV2()));
  bool inMUV[2] = {false, false};
  inMUV[0] = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kMUV1, 0, 130., 1100.);
  inMUV[1] = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kMUV2, 0, 130., 1100.);
  if(verb) cout<<"Track in MUV1 acceptance? "<<inMUV[0]<<endl;
  if(verb) cout<<"Track in MUV2 acceptance? "<<inMUV[1]<<endl;

  FillHisto("hAtMUV3YvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZMUV3()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZMUV3()));
  bool inMUV3 = false;
  inMUV3 = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kMUV3, 0, 130., 1200.);
  if(verb) cout<<"Track in MUV3 acceptance? "<<inMUV3<<endl;

  FillHisto("hAtLAV12FrontYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZLAVFront(11)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZLAVFront(11)));
  FillHisto("hAtLAV12BackYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZLAVBack(11)), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZLAVBack(11)));
  bool inLAV12 = false;
  inLAV12 = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kLAV, 11, -1., -1.);
  if(verb) cout<<"Track in LAV12 acceptance? "<<inLAV12<<endl;

  FillHisto("hAtIRCYvsX", STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZIRC()), STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZIRC()));
  bool inIRC = false;
  inIRC = GeometricAcceptance::GetInstance()->InAcceptance(STRAWCand, kIRC, 0, -1., -1.);
  if(verb) cout<<"Track in IRC acceptance? "<<inIRC<<endl;

  bool acceptanceOK = false;
  if(inSTRAW[0] && inSTRAW[1] && inSTRAW[2] && inSTRAW[3] && inRICH && inNewCHOD && inCHOD && inLKr && inMUV[0] && inMUV[1] && inMUV3 && inLAV12 && !inIRC) acceptanceOK = true;

  return acceptanceOK;
}

bool BestTrackSelection::HasCHODAssoc(double tSTRAW, int trackID, double &tCHOD, int &chodID){
  bool hasAssoc = false;
  FillHisto("hNAssocRecCHOD", fSpecCHOD[trackID].GetNAssociationRecords());
  if(verb) cout<<"CHODAssocRecords: "<<fSpecCHOD[trackID].GetNAssociationRecords()<<" > "<<0<<endl;
  if(fSpecCHOD[trackID].GetNAssociationRecords()==0) return hasAssoc;
  double timeCHOD = fSpecCHOD[trackID].GetBestAssociationRecord()->GetCHODCandidateTime();
  FillHisto("hTimeDiffBestAssocCHODSTRAW", timeCHOD-tSTRAW);
  if(verb) cout<<"Time diff STRAW CHOD: "<<fabs(timeCHOD-tSTRAW)<<" < "<<fCutTimeDiffSTRAWCHOD<<endl;
  if(fabs(timeCHOD-tSTRAW)>fCutTimeDiffSTRAWCHOD) return hasAssoc;
  double Dchod = pow((fSpecCHOD[trackID].GetBestAssociationRecord()->GetTrackCandidateDistance())/(2.*fCHODAssocSigmaX), 2) + pow((timeCHOD - tSTRAW)/(3.*fCHODAssocSigmaT), 2);
  FillHisto("hDchod", Dchod);
  FillHisto("hDistanceTrackCHODAssociation", fSpecCHOD[trackID].GetBestAssociationRecord()->GetTrackCandidateDistance());
  if(verb) cout<<"D CHOD: "<<Dchod<<" < "<<fCutDCHOD<<endl;
  if(Dchod>=fCutDCHOD) return hasAssoc;
  FillHisto("hTimeDiffBestAssocCHODTrigger", timeCHOD-tTrigger);
  if(verb) cout<<"Time diff Trigger CHOD: "<<fabs(timeCHOD-tTrigger)<<" < "<<fCutTimeDiffTriggerCHOD<<endl;
  if(fabs(timeCHOD-tTrigger)>fCutTimeDiffTriggerCHOD) return hasAssoc;
  int bestCHOD = fSpecCHOD[trackID].GetBestAssociationRecord()->GetCHODCandidateID();
  FillHisto("hTimeDiffBestCHODTrigger", timeCHOD - tTrigger);
  FillHisto("hTimeDiffBestCHODSTRAW", timeCHOD - tSTRAW);
  if(bestCHOD>=0) hasAssoc = true;
  chodID = bestCHOD;
  tCHOD = timeCHOD;
  return hasAssoc;
}

bool BestTrackSelection::HasNewCHODAssoc(int i, TVector2 trackAtNewCHOD, double tSTRAW, double tCHOD, double &tNewCHOD, int &newCHODID){
  FillHisto("hNAssocRecNewCHOD", fSpecNewCHOD[i].GetNAssociationRecords());
  if(verb) cout<<"NewCHODAssocRecords: "<<fSpecNewCHOD[i].GetNAssociationRecords()<<" > "<<0<<endl;
  if(fSpecNewCHOD[i].GetNAssociationRecords()==0) return false;
  if(verb){
    for(int m=0; m<fSpecNewCHOD[i].GetNAssociationRecords(); m++){
      cout<<"record ID "<<m<<endl;
      cout<<"time of record = "<<fSpecNewCHOD[i].GetAssociationRecord(m)->GetRecoHitTime()<<endl;
      cout<<"position of record = "<<fSpecNewCHOD[i].GetAssociationRecord(m)->GetRecoHitXY().X()<<" "<<fSpecNewCHOD[i].GetAssociationRecord(m)->GetRecoHitXY().Y()<<endl;
      cout<<"distance from track = "<<(trackAtNewCHOD-fSpecNewCHOD[i].GetAssociationRecord(m)->GetRecoHitXY()).Mod()<<endl;
    };
    cout<<endl;
  };
  TVector2 posNewCHOD = fSpecNewCHOD[i].GetBestAssociationRecord()->GetRecoHitXY();
  if(verb) cout<<"pos new CHOD "<<posNewCHOD.X()<<" "<<posNewCHOD.Y()<<endl;
  if(verb) cout<<"track at newCHOD "<<trackAtNewCHOD.X()<<" "<<trackAtNewCHOD.Y()<<endl;
  double tnch = fSpecNewCHOD[i].GetBestAssociationRecord()->GetRecoHitTime();
  if(verb) cout<<"tNewCHOD "<<tnch<<endl;
  double D = pow(((posNewCHOD-trackAtNewCHOD).Mod())/(fSigmaPosNewCHOD), 2) + pow((tnch-tSTRAW)/(fSigmaTimeNewCHOD), 2);
  FillHisto("hPosDiffAtNewCHOD", (posNewCHOD-trackAtNewCHOD).Mod());
  FillHisto("hPosDiffAtNewCHODYvsX", (posNewCHOD-trackAtNewCHOD).X(), (posNewCHOD-trackAtNewCHOD).Y());
  FillHisto("hTimeDiffNewCHODSTRAW", tnch-tSTRAW);
  FillHisto("hDiscriminantNewCHOD", D);
  if(verb) cout<<"D NewCHOD: "<<D<<" < "<<fCutDiscriminantNewCHOD<<endl;
  if(D>=fCutDiscriminantNewCHOD) return false;
  if(verb) cout<<"Time diff NewCHOD CHOD: "<<fabs(tnch-tCHOD)<<" < "<<fCutTimeDiffCHODNewCHOD<<endl;
  if(fabs(tnch-tCHOD)>fCutTimeDiffCHODNewCHOD) return false;
  tNewCHOD = tnch;
  newCHODID = fSpecNewCHOD[i].GetBestAssociationRecordID();
  return true;
}

bool BestTrackSelection::HasLKrStandardAssoc(TVector2 trackAtLKr, double tSTRAW, double &tLKr, double &bestLKrEnergy, double &seedEnergy, int &nCells, TVector2 &clusterPos, int &lkrID){
  bool hasAssoc = false;
  int bestID = -1;
  TRecoLKrCandidate *LKrCand;
  double minD = 999999;
  for(int k=0; k<fLKrEvent->GetNCandidates(); k++){
    if(verb) cout<<"candidate "<<k<<endl;
    LKrCand = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(k));
    TVector2 pos;
    pos.SetX(LKrCand->GetClusterX());
    pos.SetY(LKrCand->GetClusterY());
    if(verb) cout<<"position "<<pos.X()<<" "<<pos.Y()<<endl;
    double d = (trackAtLKr-pos).Mod();
    FillHisto("hLKrAssocDistLKrTrack", d);
    if(verb) cout<<"minimal D? "<<d<<" < "<<minD<<endl;
    if(d<minD){
      minD = d;
      bestID = k;
      if(verb) cout<<"bestLKr "<<bestID<<endl;
    };
    if(verb) cout<<endl;
  };
  if(bestID==-1) return hasAssoc;
  LKrCand = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(bestID));
  double t = LKrCand->GetTime() + fOffsetLKrStandard;
  FillHisto("hLKrAssocTimeDiffLKrSTRAW", t-tSTRAW);
  if(verb) cout<<"time "<<t<<endl;
  if(verb) cout<<"tLKr - tSTRAW = "<<fabs(t-tSTRAW)<<" < "<<fCutTimeDiffLKrStandardSTRAW<<endl;
  if(verb) cout<<"distance "<<minD<<" < "<<fCutMaxDistLKrStandard<<endl;
  if((minD>fCutMaxDistLKrStandard) || (fabs(t-tSTRAW)>fCutTimeDiffLKrStandardSTRAW)) bestID = -1;

  if(bestID!=-1){
    hasAssoc = true;
    lkrID = bestID;
    tLKr = t;
    bestLKrEnergy = LKrCand->GetClusterEnergy();
    seedEnergy = LKrCand->GetClusterSeedEnergy();
    nCells = LKrCand->GetNCells();
    clusterPos.SetX(LKrCand->GetClusterX());
    clusterPos.SetY(LKrCand->GetClusterY());
  };
  return hasAssoc;
}

bool BestTrackSelection::HasLKrCellAssoc(TVector2 trackAtLKr, double tSTRAW, double &tLKr, double &bestLKrEnergy, double &seedEnergy, int &nCells, TVector2 &clusterPos){
  bool hasCellAssoc = false;
  int ncells = 0;
  double ECluster = 0.;
  double posX = 0.;
  double posY = 0.;
  double maxEnergy = 0.;
  double tCluster = -99999.;
  for(int k=0; k<fLKrEvent->GetNHits(); k++){
    if(verb) cout<<"hit "<<k<<endl;
    TRecoLKrHit *LKrHit = static_cast<TRecoLKrHit*>(fLKrEvent->GetHit(k));
    double t = LKrHit->GetTime() + fOffsetLKrCell;
    if(verb){
      cout<<"time "<<t<<endl;
      cout<<"position "<<LKrHit->GetPosition().XYvector().X()<<" "<<LKrHit->GetPosition().XYvector().Y()<<endl;
    };
    FillHisto("hDistLKrHitTrack", (LKrHit->GetPosition().XYvector()-trackAtLKr).Mod());
    FillHisto("hTimeDiffLKrHitSTRAW", t-tSTRAW);
    if(verb) cout<<"distance = "<<(LKrHit->GetPosition().XYvector()-trackAtLKr).Mod()<<" < "<<fCutMaxDistLKrCell<<endl;
    if(verb) cout<<"tLKr - tSTRAW = "<<fabs(t-tSTRAW)<<" < "<<fCutTimeDiffLKrCellSTRAW<<endl;
    if((LKrHit->GetPosition().XYvector()-trackAtLKr).Mod()<fCutMaxDistLKrCell && fabs(t-tSTRAW)<fCutTimeDiffLKrCellSTRAW){
      ncells++;
      ECluster+=LKrHit->GetEnergy();
      posX+=(LKrHit->GetPosition().X())*(LKrHit->GetEnergy());
      posY+=(LKrHit->GetPosition().Y())*(LKrHit->GetEnergy());
      if(verb){
	cout<<"E hit "<<LKrHit->GetEnergy()<<endl;
	cout<<"current E cluster "<<ECluster<<endl;
	cout<<"current n cells "<<ncells<<endl;
      };
      if(verb) cout<<"Is max energy? "<<LKrHit->GetEnergy()<<" > "<<maxEnergy<<endl;
      if(LKrHit->GetEnergy()>maxEnergy){
	maxEnergy = LKrHit->GetEnergy();
	tCluster = t;
	if(verb) cout<<"cluster time "<<tCluster<<endl;
      };
    };
    if(verb) cout<<endl;
  };
  FillHisto("hLKrAssocClusterMaxHitEnergy", maxEnergy);
  FillHisto("hLKrAssocClusterTimeDiffClusterSTRAW", tCluster-tSTRAW);
  FillHisto("hLKrMatch2Found", ((maxEnergy<=fCutMinEnergy) ? 0 : 1));
  posX = posX/ECluster;
  posY = posY/ECluster;
  ECluster = CorrectLKrCellCluster(ncells, ECluster);
  if(verb){
    cout<<"cluster energy = "<<ECluster<<endl;
    cout<<"tLKr = "<<tCluster<<endl;
    cout<<"Seed energy = "<<maxEnergy<<" > "<<fCutMinEnergy<<endl;
  };
  if(maxEnergy>fCutMinEnergy){
    hasCellAssoc = true;
    clusterPos.Set(posX, posY);
    tLKr = tCluster;
    bestLKrEnergy = ECluster;
    nCells = ncells;
    seedEnergy = maxEnergy;
  };
  return hasCellAssoc;
}
