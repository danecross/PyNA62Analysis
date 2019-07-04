// ---------------------------------------------------------
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-05-13
//
// ---------------------------------------------------------
/// \class MatchingRG
/// \Brief
/// Algorithm with track-GTK matching by Giuseppe & Rado.
/// \EndBrief
/// \Detailed
/// Track-GTK matching algorithm developed by Giuseppe and Rado.
/// This algorithm is used in PnnKinematicTails analyzers.
///
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "MatchingRG.hh"
#include "SpectrometerTrackVertex.hh"
#include "BlueTubeTracker.hh"
#include "BeamParameters.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include <TF1.h>
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

MatchingRG::MatchingRG(BaseAnalysis *ba, Analyzer* ana, const std::string &name) : Algorithm(ba, ana, name) {
  ftracker = BlueTubeTracker::GetInstance();

  //IsBeamParticle (RG chi2 condition)
  fChi2 = 50.;
  fMaxChi2Event = 20.;
  fDeltaT = 0.6;

  //Normalization
  fMaxDeltaT = 0.95;
  fMaxCDA = 60.;

  //PileupTreatment
  fCutSimilarRatioCDA = 1.5;
  fCutSimilarRatioDT = 1.5;
  fDeltaD1 = 0.3;

  //final cuts
  fMaxNInTimeWithKTAG = 5;
  fMinD = 0.03;
  fMinD1 = 0.005;
  fMinD2 = 0.005;

  fCDA = new TF1("fCDA", "[0]*exp(-0.5*(x/[1])*(x/[1])) + [2]*exp(-0.5*(x/[3])*(x/[3])) + [4]*exp([5]*x) + [6] + [7]*x", 0., 1000.);
  fCDA->SetParameter(0, 0.0702); //G
  fCDA->SetParameter(1, 1.47); //G
  fCDA->SetParameter(2, 0.0229); //G
  fCDA->SetParameter(3, 2.6); //G
  fCDA->SetParameter(4, 0.0145); //G
  fCDA->SetParameter(5, -0.319); //G
  fCDA->SetParameter(6, 0.000033); //G
  fCDA->SetParameter(7, -0.0000016); //G

  fDT1 = new TF1("fDT1", "[0]*exp(-0.5*(x/[1])*(x/[1])) + [2]*exp(-0.5*(x/[3])*(x/[3]))", -100., 100.);
  fDT1->SetParameter(0, 0.027); //0.024
  fDT1->SetParameter(1, 0.13); //0.19
  fDT1->SetParameter(2, 0.0017); //0.00026
  fDT1->SetParameter(3, 0.23); //0.4

  fDT2 = new TF1("fDT2", "[0]*exp(-0.5*(x/[1])*(x/[1])) + [2]*exp(-0.5*(x/[3])*(x/[3]))", -100., 100.);
  fDT2->SetParameter(0, 0.026);
  fDT2->SetParameter(1, 0.141);
  fDT2->SetParameter(2, 0.001);
  fDT2->SetParameter(3, 0.279);

  fCDA_p = new TF1("fCDA_p", "[0]*exp(-0.5*((x-[1])/[2])*((x-[1])/[2])) + [3]*exp(-0.5*(x/[4])*(x/[4]))", 0., 1000.);
  fCDA_p->SetParameter(0, 0.0122); //0.024
  fCDA_p->SetParameter(1, 0.); //0
  fCDA_p->SetParameter(2, 5.95); //0.19
  fCDA_p->SetParameter(3, 0.0235); //0.00026
  fCDA_p->SetParameter(4, 13.87); //0.4

  fDT_p = new TF1("fDT_p", "[0]", -100., 100.);
  fDT_p->SetParameter(0, 0.0081);

  DiscriminantNormalization();
}

void MatchingRG::Init(TString s){
  BookHisto(new TH1F(s+"hNGTKCandidates", "hNGTKCandidates", 50, 0, 50));
  BookHisto(new TH1F(s+"hGTKChi2", "hGTKChi2", 100, 0., 50.));
  BookHisto(new TH1F(s+"hTimeDiffGTKRef", "hTimeDiffGTKRef", 100, -5., 5.));
  BookHisto(new TH1F(s+"hGTKChi2Event", "hGTKChi2Event", 100, 0., 50.));
  BookHisto(new TH1I(s+"hIsBeamParticle", "hIsBeamParticle", 2, 0, 2));

  BookHisto(new TH1F(s+"hVertexZ", "hVertexZ", 200, 100000., 200000.));
  BookHisto(new TH1D(s+"hDeltaT1", "hDeltaT1", 400, -2., 2.));
  BookHisto(new TH1D(s+"hDeltaT2", "hDeltaT2", 400, -2., 2.));
  BookHisto(new TH1D(s+"hCDA", "hCDA", 200, 0., 60.));
  BookHisto(new TH2F(s+"hCDAvsDeltaT1", "hCDAvsDeltaT1", 400, -2., 2., 200, 0., 60.));
  BookHisto(new TH2F(s+"hCDAvsDeltaT2", "hCDAvsDeltaT2", 400, -2., 2., 200, 0., 60.));
  BookHisto(new TH2F(s+"hDeltaT2vsDeltaT1", "hDeltaT2vsDeltaT1", 200, -2., 2., 200, -2., 2.));
  BookHisto(new TH1D(s+"hD12_partCDA", "hD12_partCDA", 200, 0., 1.));
  BookHisto(new TH1D(s+"hD1_partT1", "hD1_partT1", 200, 0., 1.));
  BookHisto(new TH1D(s+"hD2_partT2", "hD2_partT2", 200, 0., 1.));
  BookHisto(new TH1D(s+"hDp_partCDA", "hDp_partCDA", 200, 0., 1.));
  BookHisto(new TH1D(s+"hDp_partT", "hDp_partT", 200, 0., 1.));
  BookHisto(new TH1D(s+"hD1", "hD1", 400, 0., 1.));
  BookHisto(new TH1D(s+"hD2", "hD2", 400, 0., 1.));
  BookHisto(new TH1D(s+"hDp", "hDp", 400, 0., 1.));
  BookHisto(new TH2D(s+"hD2_vs_D1", "hD2_vs_D1", 200, 0., 1., 200, 0., 1.));
  BookHisto(new TH2D(s+"hD1_vs_Dp", "hD1_vs_Dp", 200, 0., 1., 200, 0., 1.));
  BookHisto(new TH2D(s+"hD2_vs_Dp", "hD2_vs_Dp", 200, 0., 1., 200, 0., 1.));

  BookHisto(new TH1D(s+"hDiffD1AllMinusBest", "hDiffD1AllMinusBest", 200, 0., 1.));
  BookHisto(new TH1D(s+"hDiffD1BestTwo", "hDiffD1BestTwo", 200, 0., 1.));
  BookHisto(new TH2D(s+"hNBeamParticlesInTime_vs_DiffD1BestTwo", "hNBeamParticlesInTime_vs_DiffD1BestTwo", 200, 0., 1., 20, 0, 20));

  BookHisto(new TH1D(s+"hR_dt_i", "hR_dt_i", 100, 0., 1.));
  BookHisto(new TH1D(s+"hR_cda_i", "hR_cda_i", 100, 0., 1.));
  BookHisto(new TH1D(s+"hR_dt_j", "hR_dt_j", 100, 0., 1.));
  BookHisto(new TH1D(s+"hR_cda_j", "hR_cda_j", 100, 0., 1.));
  BookHisto(new TH1D(s+"hRR_dt_ij", "hRR_dt_ij", 500, 0., 5.));
  BookHisto(new TH1D(s+"hRR_cda_ij", "hRR_cda_ij", 500, 0., 5.));

  BookHisto(new TH1D(s+"hD1_best", "hD1_best", 400, 0., 1.));
  BookHisto(new TH1D(s+"hD2_best", "hD2_best", 400, 0., 1.));
  BookHisto(new TH2D(s+"hD2_best_vs_D1_best", "hD2_best_vs_D1_best", 200, 0., 1., 200, 0., 1.));
  BookHisto(new TH2F(s+"hDeltaY_best_vs_DeltaX_best", "hDeltaY_best_vs_DeltaX_best", 200, -50., 50., 200, -50., 50.));
  BookHisto(new TH2D(s+"hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH_aftFinalCuts", "hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH_aftFinalCuts", 200, -2., 2., 200, 0., 50.));
  BookHisto(new TH2D(s+"hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG_aftFinalCuts", "hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG_aftFinalCuts", 200, -2., 2., 200, 0., 50.));
  BookHisto(new TH1I(s+"hNBeamParticlesInTime", "hNBeamParticlesInTime", 20, 0, 20));
  BookHisto(new TH1F(s+"hGTKMatched", "hGTKMatched", 2, 0, 2));
}

void MatchingRG::Process(TRecoGigaTrackerEvent *GTKEvent, TRecoSpectrometerCandidate *StrawCand, double refTime, double trackTime1, double trackTime2, bool fill, TString s){
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"*******************"<<endl;
    cout<<"MatchingRG - Process"<<endl;
    cout<<"*******************"<<endl;
    cout<<endl;
  };

 //prepare output
  PrepareDefaultOutputs();

  // loop over all GTK candidates for a STRAW candidate
  int beamParticlesInTime = 0;
  if(fill) FillHisto(s+"hNGTKCandidates", GTKEvent->GetNCandidates());
  cout<<user()<<"N GTK candidates: "<<GTKEvent->GetNCandidates()<<endl;
  for(int i=0; i<GTKEvent->GetNCandidates(); i++){
    cout<<user()<<"._._._._._._._._._._._."<<endl;
    cout<<user()<<"GTK candidate "<<i<<endl;
    TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(i));

    cout<<user()<<"Is in-time? "<<endl;
    cout<<user()<<"GTK time = "<<GTKCand->GetTime()<<endl;
    double timeDiff = GTKCand->GetTime() - refTime;
    if(fill) FillHisto(s+"hTimeDiffGTKRef", timeDiff);
    cout<<user()<<"GTK time - reference time: "<<-1*fDeltaT<<" < "<<timeDiff<<" < "<<fDeltaT<<endl;
    bool isInTime = (fabs(timeDiff)<fDeltaT);
    cout<<user()<<"It is "<<(isInTime?"":"not")<<" in time."<<endl;

    cout<<user()<<"Is it beam particle?"<<endl;
    bool isBeamParticle = IsBeamParticle(GTKCand, fill, s);
    if(fill) FillHisto(s+"hIsBeamParticle", isBeamParticle);
    cout<<user()<<"It is "<<(isBeamParticle?"":"not")<<" beam particle."<<endl;

    if(isInTime && isBeamParticle){
      beamParticlesInTime++;
      cout<<user()<<"It is in-time beam particle. "<<"Currently "<<beamParticlesInTime<<" candidates are in-time beam particles."<<endl;

      cout<<user()<<"Find best GTK. "<<"track time (ktag) "<<trackTime1<<" track time (rich) "<<trackTime2<<endl;
      FindBestGTK(GTKCand, StrawCand, trackTime1, trackTime2, i, fill, s); //fill output vectors according to largest discriminant (D1)
    };
  };
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"All matched GTK candidates:"<<endl;
    for(unsigned int k=0; k<fGTKID.size(); k++){
      cout<<"----------------------------------------"<<endl;
      cout<<"ID: "<<fGTKID.at(k)<<endl;;
      cout<<"Time: "<<fGTKTime.at(k)<<endl;;
      cout<<"Vertex: "<<fVertex.at(k).X()<<" "<<fVertex.at(k).Y()<<" "<<fVertex.at(k).Z()<<endl;
      cout<<"TrackMomentum: "<<fTrackMomentum.at(k).X()<<" "<<fTrackMomentum.at(k).Y()<<" "<<fTrackMomentum.at(k).Z()<<endl;
      cout<<"GTKMomentum: "<<fGTKMomentum.at(k).X()<<" "<<fGTKMomentum.at(k).Y()<<" "<<fGTKMomentum.at(k).Z()<<endl;
      cout<<"TrackPosition: "<<fTrackPosition.at(k).X()<<" "<<fTrackPosition.at(k).Y()<<" "<<fTrackPosition.at(k).Z()<<endl;
      cout<<"GTKPosition: "<<fGTKPosition.at(k).X()<<" "<<fGTKPosition.at(k).Y()<<" "<<fGTKPosition.at(k).Z()<<endl;
      cout<<"MatchingQuality1: "<<fMatchingQuality1.at(k)<<endl;
      cout<<"MatchingQuality2: "<<fMatchingQuality2.at(k)<<endl;
      cout<<"MatchingQualityP: "<<fMatchingQualityP.at(k)<<endl;
      cout<<"MatchingParts1: "<<fMatchingParts1.at(k).first<<" "<<fMatchingParts1.at(k).second<<endl;
      cout<<"MatchingParts2: "<<fMatchingParts2.at(k).first<<" "<<fMatchingParts2.at(k).second<<endl;
      cout<<"MatchingPartsP: "<<fMatchingPartsP.at(k).first<<" "<<fMatchingPartsP.at(k).second<<endl;
    };
    cout<<endl;
  };

  cout<<user()<<"More similar candidates?"<<endl;
  int NCloseDiscr = count_if(fMatchingQuality1.begin(), fMatchingQuality1.end(), [&](double i){return ((i>0.) && (fMatchingQuality1.at(0)-i)<fDeltaD1);});
  if(fill){
    for(unsigned int k=1; k<fMatchingQuality1.size(); k++){
      if(fMatchingQuality1.at(k)>0.) FillHisto(s+"hDiffD1AllMinusBest", fabs(fMatchingQuality1.at(k)-fMatchingQuality1.at(0)));
    };
    if(fMatchingQuality1.size()>2){
      FillHisto(s+"hDiffD1BestTwo", fabs(fMatchingQuality1.at(1)-fMatchingQuality1.at(0)));
      FillHisto(s+"hNBeamParticlesInTime_vs_DiffD1BestTwo", fabs(fMatchingQuality1.at(1)-fMatchingQuality1.at(0)), beamParticlesInTime);
    };
  };
  cout<<user()<<"Number of candidates with deltaD<"<<fDeltaD1<<": "<<NCloseDiscr<<endl;
  cout<<user()<<"Number of in-time beam particles "<<beamParticlesInTime<<" > 2"<<endl;
  if((beamParticlesInTime>2) && (NCloseDiscr>1)){
    cout<<user()<<"More similar candidates."<<endl;
    std::vector<int> PU;
    std::vector<double> QualityPU;
    FindBestPU(fGTKID, fMatchingQualityP, NCloseDiscr, PU, QualityPU);
    cout<<user()<<"Best PU "<<PU.at(0)<<"=  Best GTK "<<fGTKID.at(0)<<endl;
    cout<<user()<<"Are similar? "<<endl;
    if((PU.at(0)!=fGTKID.at(0)) || AreSimilar(0, std::distance(fGTKID.begin(), std::find(fGTKID.begin(), fGTKID.end(), PU.at(1))), fill, s)){
      cout<<user()<<"Best two pileup candidates too similar or Best GTK is not best PU."<<endl;
      cout<<user()<<"No match found"<<endl;
      RestoreDefaultOutputs();
    };
  }else{
    cout<<user()<<"Only one best candidate."<<endl;
  };

  //if the conditions are not met, leave only the default values
  double D1 = fMatchingQuality1.at(0);
  double D2 = fMatchingQuality2.at(0);
  cout<<user()<<"Number of in-time candidate: "<<beamParticlesInTime<<" <= "<<fMaxNInTimeWithKTAG<<endl;;
  cout<<user()<<"At least one large discriminant? D1: "<<D1<<" > "<<fMinD<<" D2: "<<D2<<" > "<<fMinD<<endl;
  cout<<user()<<"Discriminants large enough? D1: "<<D1<<" > "<<fMinD1<<" D2: "<<D2<<" > "<<fMinD2<<endl;
  if(beamParticlesInTime>fMaxNInTimeWithKTAG || !((D1>fMinD || D2>fMinD) && (D1>fMinD1) && (D2>fMinD2))){
    cout<<user()<<"Too many beam particles GTK candidates in time with KTAG or Too small discriminant(s)"<<endl;
    cout<<user()<<"No match found"<<endl;
    RestoreDefaultOutputs();
  };

  //if the conditions are met, erase default values from the output vectors
  if(fGTKID.at(0)!=-1){
    cout<<user()<<"Matching candidate found: "<<fGTKID.at(0)<<endl;
    if(fill){
      FillHisto(s+"hGTKMatched", 1);
      FillHisto(s+"hD1_best", D1);
      FillHisto(s+"hD2_best", D2);
      FillHisto(s+"hD2_best_vs_D1_best", D1, D2);
      FillHisto(s+"hDeltaY_best_vs_DeltaX_best", (fGTKPosition.at(0) - fTrackPosition.at(0)).X(), (fGTKPosition.at(0) - fTrackPosition.at(0)).Y());
      FillHisto(s+"hTrackGTKDistAtVertexVsTimeDiffMatchedGTKRICH_aftFinalCuts", fGTKTime.at(0) - trackTime2, (fGTKPosition.at(0) - fTrackPosition.at(0)).Mag());
      FillHisto(s+"hTrackGTKDistAtVertexVsTimeDiffMatchedGTKKTAG_aftFinalCuts", fGTKTime.at(0) - trackTime1, (fGTKPosition.at(0) - fTrackPosition.at(0)).Mag());
      FillHisto(s+"hNBeamParticlesInTime", beamParticlesInTime);
    };
    EraseDefaultOutputs();
  }else{
    if(fill) FillHisto(s+"hGTKMatched", 0);
  };

  if(TestLevel(Verbosity::kUser)){
    for(unsigned int k=0; k<fGTKID.size(); k++){
      cout<<"----------------------------------------"<<endl;
      cout<<"ID: "<<fGTKID.at(k)<<endl;;
      cout<<"Time: "<<fGTKTime.at(k)<<endl;;
      cout<<"Vertex: "<<fVertex.at(k).X()<<" "<<fVertex.at(k).Y()<<" "<<fVertex.at(k).Z()<<endl;
      cout<<"TrackMomentum: "<<fTrackMomentum.at(k).X()<<" "<<fTrackMomentum.at(k).Y()<<" "<<fTrackMomentum.at(k).Z()<<endl;
      cout<<"GTKMomentum: "<<fGTKMomentum.at(k).X()<<" "<<fGTKMomentum.at(k).Y()<<" "<<fGTKMomentum.at(k).Z()<<endl;
      cout<<"TrackPosition: "<<fTrackPosition.at(k).X()<<" "<<fTrackPosition.at(k).Y()<<" "<<fTrackPosition.at(k).Z()<<endl;
      cout<<"GTKPosition: "<<fGTKPosition.at(k).X()<<" "<<fGTKPosition.at(k).Y()<<" "<<fGTKPosition.at(k).Z()<<endl;
      cout<<"MatchingQuality1: "<<fMatchingQuality1.at(k)<<endl;
      cout<<"MatchingQuality2: "<<fMatchingQuality2.at(k)<<endl;
      cout<<"MatchingQualityP: "<<fMatchingQualityP.at(k)<<endl;
      cout<<"MatchingParts1: "<<fMatchingParts1.at(k).first<<" "<<fMatchingParts1.at(k).second<<endl;
      cout<<"MatchingParts2: "<<fMatchingParts2.at(k).first<<" "<<fMatchingParts2.at(k).second<<endl;
      cout<<"MatchingPartsP: "<<fMatchingPartsP.at(k).first<<" "<<fMatchingPartsP.at(k).second<<endl;
      cout<<"----------------------------------------"<<endl;
    };
  };
}

MatchingRG::~MatchingRG(){
  delete fCDA;
  delete fDT1;
  delete fDT2;
  delete fCDA_p;
  delete fDT_p;
}

TVector3 MatchingRG::GetVertex(TVector3 trackMom, TVector3 trackPos, TVector3 kaonMom, TVector3 kaonPos){
  fTwoLinesCDA.SetLine1Point1(trackPos);
  fTwoLinesCDA.SetDir1(trackMom);
  fTwoLinesCDA.SetLine2Point1(kaonPos);
  fTwoLinesCDA.SetDir2(kaonMom);
  fTwoLinesCDA.ComputeVertexCDA();
  return fTwoLinesCDA.GetVertex();
}

void MatchingRG::ApplyBlueTube(int charge, TVector3 oldPos, TVector3 oldMom, double finalZ, TVector3 *newPos, TVector3 *newMom){
  ftracker->SetCharge(charge);
  ftracker->SetInitialPosition(oldPos);
  ftracker->SetInitialMomentum(oldMom);
  ftracker->SetZFinal(finalZ);
  ftracker->TrackParticle();
  TVector3 mom = ftracker->GetFinalMomentum();
  TVector3 pos = ftracker->GetFinalPosition();
  newPos->SetXYZ(pos.X(), pos.Y(), pos.Z());
  newMom->SetXYZ(mom.X(), mom.Y(), mom.Z());
}

bool MatchingRG::IsBeamParticle(TRecoGigaTrackerCandidate *GTKCand, bool fill, TString s){
  cout<<user()<<"Chi2: "<<GTKCand->GetChi2()<<" <= "<<fChi2<<endl;
  if(fill) FillHisto(s+"hGTKChi2", GTKCand->GetChi2());
  if(GTKCand->GetChi2()>fChi2){
    cout<<user()<<"Not beam particle (wrong chi2), try other candidates."<<endl;
    return 0;
  };

  TVector3 mom = GTKCand->GetMomentum();
  double chi2Event = pow((mom.Mag() - 74900.)/900., 2) + pow((mom.X()/mom.Z() - 0.00122)/0.00012, 2) + pow((mom.Y()/mom.Z() - 0.000025)/0.0001, 2);
  if(TestLevel(Verbosity::kUser)){
    cout<<"momentum diff: "<<mom.Mag()<<" - 74900."<<endl;
    cout<<"slope X diff: "<<mom.X()/mom.Z()<<" - 0.00122"<<endl;
    cout<<"slope Y diff: "<<mom.Y()/mom.Z()<<" - 0.000025"<<endl;
    cout<<"Chi2Event = "<<pow((mom.Mag() - 74900.)/900., 2)<<" + "<<pow((mom.X()/mom.Z() - 0.00122)/0.00012, 2)<<" + "<<pow((mom.Y()/mom.Z() - 0.000025)/0.0001, 2)<<endl;
  };
  cout<<user()<<"Chi2Event: "<<chi2Event<<" < "<<fMaxChi2Event<<endl;
  if(fill) FillHisto(s+"hGTKChi2Event", chi2Event);
  if(chi2Event>=fMaxChi2Event){
    cout<<user()<<"Not beam particle (wrong chi2Event), try other candidates."<<endl;
    return 0;
  };

  return 1;
}

void MatchingRG::FindBestGTK(TRecoGigaTrackerCandidate* GTKCand, TRecoSpectrometerCandidate* STRAWCand, double time1, double time2, int i, bool fill, TString s){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;

  //GTK candidate - original position and momentum
  TVector3 GTK;
  TVector3 newPosGTK;
  GTK.SetXYZ(GTKCand->GetMomentum().X(), GTKCand->GetMomentum().Y(), GTKCand->GetMomentum().Z());
  newPosGTK.SetXYZ(GTKCand->GetPosition(2).X(), GTKCand->GetPosition(2).Y(), GTKCand->GetPosition(2).Z());

  //track - original position and momentum
  TVector3 Track;
  TVector3 newPosTrack;
  Track.SetXYZ(STRAWCand->GetThreeMomentumBeforeMagnet().X(), STRAWCand->GetThreeMomentumBeforeMagnet().Y(), STRAWCand->GetThreeMomentumBeforeMagnet().Z());
  newPosTrack.SetXYZ(STRAWCand->GetPositionBeforeMagnet().X(), STRAWCand->GetPositionBeforeMagnet().Y(), STRAWCand->GetPositionBeforeMagnet().Z());

  if(TestLevel(Verbosity::kUser)){
    cout<<"track momentum before magnet: "<<Track.X()<<" "<<Track.Y()<<" "<<Track.Z()<<endl;
    cout<<"GTK momentum before magnet: "<<GTK.X()<<" "<<GTK.Y()<<" "<<GTK.Z()<<endl;
    cout<<"track position before magnet: "<<newPosTrack.X()<<" "<<newPosTrack.Y()<<" "<<newPosTrack.Z()<<endl;
    cout<<"GTK position before magnet: "<<newPosGTK.X()<<" "<<newPosGTK.Y()<<" "<<newPosGTK.Z()<<endl;
  };

  //apply BT field iteratively until they are 5m apart
  int count = 0;
  TVector3 simpleVert;
  cout<<user()<<"Find vertex position"<<endl;
  while(fabs(newPosGTK.Z() - newPosTrack.Z())>5000.){
    simpleVert = GetVertex(Track, newPosTrack, GTK, newPosGTK);
    if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)){
      cout<<user()<<"found vertex position Z = "<<simpleVert.Z()<<endl;
      cout<<user()<<"vertex Z is out of bounds"<<endl;
      return;
    };
    ApplyBlueTube(1, newPosGTK, GTK, (simpleVert.Z() - newPosGTK.Z())/2. + newPosGTK.Z(), &newPosGTK, &GTK);
    ApplyBlueTube(STRAWCand->GetCharge(), newPosTrack, Track, newPosTrack.Z() - (-simpleVert.Z() + newPosTrack.Z())/2., &newPosTrack, &Track);
    count++;
    if(count>50) break;
  };
  simpleVert = GetVertex(Track, newPosTrack, GTK, newPosGTK);
  cout<<user()<<"vertex "<<simpleVert.X()<<" "<<simpleVert.Y()<<" "<<simpleVert.Z()<<endl;
  cout<<user()<<"Is good vertex Z? "<<GTK3Z<<" <= "<<simpleVert.Z()<<" <= "<<STRAW0Z_front<<endl;
  if(fill) FillHisto(s+"hVertexZ", simpleVert.Z());
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return;
  cout<<user()<<"Apply Blue Tube"<<endl;
  ApplyBlueTube(1, GTKCand->GetPosition(2), GTKCand->GetMomentum(), simpleVert.Z(), &newPosGTK, &GTK);
  ApplyBlueTube(STRAWCand->GetCharge(), STRAWCand->GetPositionBeforeMagnet(), STRAWCand->GetThreeMomentumBeforeMagnet(), simpleVert.Z(), &newPosTrack, &Track);
  double deltaT1 = time1 - GTKCand->GetTime();
  double deltaT2 = time2 - GTKCand->GetTime();
  double cda = sqrt(pow(newPosTrack.X() - newPosGTK.X(), 2) + pow(newPosTrack.Y() - newPosGTK.Y(), 2));
  if(fill){
    FillHisto(s+"hDeltaT1", deltaT1);
    FillHisto(s+"hDeltaT2", deltaT2);
    FillHisto(s+"hCDA", cda);
    FillHisto(s+"hCDAvsDeltaT1", deltaT1, cda);
    FillHisto(s+"hCDAvsDeltaT2", deltaT2, cda);
    FillHisto(s+"hDeltaT2vsDeltaT1", deltaT1, deltaT2);
  };
  cout<<user()<<"CDA at vertex: "<<cda<<" < "<<fMaxCDA<<endl;
  cout<<user()<<"DeltaT1 at vertex: "<<deltaT1<<" < "<<fMaxDeltaT<<endl;
  if((cda>fMaxCDA) || (fabs(deltaT1)>fMaxDeltaT)) return;

  double NnormCDA = 0.;
  double NnormDT1 = 0.;
  double NnormDT2 = 0.;
  double NnormCDA_p = 0.;
  double NnormDT_p = 0.;
  double NintCDA = 0.;
  double NintDT1 = 0.;
  double NintDT2 = 0.;
  double NintCDA_p = 0.;
  double NintDT_p = 0.;
  double pCDA = 1.;
  double pDT1 = 1.;
  double pDT2 = 1.;
  double pCDA_p = 1.;
  double pDT_p = 1.;
  EvaluateDiscriminant(cda, deltaT1, deltaT2, NnormCDA, NnormDT1, NnormDT2, NnormCDA_p, NnormDT_p, NintCDA, NintDT1, NintDT2, NintCDA_p, NintDT_p, pCDA, pDT1, pDT2, pCDA_p, pDT_p);
  double D1 = (1.-pCDA)*(1.-pDT1);
  double D2 = (1.-pCDA)*(1.-pDT2);
  double Dp = (1.-pCDA_p)*(1.-pDT_p);
  if(fill){
    FillHisto(s+"hD12_partCDA", pCDA);
    FillHisto(s+"hDp_partCDA", pCDA_p);
    FillHisto(s+"hD1_partT1", pDT1);
    FillHisto(s+"hD2_partT2", pDT2);
    FillHisto(s+"hDp_partT", pDT_p);
    FillHisto(s+"hD1", D1);
    FillHisto(s+"hD2", D2);
    FillHisto(s+"hDp", Dp);
    FillHisto(s+"hD2_vs_D1", D1, D2);
    FillHisto(s+"hD1_vs_Dp", Dp, D1);
    FillHisto(s+"hD2_vs_Dp", Dp, D2);
 };
  if(TestLevel(Verbosity::kUser)){
    cout<<"track momentum at vertex: "<<Track.X()<<" "<<Track.Y()<<" "<<Track.Z()<<endl;
    cout<<"GTK momentum at vertex: "<<GTK.X()<<" "<<GTK.Y()<<" "<<GTK.Z()<<endl;
    cout<<"track position at vertex: "<<newPosTrack.X()<<" "<<newPosTrack.Y()<<" "<<newPosTrack.Z()<<endl;
    cout<<"GTK position at vertex: "<<newPosGTK.X()<<" "<<newPosGTK.Y()<<" "<<newPosGTK.Z()<<endl;
    cout<<"track time (ktag): "<<time1<<endl;
    cout<<"track time (rich): "<<time2<<endl;
    cout<<"GTK time: "<<GTKCand->GetTime()<<endl;
    cout<<"deltaT1 = "<<deltaT1<<endl;
    cout<<"deltaT2 = "<<deltaT2<<endl;
    cout<<"CDA = "<<cda<<endl;
    cout<<"pDT1 = "<<pDT1<<" NnormDT1 = "<<NnormDT1<<" IntegralDT1 = "<<NintDT1<<endl;
    cout<<"pDT2 = "<<pDT2<<" NnormDT2 = "<<NnormDT2<<" IntegralDT2 = "<<NintDT2<<endl;
    cout<<"pCDA = "<<pCDA<<" NnormCDA = "<<NnormCDA<<" IntegralCDA = "<<NintCDA<<endl;
    cout<<"pDT_p = "<<pDT_p<<" NnormDT_p = "<<NnormDT_p<<" IntegralDT_p = "<<NintDT_p<<endl;
    cout<<"pCDA_p = "<<pCDA_p<<" NnormCDA_p = "<<NnormCDA_p<<" IntegralCDA_p = "<<NintCDA_p<<endl;
    cout<<"D1 =  (1-pDT1)(1-pCDA) = "<<(1.-pDT1)<<" * "<<(1.-pCDA)<<" = "<<D1<<endl;
    cout<<"D2 =  (1-pDT2)(1-pCDA) = "<<(1.-pDT2)<<" * "<<(1.-pCDA)<<" = "<<D2<<endl;
    cout<<"Dp =  (1-pDT_p)(1-pCDA_p) = "<<(1.-pDT_p)<<" * "<<(1.-pCDA_p)<<" = "<<Dp<<endl;
  };

  for(unsigned int j=0; j<fMatchingQuality1.size(); j++){
    cout<<user()<<"This D1 "<<D1<<" > "<<fMatchingQuality1.at(j)<<endl;
    if(D1>fMatchingQuality1.at(j)){
      fGTKID.insert(fGTKID.begin() + j, i);
      fGTKTime.insert(fGTKTime.begin() + j, GTKCand->GetTime());
      fVertex.insert(fVertex.begin() + j, simpleVert);
      fTrackMomentum.insert(fTrackMomentum.begin() + j, Track);
      fGTKMomentum.insert(fGTKMomentum.begin() + j, GTK);
      fMatchingParts1.insert(fMatchingParts1.begin() + j, std::pair<double, double>((1.-pCDA), (1.-pDT1)));
      fMatchingParts2.insert(fMatchingParts2.begin() + j, std::pair<double, double>((1.-pCDA), (1.-pDT2)));
      fMatchingPartsP.insert(fMatchingPartsP.begin() + j, std::pair<double, double>((1.-pCDA_p), (1.-pDT_p)));
      fMatchingQuality1.insert(fMatchingQuality1.begin() + j, D1);
      fMatchingQuality2.insert(fMatchingQuality2.begin() + j, D2);
      fMatchingQualityP.insert(fMatchingQualityP.begin() + j, Dp);
      fGTKPosition.insert(fGTKPosition.begin() + j, newPosGTK);
      fTrackPosition.insert(fTrackPosition.begin() + j, newPosTrack);
      cout<<user()<<"candidate "<<i<<" at position "<<j<<" based on matching quality"<<endl;
      break;
    };
  };
}

std::vector<int> MatchingRG::GetMatchedGTKIDs(){
  return fGTKID;
}

std::vector<double> MatchingRG::GetGTKTimes(){
  return fGTKTime;
}

std::vector<TVector3> MatchingRG::GetVertices(){
  return fVertex;
}

std::vector<TVector3> MatchingRG::GetTrackMomentaAtVertices(){
  return fTrackMomentum;
}

std::vector<TVector3> MatchingRG::GetGTKMomentaAtVertices(){
  return fGTKMomentum;
}

std::vector<double> MatchingRG::GetMatchingQuality1(){
  return fMatchingQuality1;
}

std::vector<double> MatchingRG::GetMatchingQuality2(){
  return fMatchingQuality2;
}

std::vector<double> MatchingRG::GetMatchingQualityP(){
  return fMatchingQualityP;
}

std::vector<std::pair<double, double>> MatchingRG::GetMatchingParts1(){
  return fMatchingParts1;
}

std::vector<std::pair<double, double>> MatchingRG::GetMatchingParts2(){
  return fMatchingParts2;
}

std::vector<std::pair<double, double>> MatchingRG::GetMatchingPartsP(){
  return fMatchingPartsP;
}

std::vector<TVector3> MatchingRG::GetTrackPositionsAtVertices(){
  return fTrackPosition;
}

std::vector<TVector3> MatchingRG::GetGTKPositionsAtVertices(){
  return fGTKPosition;
}

TVector3 MatchingRG::MomAfterKick(TVector3 oldMom, double kick){
  TVector3 mom = oldMom;
  TVector2 pTvec(mom.X(), mom.Z());
  double pT = pTvec.Mod();
  double pxNew = mom.X() - kick;
  double beta = acos(mom.X()/pT);
  double theta = acos(pxNew/pT);
  double alpha = theta - beta;
  mom.RotateY(-alpha);
  TVector3 newMom = mom;

  return newMom;
}

void MatchingRG::SetMinD1(double D){
  fMinD1 = D;
}

void MatchingRG::SetMinD2(double D){
  fMinD2 = D;
}

void MatchingRG::SetMinD(double D){
  fMinD = D;
}

void MatchingRG::EraseDefaultOutputs(){
    fGTKID.erase(fGTKID.end()-1);
    fGTKTime.erase(fGTKTime.end()-1);
    fVertex.erase(fVertex.end()-1);
    fTrackMomentum.erase(fTrackMomentum.end()-1);
    fGTKMomentum.erase(fGTKMomentum.end()-1);
    fMatchingQuality1.erase(fMatchingQuality1.end()-1);
    fMatchingQuality2.erase(fMatchingQuality2.end()-1);
    fMatchingQualityP.erase(fMatchingQualityP.end()-1);
    fMatchingParts1.erase(fMatchingParts1.end()-1);
    fMatchingParts2.erase(fMatchingParts2.end()-1);
    fMatchingPartsP.erase(fMatchingPartsP.end()-1);
    fTrackPosition.erase(fTrackPosition.end()-1);
    fGTKPosition.erase(fGTKPosition.end()-1);
}

void MatchingRG::RestoreDefaultOutputs(){
    fGTKID.erase(fGTKID.begin(), fGTKID.end()-1);
    fGTKTime.erase(fGTKTime.begin(), fGTKTime.end()-1);
    fVertex.erase(fVertex.begin(), fVertex.end()-1);
    fTrackMomentum.erase(fTrackMomentum.begin(), fTrackMomentum.end() - 1);
    fGTKMomentum.erase(fGTKMomentum.begin(), fGTKMomentum.end() - 1);
    fMatchingQuality1.erase(fMatchingQuality1.begin(), fMatchingQuality1.end() - 1);
    fMatchingQuality2.erase(fMatchingQuality2.begin(), fMatchingQuality2.end() - 1);
    fMatchingQualityP.erase(fMatchingQualityP.begin(), fMatchingQualityP.end() - 1);
    fMatchingParts1.erase(fMatchingParts1.begin(), fMatchingParts1.end() - 1);
    fMatchingParts2.erase(fMatchingParts2.begin(), fMatchingParts2.end() - 1);
    fMatchingPartsP.erase(fMatchingPartsP.begin(), fMatchingPartsP.end() - 1);
    fTrackPosition.erase(fTrackPosition.begin(), fTrackPosition.end() - 1);
    fGTKPosition.erase(fGTKPosition.begin(), fGTKPosition.end() - 1);
}

void MatchingRG::PrepareDefaultOutputs(){
  fGTKID.clear();
  fGTKTime.clear();
  fVertex.clear();
  fTrackMomentum.clear();
  fGTKMomentum.clear();
  fMatchingQuality1.clear();
  fMatchingQuality2.clear();
  fMatchingQualityP.clear();
  fMatchingParts1.clear();
  fMatchingParts2.clear();
  fMatchingPartsP.clear();
  fTrackPosition.clear();
  fGTKPosition.clear();
  int gtkid = -1;
  double gtktime = -999999.;
  TVector3 vertex(0., 0., 0.);
  TVector3 trackmomentum(0., 0., 0.);
  TVector3 gtkmomentum(0., 0., 0.);
  TVector3 trackposition(0., 0., 0.);
  TVector3 gtkposition(0., 0., 0.);
  double quality = -999.;
  std::pair<int, int> p(quality, quality);
  fGTKID.push_back(gtkid);
  fGTKTime.push_back(gtktime);
  fVertex.push_back(vertex);
  fTrackMomentum.push_back(trackmomentum);
  fGTKMomentum.push_back(gtkmomentum);
  fTrackPosition.push_back(trackposition);
  fGTKPosition.push_back(gtkposition);
  fMatchingQuality1.push_back(quality);
  fMatchingQuality2.push_back(quality);
  fMatchingQualityP.push_back(quality);
  fMatchingParts1.push_back(p);
  fMatchingParts2.push_back(p);
  fMatchingPartsP.push_back(p);
}

void MatchingRG::FindBestPU(std::vector<int> GTKID, std::vector<double> MatchingQuality_p, int NCloseDiscr, std::vector<int> &PU, std::vector<double> &QualityPU){
  cout<<user()<<endl;
  cout<<user()<<"Find best PU"<<endl;
  double dpu = 0.;
  int id = -1;
  PU.push_back(id);
  QualityPU.push_back(dpu);
  cout<<user()<<"Number of candidates with close discriminant: "<<NCloseDiscr<<endl;
  for(int i=0; i<NCloseDiscr; i++){
    cout<<user()<<"Test candidate at place: "<<i<<endl;
    for(unsigned int j=0; j<QualityPU.size(); j++){
      cout<<user()<<"This matching quality: "<<MatchingQuality_p.at(i)<<" > "<<QualityPU.at(j)<<endl;
      if(MatchingQuality_p.at(i)>QualityPU.at(j)){
	QualityPU.insert(QualityPU.begin() + j, MatchingQuality_p.at(i));
	PU.insert(PU.begin() + j, GTKID.at(i));
	cout<<user()<<"candidate "<<PU.at(j)<<" at position "<<j<<" based on PU matching quality"<<endl;
	break;
      };
    };
  };
  cout<<user()<<endl;
}

bool MatchingRG::AreSimilar(int i, int j, bool fill, TString s){
  cout<<user()<<"Test candidates at positions "<<i<<" and "<<j<<endl;
  bool are = false;
  double DT1k = (fMatchingParts1.at(i)).second;
  double DT1p = (fMatchingPartsP.at(i)).second;
  double CDA1k = (fMatchingParts1.at(i)).first;
  double CDA1p = (fMatchingPartsP.at(i)).first;
  double DT2k = (fMatchingParts1.at(j)).second;
  double DT2p = (fMatchingPartsP.at(j)).second;
  double CDA2k = (fMatchingParts1.at(j)).first;
  double CDA2p = (fMatchingPartsP.at(j)).first;

  double R_dt_i = DT1k/DT1p;
  double R_cda_i = CDA1k/CDA1p;
  double R_dt_j = DT2k/DT2p;
  double R_cda_j = CDA2k/CDA2p;

  double RR_dt_ij = R_dt_i/R_dt_j;
  double RR_cda_ij = R_cda_i/R_cda_j;

  if(fill){
    FillHisto(s+"hR_dt_i", R_dt_i);
    FillHisto(s+"hR_cda_i", R_cda_i);
    FillHisto(s+"hR_dt_j", R_dt_j);
    FillHisto(s+"hR_cda_j", R_cda_j);
    FillHisto(s+"hRR_dt_ij", RR_dt_ij);
    FillHisto(s+"hRR_cda_ij", RR_cda_ij);
  };

  if(TestLevel(Verbosity::kUser)){
    cout<<"DT1k = "<<DT1k<<endl;
    cout<<"DT1p = "<<DT1p<<endl;
    cout<<"CDA1k = "<<CDA1k<<endl;
    cout<<"CDA1p = "<<CDA1p<<endl;
    cout<<"DT2k = "<<DT2k<<endl;
    cout<<"DT2p = "<<DT2p<<endl;
    cout<<"CDA2k = "<<CDA2k<<endl;
    cout<<"CDA2p = "<<CDA2p<<endl;
    cout<<"R_dt_i = "<<R_dt_i<<endl;
    cout<<"R_cda_i = "<<R_cda_i<<endl;
    cout<<"R_dt_j = "<<R_dt_j<<endl;
    cout<<"R_cda_j = "<<R_cda_j<<endl;
    cout<<"RR_dt_ij = "<<RR_dt_ij<<endl;
    cout<<"RR_cda_ij = "<<RR_cda_ij<<endl;
  };

  cout<<user()<<"RR_dt_ij: "<<RR_dt_ij<<" < 1.5"<<" || "<<"RR_cda_ij: "<<RR_cda_ij<<" < 1.5"<<endl;
  if((RR_dt_ij<fCutSimilarRatioDT) || (RR_cda_ij<fCutSimilarRatioCDA)){
    are = true;
  };
  cout<<user()<<"Similar: "<<(int)are<<endl;

  return are;
}

void MatchingRG::DiscriminantNormalization(){
  double pass_cda = 0.01; // mm
  double pass_dt = 0.001; // ns

  fPDFKaonCDA.clear();
  fIntPDFKaonCDA = 0.;
  fPDFPileupCDA.clear();
  fIntPDFPileupCDA = 0.;
  for(int jbincda(0); jbincda<6000; jbincda++){ // < 60 mm
    double cda = pass_cda*jbincda+0.5*pass_cda;
    double pdfkc = fCDA->Eval(cda);
    if(cda>25. || pdfkc<0.) pdfkc=0.;
    fIntPDFKaonCDA += pdfkc*pass_cda/0.25;
    //    if (fIntPDFKaonCDA[jD]>=1) fIntPDFKaonCDA[jD] = 1;
    fPDFKaonCDA.push_back(fIntPDFKaonCDA);
    double pdfpc = fCDA_p->Eval(cda);
    if(cda>25. || pdfpc<0.) pdfpc=0.;
    fIntPDFPileupCDA += pdfpc*pass_cda/0.25;
    //    if (fIntPDFPileCDA[jD]>=1) fIntPDFPileCDA[jD] = 1;
    fPDFPileupCDA.push_back(fIntPDFPileupCDA);
  };
  fPDFKaonDT1.clear();
  fIntPDFKaonDT1 = 0.;
  fPDFKaonDT2.clear();
  fIntPDFKaonDT2 = 0.;
  fPDFPileupDT.clear();
  fIntPDFPileupDT = 0.;
  for (int jbindt=0; jbindt<1000; jbindt++) { // +-1 ns
    double dt = pass_dt*jbindt+0.5*pass_dt;
    fIntPDFKaonDT1 += (fDT1->Eval(dt)+fDT1->Eval(-dt))*pass_dt/0.01;
    fPDFKaonDT1.push_back(fIntPDFKaonDT1);
    fIntPDFKaonDT2 += (fDT2->Eval(dt)+fDT2->Eval(-dt))*pass_dt/0.01;
    fPDFKaonDT2.push_back(fIntPDFKaonDT2);
    fIntPDFPileupDT += (fDT_p->Eval(dt)+fDT_p->Eval(-dt))*pass_dt/0.01;
    fPDFPileupDT.push_back(fIntPDFPileupDT);
  };
}

void MatchingRG::EvaluateDiscriminant(double cda_val, double dt1_val, double dt2_val, double &NnormCDA, double &NnormDT1, double &NnormDT2, double &NnormCDA_p, double &NnormDT_p, double &NintCDA, double &NintDT1, double &NintDT2, double &NintCDA_p, double &NintDT_p, double &pCDA, double &pDT1, double &pDT2, double &pCDA_p, double &pDT_p){

  if (cda_val>59.9 || fabs(dt1_val)>0.95 || fabs(dt2_val)>0.95) return;

  double pass_cda = 0.01; // mm
  double pass_dt = 0.001;//0.05; // ns

  double pkcda = 0.;
  vector<double>::iterator ikcda = fPDFKaonCDA.begin();
  while(ikcda!=fPDFKaonCDA.end()) {
    double cda = (double)(distance(fPDFKaonCDA.begin(),ikcda)-1)*pass_cda;
    if ((pkcda=EvaluateCondition(cda,cda_val,*ikcda))>=0) break;
    ++ikcda;
  }
  NintCDA = pkcda;
  NnormCDA = fIntPDFKaonCDA;
  pCDA = pkcda/fIntPDFKaonCDA;

  double pkdt1 = 0.;
  vector<double>::iterator ikdt1 = fPDFKaonDT1.begin();
  while(ikdt1!=fPDFKaonDT1.end()) {
    double dt1 = (double)(distance(fPDFKaonDT1.begin(),ikdt1)-1)*pass_dt;
    if ((pkdt1=EvaluateCondition(fabs(dt1),fabs(dt1_val),*ikdt1))>=0) break;
    ++ikdt1;
  }
  NintDT1 = pkdt1;
  NnormDT1 = fIntPDFKaonDT1;
  pDT1 = pkdt1/fIntPDFKaonDT1;

  double pkdt2 = 0.;
  vector<double>::iterator ikdt2 = fPDFKaonDT2.begin();
  while(ikdt2!=fPDFKaonDT2.end()) {
    double dt2 = (double)(distance(fPDFKaonDT2.begin(),ikdt2)-1)*pass_dt;
    if ((pkdt2=EvaluateCondition(fabs(dt2),fabs(dt2_val),*ikdt2))>=0) break;
    ++ikdt2;
  }
  NintDT2 = pkdt2;
  NnormDT2 = fIntPDFKaonDT2;
  pDT2 = pkdt2/fIntPDFKaonDT2;

  double ppcda = 0.;
  vector<double>::iterator ipcda = fPDFPileupCDA.begin();
  while(ipcda!=fPDFPileupCDA.end()) {
    double cda = (double)(distance(fPDFPileupCDA.begin(),ipcda)-1)*pass_cda;
    if ((ppcda=EvaluateCondition(cda,cda_val,*ipcda))>=0) break;
    ++ipcda;
  }
  NintCDA_p = ppcda;
  NnormCDA_p = fIntPDFPileupCDA;
  pCDA_p = ppcda/fIntPDFPileupCDA;

  double ppdt = 0.;
  vector<double>::iterator ipdt = fPDFPileupDT.begin();
  while(ipdt!=fPDFPileupDT.end()) {
    double dt = (double)(distance(fPDFPileupDT.begin(),ipdt)-1)*pass_dt;
    if ((ppdt=EvaluateCondition(fabs(dt),fabs(dt1_val),*ipdt))>=0) break;
    ++ipdt;
  }
  NintDT_p = ppdt;
  NnormDT_p = fIntPDFPileupDT;
  pDT_p = ppdt/fIntPDFPileupDT;

  return;
}

double MatchingRG::EvaluateCondition(double var, double cut, double integral){
  if(var>cut){
    return integral;
  }else{
    return -1.;
  };
}
