// ---------------------------------------------------------
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-05-13
//
// ---------------------------------------------------------
/// \class PnnKinematicTailsFunctions
/// \Brief
/// Tools for PnnKinematicTails.
/// \EndBrief
/// \Detailed
/// Tools and functions used in PnnKinematicTails analysers.
///
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <bitset>
#include <TChain.h>
#include "BeamParameters.hh"
#include "RICHParameters.hh"
#include "GeometricAcceptance.hh"
#include "TriggerConditions.hh"
#include "BlueTubeTracker.hh"
#include "VertexLSF.hh"
#include "TwoLinesCDA.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

bool EvaluateLKrCluster(double Eclus, double deltaT, double sigma, double LKrClusterEnergy1, double LKrClusterEnergy2, double LKrClusterEnergy3, double LKrClusterEnergy4, double LKrClusterTimeDiff1, double LKrClusterTimeDiff2, double LKrClusterTimeDiffSigma1, double LKrClusterTimeDiffSigma2, double LKrClusterTimeDiffSigma3, bool verb){
  bool photonInLKr = false;
  if(verb){
    cout<<"cluster energy: "<<Eclus<<" < "<<LKrClusterEnergy1<<" && "<<"track-cluster time diff: "<<fabs(deltaT)<<" < "<<LKrClusterTimeDiff1<<endl;
    cout<<"OR"<<endl;
    cout<<"cluster energy: "<<LKrClusterEnergy1<<" <= "<<Eclus<<" < "<<LKrClusterEnergy2<<" && "<<"track-cluster time diff: "<<fabs(deltaT)<<" < "<<LKrClusterTimeDiffSigma1*sigma<<endl;
    cout<<"OR"<<endl;
    cout<<"cluster energy: "<<LKrClusterEnergy2<<" <= "<<Eclus<<" < "<<LKrClusterEnergy4<<" && "<<"track-cluster time diff: "<<fabs(deltaT)<<" < "<<LKrClusterTimeDiffSigma2*sigma<<endl;
    cout<<"OR"<<endl;
    cout<<"cluster energy: "<<LKrClusterEnergy4<<" <= "<<Eclus<<" && "<<"track-cluster time diff: "<<fabs(deltaT)<<" < "<<LKrClusterTimeDiffSigma3*sigma<<endl;
    cout<<"OR"<<endl;
    cout<<"cluster energy: "<<Eclus<<" > "<<LKrClusterEnergy3<<" && ("<<"track-cluster time diff + 25ns: "<<fabs(deltaT+25.)<<" < "<<LKrClusterTimeDiff2<<" || "<<"track-cluster time diff - 25ns: "<<fabs(deltaT-25.)<<" < "<<LKrClusterTimeDiff2<<" ) "<<endl;
    cout<<"THEN activity in LKr"<<endl;
  };
  if((Eclus<LKrClusterEnergy1 && fabs(deltaT)<LKrClusterTimeDiff1) ||
     (Eclus>=LKrClusterEnergy1 && Eclus<LKrClusterEnergy2 && fabs(deltaT)<LKrClusterTimeDiffSigma1*sigma) ||
     (Eclus>=LKrClusterEnergy2 && Eclus<LKrClusterEnergy4 && fabs(deltaT)<LKrClusterTimeDiffSigma2*sigma) ||
     (Eclus>=LKrClusterEnergy4 && fabs(deltaT)<LKrClusterTimeDiffSigma3*sigma) ||
     // (Eclus>fLKrClusterEnergy2 && clusPos.X()>fLKrClusterMinPosX && clusPos.X()<fLKrClusterMaxPosX && fabs(clusPos.Y())<fLKrClusterMaxPosY && fabs(deltaT+36.)<fLKrClusterTimeDiff2) || //only for 2016 data
     (Eclus>LKrClusterEnergy3 && (fabs(deltaT-25.)<LKrClusterTimeDiff2 || fabs(deltaT+25.)<LKrClusterTimeDiff2))){
    photonInLKr = true;
  };
  return photonInLKr;
}

bool isPositronEoP(double mom, double energy, double cut, bool verb){
  bool is = false;

  double EoP = energy/mom;
  if(verb) cout<<"EoP: "<<EoP<<" > "<<cut<<" to be positron."<<endl;
  if(EoP>=cut) is = true;

  if(verb) cout<<"Is "<<(is?"":"not")<<" positron."<<endl;
  return is;
}

bool isMuonMUV3Candidates(TRecoMUV3Event *MUV3Event, double trackTime, double cut, bool verb){
  bool is = false;
  if(verb) cout<<"track time = "<<trackTime<<endl;
  for(int i=0; i<MUV3Event->GetNCandidates(); i++){
    TRecoMUV3Candidate *MUV3cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(i));
    if(verb){
        cout<<"MUV3 cand time = "<<MUV3cand->GetTime()<<endl;
        cout<<"deltaT = "<<fabs(MUV3cand->GetTime() - trackTime)<<" < "<<cut<<" to be muon."<<endl;
    }
    if(fabs(MUV3cand->GetTime() - trackTime)<cut){
      is = true;
      break;
    };
  };
  if(verb) cout<<"Is "<<(is?"":"not")<<" muon by MUV3 candidates."<<endl;
  return is;
}

bool isMuonMUV3Associations(SpectrometerMUV3AssociationOutput *muv3asso, double trackTime, double cut, bool verb){
  bool is = false;
  if(verb) cout<<"track time = "<<trackTime<<endl;
  for(int i=0; i<muv3asso->GetNAssociationRecords(); i++){
    SpectrometerMUV3AssociationRecord *rec = static_cast<SpectrometerMUV3AssociationRecord*>(muv3asso->GetAssociationRecord(i));
    if(verb) {
        cout<<"MUV3 rec time = "<<rec->GetMuonTime()<<endl;
        cout<<"deltaT = "<<fabs(rec->GetMuonTime() - trackTime)<<" < "<<cut<<" to be muon."<<endl;
    }
    if(fabs(rec->GetMuonTime() - trackTime)<cut){
      is = true;
      break;
    };
  };

  if(verb) cout<<"Is "<<(is?"":"not")<<" muon by MUV3 associations."<<endl;
  return is;
}

bool isMuonProbability(double probability, double cut, bool verb){
  bool is = false;
  if(verb) cout<<"Muon probability: "<<probability<<" > "<<cut<<" to be muon."<<endl;
  if(probability>cut){ //Rado suggested to try this function
    is = true;
  };
  if(verb) cout<<"Is "<<(is?"":"not")<<" muon by probability."<<endl;
  return is;
}

bool isMuonMIP(double dmip, double cut, bool verb){
 bool is = false;
  if(verb) cout<<"Muon dMIP: "<<dmip<<" < "<<cut<<" to be muon."<<endl;
  if(dmip<cut){ //Rado suggested to try this function
    is = true;
  };
  if(verb) cout<<"Is "<<(is?"":"not")<<" muon by dMIP."<<endl;
  return is;
}

bool isPionProbability(double mom, double probability, bool verb){
  bool is = false;
  double cutPiProb = 0.98-0.4596*TMath::Exp(-(mom/1000. - 11.44)/5.27);
  if(verb) cout<<"Cut on pion probability = "<<cutPiProb<<endl;
  if(cutPiProb<0.7) cutPiProb = 0.7;
  if(verb) cout<<"Pion probability: "<<probability<<" >= "<<cutPiProb<<" to be pion."<<endl;
  if(probability>=cutPiProb){ //Rado suggested to try this function
    is = true;
  };
  if(verb) cout<<"Is "<<(is?"":"not")<<" pion by probability."<<endl;
  return is;
}

bool isPionCaloEnergy(double total, double mom, bool verb){
  bool is = false;
  if(verb) cout<<"total energy = "<<total<<" <= "<<1.2*mom<<endl;
  if(total<=1.2*mom){
    if(verb) cout<<"Total energy vs mom is true."<<endl;
    is = true;
  };
   if(verb) cout<<"Is "<<(is?"":"not")<<" pion by calorimetric energy."<<endl;
  return is;
}

bool isExtraCaloEnergy(double extraMUV1, double extraMUV2, double cutExtra, bool verb){
  bool is = false;
  if((extraMUV1 + extraMUV2)>=cutExtra){
    if(verb) cout<<"Extra MUV1/2 energy is true."<<endl;
    is = true;
  };
  if(verb) cout<<"Does "<<(is?"":"not")<<" have extra calorimetric energy."<<endl;
  return is;
}

bool isPionCellSeed(double MUV1Energy, double MUV2Energy, double TotalEnergy, double LKrSeedEnergy, double LKrEnergy, double LKrNCells, bool verb){
  bool is = true;
  bool is1 = true;
  bool is2 = true;
  bool is3 = true;
  double R1 = MUV1Energy/TotalEnergy;
  double R2 = MUV2Energy/TotalEnergy;
  double Rseed = LKrSeedEnergy/LKrEnergy;
  double Rcell = LKrNCells/LKrEnergy;

  if(verb){
    cout<<"1. R1/R2 condition to be pion:"<<endl;
    cout<<"R1 = "<<R1<<" >= 0.01 || R2 = "<<R2<<" >= 0.01"<<endl;
    cout<<"or:"<<endl;
    cout<<"R seed = "<<Rseed<<" < 0.2 || R cell = "<<Rcell<<" > 0.003"<<endl;
    cout<<"and"<<endl;
    cout<<"R seed = "<<Rseed<<" > 0.2 || R cell = "<<Rcell<<" >= 0.0018"<<endl;
    cout<<"and"<<endl;
    cout<<"R seed = "<<Rseed<<" <= 0.35"<<endl;
    cout<<"and"<<endl;
    cout<<"R seed = "<<Rseed<<" >= 0.05"<<endl;
  };
  if(R1<0.01 && R2<0.01){
    if((Rseed>0.2 && Rcell<=0.003) || (Rseed<=0.2 && Rcell<0.0018) || (Rseed>0.35) || (Rseed<0.05)){
      if(verb) cout<<"1.(3) R1/R2 condition is false."<<endl;
      is1 = false;
    };
  };

  if(verb) {
      cout<<"2. Rseed/Rcell condition to be pion:"<<endl;
      cout<<"Rseed = "<<Rseed<<" <= 0. || Rseed = "<<Rseed<<" >= 0.8 || Rcell = "<<Rcell<<" >= 0.0014"<<endl;
  }
  if((Rseed>0.) && (Rseed<0.8) && (Rcell<0.0014)){
    if(verb) cout<<"2.(3) Rseed/Rcell condition is false."<<endl;
    is2 = false;
  };
  if(verb) {
      cout<<"3. MUV1/2 energy condition to be pion: "<<endl;
      cout<<"MUV1Energy = "<<MUV1Energy<<" > 0. || MUV2Energy = "<<MUV2Energy<<" == 0."<<endl;
  }
  if(MUV1Energy==0. && MUV2Energy>0.){
    if(verb) cout<<"3.(3) MUV1/2 energy is false."<<endl;
    is3 = false;
  };

  if(!is1 || !is2 || !is3) is = false;

  if(verb) cout<<"Is "<<(is?"":"not")<<" pion by MUV1/2 energy and Cell and Seed ratios."<<endl;
  return is;
}

bool isPionRICH(SpectrometerRICHAssociationOutput *specRICH, SpectrometerRICHAssociationOutputSingleRing *specRICHsr, double minmass, double maxmass, double cutLikelihood, bool verb){
  bool is = false;
  bool is1 = false;
  bool is2 = false;
  double mass = specRICHsr->GetMass();
  double maxL = std::fmax(std::fmax(specRICH->GetLikelihood(0), specRICH->GetLikelihood(1)), std::fmax(specRICH->GetLikelihood(2), specRICH->GetLikelihood(4))); //Rado suggested this new method
  if(verb) cout<<"RICH mass:"<<minmass<<" < "<<mass<<" < "<<maxmass<<" to be pion."<<endl;
  if(mass>minmass && mass<maxmass) is1 = true;
  if(verb) cout<<"Max likelihood: "<<maxL<<" <= "<<cutLikelihood<<" to be pion."<<endl;
  if(maxL<=cutLikelihood) is2 = true;

  if(is1 && is2) is = true;
  if(verb) cout<<"Is "<<(is?"":"not")<<" pion by RICH."<<endl;
  return is;
}

void ApplyBlueTube(int charge, TVector3 oldPos, TVector3 oldMom, double finalZ, TVector3 *newPos, TVector3 *newMom){
  BlueTubeTracker *tracker = BlueTubeTracker::GetInstance();
  tracker->SetCharge(charge);
  tracker->SetInitialPosition(oldPos);
  tracker->SetInitialMomentum(oldMom);
  tracker->SetZFinal(finalZ);
  tracker->TrackParticle();
  TVector3 mom = tracker->GetFinalMomentum();
  TVector3 pos = tracker->GetFinalPosition();
  newPos->SetXYZ(pos.X(), pos.Y(), pos.Z());
  newMom->SetXYZ(mom.X(), mom.Y(), mom.Z());
}

TVector3 GetPositionAtZ(TVector3 mom, TVector3 oldPos, double newZ){
  double MNP33kick = 270.;
  if(((newZ<=197600.)&&(oldPos.Z()<=197600.)) || ((newZ>=197600.)&&(oldPos.Z()>=197600.))){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - newZ);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - newZ);
    TVector3 newPos(xAtZ, yAtZ, newZ);
    return newPos;
  }else if((newZ>197600.)&&(oldPos.Z()<197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, MNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else if((newZ<197600.)&&(oldPos.Z()>197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, -MNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else{
    cout<<"cannot calculate the position"<<endl;
    return oldPos;
  };
}

TVector3 MomAfterKick(TVector3 oldMom, double kick){
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

double momRICH(SpectrometerRICHAssociationOutputSingleRing *SpecRICHsr, int RunNumber, time_t BurstTime){
  double FLength = 17020.;
  double Relectron = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, BurstTime);

  double RICHmom = MPI*FLength/sqrt(pow(Relectron, 2) - pow(SpecRICHsr->GetRingRadius(), 2));
  return RICHmom;
}

TVector3 GetVertexCDA(TVector3 trackMom, TVector3 trackPos, TVector3 kaonMom, TVector3 kaonPos){
  TwoLinesCDA twoLinesCDA;

  twoLinesCDA.SetLine1Point1(trackPos);
  twoLinesCDA.SetDir1(trackMom);
  twoLinesCDA.SetLine2Point1(kaonPos);
  twoLinesCDA.SetDir2(kaonMom);
  twoLinesCDA.ComputeVertexCDA();
  TVector3 vertex = twoLinesCDA.GetVertex();

  return vertex;
}

TVector3 GetSimpleVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  TVector3 nul(0., 0., 0.);
  TVector3 simpleVert = GetVertexCDA(trackMom, trackPos, kaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

TVector3 GetIterativeVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos, double dist, bool verb){

  int count = 0;
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;

  TVector3 simpleVert;
  TVector3 nul(0., 0., 0.);
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());
  if(verb){
    cout<<"kaon momentum "<<newKaonMom->X()<<" "<<newKaonMom->Y()<<" "<<newKaonMom->Z()<<endl;
    cout<<"track momentum "<<newTrackMom->X()<<" "<<newTrackMom->Y()<<" "<<newTrackMom->Z()<<endl;
    cout<<"kaon pos = "<<newKaonPos->X()<<" "<<newKaonPos->Y()<<" "<<newKaonPos->Z()<<endl;
    cout<<"track pos = "<<newTrackPos->X()<<" "<<newTrackPos->Y()<<" "<<newTrackPos->Z()<<endl;
  };
  while(fabs((*newKaonPos).Z() - (*newTrackPos).Z())>dist){
    if(verb) cout<<"count = "<<count<<endl;
    simpleVert = GetVertexCDA(*newTrackMom, *newTrackPos, *newKaonMom, *newKaonPos);
    if(verb) cout<<"vertex = "<<simpleVert.Z()<<endl;
    if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
    double step = ((simpleVert.Z() - (*newKaonPos).Z())/2.);
    if(verb){
      cout<<"kaon step = "<<step<<endl;
      cout<<"new kaon z = "<<step + (*newKaonPos).Z()<<endl;
    };
    ApplyBlueTube(kaonCharge, *newKaonPos, *newKaonMom, step + (*newKaonPos).Z(), newKaonPos, newKaonMom);
    step = ((-simpleVert.Z() + (*newTrackPos).Z())/2.);
    if(verb){
      cout<<"track step = "<<step<<endl;
      cout<<"new track z = "<<(*newTrackPos).Z()-step<<endl;
    };
    ApplyBlueTube(trackCharge, *newTrackPos, *newTrackMom, (*newTrackPos).Z() - step, newTrackPos, newTrackMom);
    if(verb){
      cout<<"new kaon momentum "<<newKaonMom->X()<<" "<<newKaonMom->Y()<<" "<<newKaonMom->Z()<<endl;
      cout<<"new track momentum "<<newTrackMom->X()<<" "<<newTrackMom->Y()<<" "<<newTrackMom->Z()<<endl;
      cout<<"new kaon pos = "<<newKaonPos->X()<<" "<<newKaonPos->Y()<<" "<<newKaonPos->Z()<<endl;
      cout<<"new track pos = "<<newTrackPos->X()<<" "<<newTrackPos->Y()<<" "<<newTrackPos->Z()<<endl;
    };
    count++;
    if(count>100) break;
  };
  simpleVert = GetVertexCDA(*newTrackMom, *newTrackPos, *newKaonMom, *newKaonPos);
  if(verb) cout<<"final vertex = "<<simpleVert.Z()<<endl;
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);
  if(verb){
    cout<<"final kaon momentum "<<newKaonMom->X()<<" "<<newKaonMom->Y()<<" "<<newKaonMom->Z()<<endl;
    cout<<"final track momentum "<<newTrackMom->X()<<" "<<newTrackMom->Y()<<" "<<newTrackMom->Z()<<endl;
    cout<<"final kaon pos = "<<newKaonPos->X()<<" "<<newKaonPos->Y()<<" "<<newKaonPos->Z()<<endl;
    cout<<"final track pos = "<<newTrackPos->X()<<" "<<newTrackPos->Y()<<" "<<newTrackPos->Z()<<endl;
  };
  return simpleVert;
}

TVector3 GetRadoVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  TVector3 simpleVert;
  TVector3 somePos;
  TVector3 nul(0., 0., 0.);
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());

  simpleVert = GetVertexCDA(trackMom, trackPos, kaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), &somePos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), &somePos, newTrackMom);
  simpleVert = GetVertexCDA(*newTrackMom, trackPos, *newKaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

TVector3 GetLSFVertex(double trackCharge, TRecoSpectrometerCandidate *STRAWCand, double kaonCharge, TRecoGigaTrackerCandidate *GTKCand, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos, double &chi2){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  VertexLSF vertexLSF;
  vertexLSF.Reset();

  vertexLSF.AddTrack(STRAWCand);
  vertexLSF.AddGTKTrack(GTKCand);

  TVector3 nul(0., 0., 0.);
  bool goodvertex = vertexLSF.FitVertex(true);
  if(!goodvertex) return nul;

  TVector3 kaonMom = GTKCand->GetMomentum();
  TVector3 kaonPos = GTKCand->GetPosition(2);
  TVector3 trackMom = STRAWCand->GetThreeMomentumBeforeMagnet();
  TVector3 trackPos = STRAWCand->GetPositionBeforeMagnet();
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());

  TVector3 simpleVert = vertexLSF.GetVertexPosition();
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  chi2 = vertexLSF.GetChi2();
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

double GetCDA(TRecoSpectrometerCandidate* STRAWCand1, TRecoSpectrometerCandidate* STRAWCand2){
  TwoLinesCDA twoLinesCDA;

  twoLinesCDA = TwoLinesCDA();
  twoLinesCDA.SetLine1Point1(STRAWCand1->GetPositionBeforeMagnet());
  twoLinesCDA.SetDir1(STRAWCand1->GetThreeMomentumBeforeMagnet());
  twoLinesCDA.SetLine2Point1(STRAWCand2->GetPositionBeforeMagnet());
  twoLinesCDA.SetDir2(STRAWCand2->GetThreeMomentumBeforeMagnet());
  twoLinesCDA.ComputeVertexCDA();
  double CDA = twoLinesCDA.GetCDA();
  return CDA;
}

double GetCDA(TVector3 trackMom, TVector3 trackPos, TVector3 kaonMom, TVector3 kaonPos){
  TwoLinesCDA twoLinesCDA;

  twoLinesCDA.SetLine1Point1(trackPos);
  twoLinesCDA.SetDir1(trackMom);
  twoLinesCDA.SetLine2Point1(kaonPos);
  twoLinesCDA.SetDir2(kaonMom);
  twoLinesCDA.ComputeVertexCDA();
  return twoLinesCDA.GetCDA();
}

double CorrectLKrCellCluster(int ncells, double energy){
  energy/=1000.;
  double finalE = energy;
  if(ncells>9){
    if(energy<22.){
      finalE = energy/(0.7666+0.0573489*log(energy));
    }else if(energy>=22. && energy<65.){
      finalE = energy/(0.828962+0.0369797*log(energy));
    }else{
      finalE = energy/(0.828962+0.0369797*log(65.));
    };
  };
  finalE *= 1.03;
  finalE = (finalE<15.) ? ((finalE+0.015)/15.015*15.*0.9999) : finalE;
  finalE *= 1000.;
  return finalE;
}

void SortCHODHits(TRecoCHODEvent *CHODEvent, std::array<std::vector<int>, 4> &Hhits, std::array<std::vector<int>, 4> &Vhits){
  for(int i=0; i<CHODEvent->GetNHits(); i++){
    TRecoCHODHit *CHODHit = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(i));
    if(CHODHit->GetPlaneID()==0){
      Vhits[CHODHit->GetQuadrantID()].push_back(i);
    }else if(CHODHit->GetPlaneID()==1){
      Hhits[CHODHit->GetQuadrantID()].push_back(i);
    };
  };
}

int WhichCHODQuadrant(TVector2 pos){
  int qID = -1;
  if(pos.X()>0. && pos.Y()>0.){
    qID = 3;
  }else if(pos.X()>0. && pos.Y()<0.){
    qID = 2;
  }else if(pos.X()<0. && pos.Y()>0.){
    qID = 0;
  }else if(pos.X()<0. && pos.Y()<0.){
    qID = 1;
  };
  return qID;
}

int NCHODSlabs(TRecoCHODEvent *CHODEvent, double tTrack, int id, double deltaT){
  TRecoCHODCandidate *c = static_cast<TRecoCHODCandidate*>(CHODEvent->GetCandidate(id));
  int *hits = c->GetHitsIndexes();
  int inTimeHits = 0;
  for(int i=0; i<CHODEvent->GetNHits(); i++){
    TRecoCHODHit* CHODHit = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(i));
    if(fabs(CHODHit->GetTime() - 7. - tTrack)>deltaT) continue;
    if(std::find(hits, hits+c->GetNHits(), i)!=hits+c->GetNHits()) continue;
    inTimeHits++;
  };
  return inTimeHits;
}

void ReadOffsets(double &offsetX, double &offsetY){
  TString s = "";
  s = "CHOD-Alignment.dat";
  if(NA62ConditionsService::GetInstance()->Open(s)!=kSuccess) exit(1);
  TString line;
  while(line.ReadLine(NA62ConditionsService::GetInstance()->Get(s))){
    if(line.BeginsWith("#")) continue;
    TObjArray *l = line.Tokenize(" ");
    offsetX = ((TObjString*)(l->At(0)))->GetString().Atof();
    offsetY = ((TObjString*)(l->At(3)))->GetString().Atof();
    delete l;
  };
  NA62ConditionsService::GetInstance()->Close(s);
  return;
}

void ReadSlewCorrections(double SlewSlope[128][16], double SlewConst[128][16], bool MC){
  TString s = "";
  s = "CHOD-SlewCorr.dat";
  if(NA62ConditionsService::GetInstance()->Open(s)!=kSuccess) exit(1);
  int lineI = 0;
  TString line;
  while(line.ReadLine(NA62ConditionsService::GetInstance()->Get(s))){
    if(line.BeginsWith("#")) continue;
    TObjArray *l = line.Tokenize(" ");
    if(l->GetEntries()>=2){
      SlewSlope[lineI/16][lineI%16] = (MC ? 0. : ((TObjString*)(l->At(0)))->GetString().Atof());
      SlewConst[lineI/16][lineI%16] = (MC ? 0. : ((TObjString*)(l->At(1)))->GetString().Atof());
      lineI++;
    };
    delete l;
  };
  if(lineI!=2048) std::cerr<<"MultiplicitySelection: Error while reading SlewingCorrections"<<std::endl;
  NA62ConditionsService::GetInstance()->Close(s);
  return;
}

void ReadLightVelocities(double *LV){
  TString s = "";
    s = "CHOD-LightVelocities.dat";
  if(NA62ConditionsService::GetInstance()->Open(s)!=kSuccess) return;
  int slabID = 0;
  TString line;
  while(line.ReadLine(NA62ConditionsService::GetInstance()->Get(s))){
    if(line.BeginsWith("#")) continue;
    LV[slabID] = line.Atof();
    slabID++;
  };
  if(slabID!=128) std::cerr<<"Error while reading LightVelocities"<<std::endl;
  NA62ConditionsService::GetInstance()->Close(s);
  return;
}

void ReadSlabCenters(double *SlabCenter){
  Double_t SlabLimitsX[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
			       -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
			       390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
  Double_t SlabLimitsY[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
			       -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
			       390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};

  for(Int_t i=0; i<128; i++){
    if(i<16){
      SlabCenter[i] = (SlabLimitsX[16-i-1] + SlabLimitsX[16-i])/2.;
    }else if(i<48){
      SlabCenter[i] = (SlabLimitsX[i-16] + SlabLimitsX[i-16+1])/2.;
    }else if(i<64){
      SlabCenter[i] = (SlabLimitsX[80-i] + SlabLimitsX[80-i-1])/2.;
    }else if(i<96){
      SlabCenter[i] = (SlabLimitsY[96-i] + SlabLimitsY[96-i-1])/2.;
    }else{
      SlabCenter[i] = (SlabLimitsY[i-96] + SlabLimitsY[i-96+1])/2.;
    };
  };
}

void ReadSlabWidths(double *SlabWidth){
  //calculate CHOD slab widths
  for(int i=0; i<64; i++){
    SlabWidth[i] = 65.0;
    if((i>=11 && i<=20) || (i>=43 && i<=52)){
      SlabWidth[i] = 99.;
    };
  };
  for(int i=64; i<128; i++){
    SlabWidth[i] = 99.0;
    if((i>=69 && i<=90) || (i>=101 && i<=122)){
      SlabWidth[i] = 65.;
    };
  };
}

void CorrectCHODHitsTime(TRecoCHODEvent *CHODEvent, int VID, int HID, double *LV, double *SlabCenter, double SlewSlope[128][16], double SlewConst[128][16], bool MC, double &timeVcorr, double &timeHcorr){
  TRecoCHODHit *hitV = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(VID));
  TRecoCHODHit *hitH = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(HID));

  int chIDV = hitV->GetChannelID();
  int chIDH = hitH->GetChannelID();
  double timeV = hitV->GetTime();
  double timeH = hitH->GetTime();
  double timeWidthV = hitV->GetTimeWidth();
  double timeWidthH = hitH->GetTimeWidth();

  double slewSlopeV = 0.;
  double slewSlopeH = 0.;
  double slewConstV = 0.;
  double slewConstH = 0.;

  slewSlopeV = SlewSlope[chIDV][(chIDH-64)%16];
  slewConstV = SlewConst[chIDV][(chIDH-64)%16];

  slewSlopeH = SlewSlope[chIDH][chIDV%16];
  slewConstH = SlewConst[chIDH][chIDV%16];

  double offtime = 0.;
  if(!MC && chIDH==79){
    if((timeWidthH-7.)<15.){
      timeWidthH -= 7.;
      offtime = 1.1;
    }else{
      timeWidthH = 15.;
    };
  };

  timeVcorr = timeV + LVCorrection(chIDV, chIDH, LV, SlabCenter);
  timeHcorr = timeH + offtime + LVCorrection(chIDH, chIDV, LV, SlabCenter);

  // cout<<"H lv correction "<<LVCorrection(chIDH, chIDV, LV, SlabCenter)<<endl;
  // cout<<"V lv correction "<<LVCorrection(chIDV, chIDH, LV, SlabCenter)<<endl;

  double effTOTH = 0.;
  double effTOTV = 0.;
  if(timeWidthV<15.){
    effTOTV = timeWidthV;
  }else{
    effTOTV = 15.;
  };
  if(timeWidthH<15.){
    effTOTH = timeWidthH;
  }else{
    effTOTH = 15.;
  };

  // cout<<"effTOTH "<<effTOTH<<endl;
  // cout<<"effTOTV "<<effTOTV<<endl;

  double scV = - slewSlopeV*effTOTV - slewConstV;
  double scH = - slewSlopeH*effTOTH - slewConstH;

  if(timeWidthV<=0.) scV = 0.;
  if(timeWidthH<=0.) scH = 0.;

  if(LVCorrection(chIDV, chIDH, LV, SlabCenter)==0.) scV = 0.;
  if(LVCorrection(chIDH, chIDV, LV, SlabCenter)==0.) scH = 0.;

  // cout<<"sc corr H "<<scH<<endl;
  // cout<<"sc corr V "<<scV<<endl;
  timeVcorr = timeVcorr + scV;
  timeHcorr = timeHcorr + scH;
}

double LVCorrection(int chID1, int chID2, double *LV, double *SlabCenter){
  if((chID1>=0) && (chID1<128) && (chID2>=0) && (chID2<128)){
    double PMCoordinate[128] = {1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				1191., 1126., 1061., 996., 897., 798., 699., 600.,
				-600., -699., -798., -897., -996., -1061., -1126., -1191.,
				-1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				-1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				-1191., -1126., -1061., -996., -897., -798., -699., -600.,
				600., 699., 798., 897., 996., 1061., 1126., 1191.,
				1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				-600., -699., -798., -897., -996., -1061., -1126., -1191.,
				-1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				-1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				-1191., -1126., -1061., -996., -897., -798., -699., -600.,
				600., 699., 798., 897., 996., 1061., 1126., 1191.,
				1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				1191., 1126., 1061., 996., 897., 798., 699., 600.};
    double a = -LV[chID1]*fabs(SlabCenter[chID2]-PMCoordinate[chID1]);
    return a;
  }else{
    return 0;
  };
}

