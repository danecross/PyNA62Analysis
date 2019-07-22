/// ---------------------------------------------------------
// History:
//
// Created by Michele Corvino (michele.corvino@cern.ch) 2019-03-20
//
// ---------------------------------------------------------


/// \class LKrMatching
/// \Brief
/// Tool to find LKr hits or candidate that are in time wrt a reference, even in case of jitters.
//  The class should be called using PhotonVetoHandler, like LAVMatching and SAVMatching.
/// \EndBrief
/// \Detailed
/// At each Start of Run, PhotonVetoHandler sets the list of burst with jitters and their info (crate, slot, time), 
//  reading the corresponding report file. 
//  If any jitter is present, its time is rounded to the closest multiple of 25 ns and stored.
//  In their main analyzers, users can call this class, giving to it a reference time and the desired time cuts.
//  Please note that the function SetTimeWindows requires 4 attributes, which have to be intended as signed numbers!
//  The function IsInTime will return true if a hit or candidate is inside the time window with respect to the reference time; 
//  in case of jitters, the jitter time will be subtracted to the LKr hit (candidate) if its position (seed position) is inside the affected CREAM.
//  It is possible to set two different time windows for the two cases.
//  This class should be use as follows:
//  \code
//  #include "LKrMatching.hh"
//
//  LKrMatching * fLKrMatching = *(LKrMatching**)GetOutput("PhotonVetoHandler.LKrMatching");
//  fLKrMatching->SetRefTime(timeref);
//  fLKrMatching->SetTimeWindows(-5,5,-5,5);
//  bool HitInTime = fLKrMatching->IsInTime(fHit);
//
//  \endcode
/// \author Michele Corvino (michele.corvino@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "LKrMatching.hh"
using namespace std;


LKrMatching::LKrMatching(){
}

LKrMatching::~LKrMatching(){
  fLKrJitters.clear();
}

void LKrMatching::SetTimeWindows(double t1, double t2, double t3, double t4){
  if(t1==t2 || t3==t4){
    cout<<"[LKrMatching] Invalid time windows!"<<endl;
    SetInvalidTimeCuts(false);
    return;
  }
  fMinTimeCut = t1;
  fMaxTimeCut = t2;
  fMinTimeCutJitter = t3;
  fMaxTimeCutJitter = t4;
}


bool LKrMatching::IsInTime(TRecoLKrHit *hit){
  if(!AreValidTimeCuts()) return false;
  if((hit->GetTime()-fRefTime)>fMinTimeCut && (hit->GetTime()-fRefTime)<fMaxTimeCut) return true;
  double offset;
  if(IsInCREAMWithJitter(hit, offset)){
    if((hit->GetTime()-fRefTime-offset)>fMinTimeCutJitter && (hit->GetTime()-fRefTime-offset)<fMaxTimeCutJitter) {
      return true;
    }
  }
  return false;
}

bool LKrMatching::IsInTime(TRecoLKrCandidate *candi){
  if(!AreValidTimeCuts()) return false;
  // Find the cluster seed 
  double offset;
  int iSeed = 0;
  int maxEnergy = 0;
  for(int iHit=0; candi->GetNHits(); iHit++){
    TRecoLKrHit *tempHit = static_cast<TRecoLKrHit*>(candi->GetHit(iHit));
    if(tempHit->GetEnergy() > maxEnergy){
      iSeed = iHit;
      maxEnergy = tempHit->GetEnergy();
    }
  }
  TRecoLKrHit *hit = static_cast<TRecoLKrHit*> (candi->GetHit(iSeed));
  if((candi->GetTime()-fRefTime)>fMinTimeCut && (candi->GetTime()-fRefTime)<fMaxTimeCut) return true;
  if(IsInCREAMWithJitter(hit, offset)){
    if((candi->GetTime()-fRefTime-offset)>fMinTimeCutJitter && (candi->GetTime()-fRefTime-offset)<fMaxTimeCutJitter) return true;
  }
  return false;
}

int LKrMatching::GetIx(int crate, int slot, int channel){
  int progressiveslot = slot<11 ? slot-3 : slot-5;
  return (int(crate)/4)*16 + (progressiveslot%4)*4 + int(channel/8);
}

int LKrMatching::GetIy(int crate, int slot, int channel){
  int progressiveslot = slot<11 ? slot-3 : slot-5;
  return 127 - ((crate%4)*32 + int(progressiveslot/4)*8 + channel%8);
}

bool LKrMatching::IsInCREAMWithJitter(TRecoLKrHit *hit, double &JitterTime){
  for(unsigned int iJitt=0; iJitt<fLKrJitters.size(); iJitt++){
    int MinXID = GetIx(fLKrJitters.at(iJitt).Crate, fLKrJitters.at(iJitt).Slot,7);
    int MaxXID = GetIx(fLKrJitters.at(iJitt).Crate, fLKrJitters.at(iJitt).Slot,24);
    int MinYID = GetIy(fLKrJitters.at(iJitt).Crate, fLKrJitters.at(iJitt).Slot,7);
    int MaxYID = GetIy(fLKrJitters.at(iJitt).Crate, fLKrJitters.at(iJitt).Slot,24);
    if(hit->GetXCellID()<MinXID || hit->GetXCellID()>MaxXID) return false;
    if(hit->GetYCellID()<MinYID || hit->GetYCellID()>MaxYID) return false;
    JitterTime = fLKrJitters.at(iJitt).TimeSlotsShift*ClockPeriod;
    return true;
  }
  return false;
}

void LKrMatching::SetJitter(int Run, int Burst, int Crate, int Slot, int Time){
  Jitter NewJitter;
  NewJitter.RunID = Run;
  NewJitter.BurstID = Burst;
  NewJitter.Crate = Crate;
  NewJitter.Slot = Slot;
  NewJitter.TimeSlotsShift = Time; 
  fLKrJitters.push_back(NewJitter);
}

void LKrMatching::SetCurrentRunAndBurstID(int Run, int Burst){
  fRunID = Run;
  fBurstID = Burst;
}

bool LKrMatching::LKrHasTimeMatching(TRecoLKrEvent *event, double EnergyThreshold){   //Pay attention: Energy Threshold is in MeV
  double EnergySum = 0;
  for(int iHit=0; iHit<event->GetNHits();iHit++){
    TRecoLKrHit *fHit = static_cast<TRecoLKrHit*> (event->GetHit(iHit));
    if(IsInTime(fHit)) EnergySum+=fHit->GetEnergy();
  }
  return (EnergySum>EnergyThreshold);
}
