/// ---------------------------------------------------------
// History:
//
// Created by Michele Corvino (michele.corvino@cern.ch) 2019-03-11
//
// ---------------------------------------------------------

#ifndef LKRMATCHING_HH
#define LKRMATCHING_HH

#include <stdlib.h>
#include <vector>
#include "Persistency.hh"

struct Jitter{
  int RunID;
  int BurstID;
  int Crate;
  int Slot;
  int TimeSlotsShift;
};

class TH1I;
class TH2F;
class TGraph;
class TTree;


class LKrMatching {
  public:
    LKrMatching();
    ~LKrMatching();
    void SetTimeWindows(double t1, double t2, double t3, double t4);
    void SetRefTime(double val){ fRefTime = val;}
    int GetIx(int crate, int slot, int channel);
    int GetIy(int crate, int slot, int channel);
    bool IsInTime(TRecoLKrHit *hit);
    bool IsInTime(TRecoLKrCandidate *candi);
    bool HasHitsInTime();
    bool HasClustersInTime();
    bool IsInCREAMWithJitter(TRecoLKrHit *hit, double &JitterTime);
    int  GetNJitters(){ return fLKrJitters.size();}
    void ClearJitters(){ fLKrJitters.clear();}
    void SetJitter(int Run, int Burst, int Crate, int Slot, int Time);
    void SetCurrentRunAndBurstID(int Run, int Burst);
    void SetInvalidTimeCuts(bool val){ fValidTimeCuts = val;}
    bool AreValidTimeCuts(){ return fValidTimeCuts;}
    bool LKrHasTimeMatching(TRecoLKrEvent *event, double EnergyThreshold); // returns true if the sum of energy in time is above a threshold
  private:
    bool fValidTimeCuts = true;
    int fRunID = 999;
    int fBurstID = 999;
    double fMinTimeCut = 999;
    double fMaxTimeCut = 999;
    double fMinTimeCutJitter = 999;
    double fMaxTimeCutJitter = 999;
    double fRefTime = -999;
    std::vector<Jitter> fLKrJitters;

};
#endif
