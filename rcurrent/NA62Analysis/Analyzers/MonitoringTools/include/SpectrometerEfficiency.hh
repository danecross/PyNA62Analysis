#ifndef SPECTROMETEREFFICIENCY_HH
#define SPECTROMETEREFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TH2D;
class TGraph;
class TTree;

class TRecoSpectrometerEvent;
class TRecoGigaTrackerEvent;

class SpectrometerEfficiency : public NA62Analysis::Analyzer {
  public:
    explicit SpectrometerEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
    ~SpectrometerEfficiency();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(int);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();

    double GetTriggerTime(int);
    void ApplyBlueTube(int, TVector3, TVector3, double, TVector3*, TVector3*);
    int FindClosestInPosition(TVector3, TVector3, TString, bool, bool);
    int FindClosestInMomentum(TVector3, TVector3, TString, bool, bool);
    bool IsCloseInMom(TVector3, TVector3, double, bool, TString);
    bool IsCloseInPos(TVector3, TVector3, double, double, double, bool, TString);
    TVector3 GetPositionAtZ(TVector3, TVector3, double);
    TVector3 MomAfterKick(TVector3, double);
    double GetLambdaGTK(double);
    double GetLowEdgeFirstFilledBin(TH1*);
    double GetUpperEdgeLastFilledBin(TH1*);
    double GetDistanceFromHoleCenter(double, TVector3, TVector3);
    void FillHistogramsForEfficiency(TLorentzVector, TVector3, TString);

    TRecoSpectrometerEvent *fSTRAWEvent;
    TRecoGigaTrackerEvent *fGTKEvent;
    Double_t fTriggerTime;
    Int_t fBurstID;

    //folders
    TString gen;
    TString nia;
    TString nc;
    TString nm;
    TString bef;
    TString mat;

    bool fNoCand;
    bool fNoMatch;

    //parameters
    double fCutMinClusterEnergy;
    bool fUseGTK;
    int fMaxNBursts;
    int fTrigger;
    double fCutTimeDifferenceForSidebands;
    bool fWantGeometricalAcceptance;
    double fMinDistanceWrtHoleCenter;
    bool fWantCloseInMom;
    bool fWantCloseInPos;
    double fCutCloseInMom;
    double fCutCloseInPosK;
    double fCutCloseInPosQ;
    double fMNP33kick;

  private:

    TString fBadBurstFileName;
    //pdf output
    bool fReadingData;
    TString fOutPDFfileName;
    TCanvas* fCanvas;
    TH1F* fhExpected_mom;
    TH1F* fhMatched_mom;
    TH1F* fhMatched_mom_4CH;
    TH2F* fhExpected_pos;
    TH2F* fhMatched_pos;
    TH2F* fhMatched_pos_4CH;
    TH2F* fhEfficiency_pos;
    TH1F* fhExpected_lambda;
    TH1F* fhMatched_lambda;
    TH1F* fhMatched_lambda_4CH;
    TH1F* fhExpected_burstID;
    TH1F* fhMatched_burstID;
    TH1F* fhMatched_burstID_4CH;
    TH1F* fhNoCand_mom;
    TH1F* fhNoMatch_mom;
    TH1F* fhMomDiff;
    TH2F* fhPosDiffVSMom;
    TH1F* fhExpected_disc;
    TH1F* fhMatched_disc;
    TH1F* fhMatched_disc_4CH;
    // Graphs storing burst quality , missing covers and straws per burst
    TGraph *fGBurstQuality, *fGMissingCovers, *fGMissingStraws;
};
#endif
