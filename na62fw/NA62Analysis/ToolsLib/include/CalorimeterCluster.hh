#ifndef CALORIMETERCLUSTER_HH
#define CALORIMETERCLUSTER_HH

#include "TObject.h"
#include "TString.h"
#include "TRecoLKrCandidate.hh"
#include "TRecoMUV1Candidate.hh"
#include "TRecoMUV2Candidate.hh"

class CalorimeterCluster : public TObject {

public:
  CalorimeterCluster ();
  ~CalorimeterCluster ();

  void Clear (Option_t *opt="");

  void SetTrackID(int ID) {fTrackID = ID;};

  void SetEnergy(double En){fEnergy = En;};
  void SetHadronicEnergy(double En){fHadronicEnergy = En;};
  void SetTime(double T){fTime = T;};

  void SetIsElectronOldProbability(double P){ fIsElectronOldProbability = P;};
  void SetIsMuonOldProbability    (double P){ fIsMuonOldProbability = P;};
  void SetIsPionOldProbability    (double P){ fIsPionOldProbability = P;};

  void SetIsElectronProbability(double P){ fIsElectronProbability = P;};
  void SetIsMuonProbability    (double P){ fIsMuonProbability = P;};
  void SetIsPionProbability    (double P){ fIsPionProbability = P;};

  void SetLKrCandidate  (TRecoLKrCandidate *cand) {fLKrCand=cand; fIsLKrMatch=true;};
  void SetMUV1Candidate (TRecoMUV1Candidate *cand){fMUV1Cand=cand; fIsMUV1Match=true;};
  void SetMUV2Candidate (TRecoMUV2Candidate *cand){fMUV2Cand=cand; fIsMUV2Match=true;};

  void SetLKrEnergy (double En){fLKrEnergy=En;};
  void SetLKrHadronicEnergy (double En){fLKrHadronicEnergy=En;};
  void SetMUV1Energy(double En){fMUV1Energy=En;};
  void SetMUV2Energy(double En){fMUV2Energy=En;};
  void SetOldMUV1Energy(double En){fOldMUV1Energy=En;};
  void SetOldMUV2Energy(double En){fOldMUV2Energy=En;};

  void SetLKrTime(double T){fLKrTime = T;};
  void SetMUV1Time(double T){fMUV1Time = T;};
  void SetMUV2Time(double T){fMUV2Time = T;};

  void SetLKrWeight  (double W){fLKrWeight=W;};
  void SetMUV1Weight (double W){fMUV1Weight=W;};
  void SetMUV2Weight (double W){fMUV2Weight=W;};
  void SetOldMUV1Weight (double W){fOldMUV1Weight=W;};
  void SetOldMUV2Weight (double W){fOldMUV2Weight=W;};

  void SetLKrEnergyCorrections (double En){fLKrEnergyCorrections=En;};
  void SetMUV1EnergyCorrections(double En){fMUV1EnergyCorrections=En;};
  void SetMUV2EnergyCorrections(double En){fMUV2EnergyCorrections=En;};

  void SetLKrOuterEnergy(double En){fLKrOuterEnergy=En;};
  void SetMUV1OuterEnergy(double En){fMUV1OuterEnergy=En;};
  void SetMUV1OuterNhits(int En){fMUV1OuterNhits=En;};
  void SetMUV1OuterPosition(double X, double Y){ fMUV1OuterPositionX = X; fMUV1OuterPositionY =Y; }
  void SetMUV2OuterEnergy(double En){fMUV2OuterEnergy=En;};
  void SetMUV2OuterNhits(int En){fMUV2OuterNhits=En;};

  void SetMUV1InTimeEnergy(double En){fMUV1InTimeEnergy=En;};
  void SetMUV2InTimeEnergy(double En){fMUV2InTimeEnergy=En;};

  void SetHACShowerWidth(double x){ fHACShowerWidth = x; }
  void SetHACWeight (double x){ fHACWeight = x; }
  void SetHACPosition(double x, double y){ fHACPosition.Set(x,y); }

  int GetTrackID (){ return fTrackID; };

  double GetEnergy(){ return fEnergy; };
  double GetHadronicEnergy(){ return fHadronicEnergy; };
  double GetTime(){ return fTime; };

  double GetIsElectronProbability(){ return fIsElectronProbability; };
  double GetIsMuonProbability(){ return fIsMuonProbability; };
  double GetIsPionProbability(){ return fIsPionProbability; };

  double GetIsElectronOldProbability(){ return fIsElectronOldProbability; };
  double GetIsMuonOldProbability(){ return fIsMuonOldProbability; };
  double GetIsPionOldProbability(){ return fIsPionOldProbability; };

  TRecoLKrCandidate * GetLKrCandidate (){return fLKrCand;};
  TRecoMUV1Candidate * GetMUV1Candidate(){return fMUV1Cand;};
  TRecoMUV2Candidate * GetMUV2Candidate(){return fMUV2Cand;};

  double GetLKrEnergy (){return fLKrEnergy;};
  double GetLKrHadronicEnergy (){return fLKrHadronicEnergy;};
  double GetMUV1Energy(){return fMUV1Energy;};
  double GetMUV2Energy(){return fMUV2Energy;};
  double GetOldMUV1Energy(){return fOldMUV1Energy;};
  double GetOldMUV2Energy(){return fOldMUV2Energy;};


  double GetLKrTime (){ return fLKrTime; };
  double GetMUV1Time(){ return fMUV1Time; };
  double GetMUV2Time(){ return fMUV2Time; };

  double GetLKrWeight  (){return fLKrWeight;};
  double GetMUV1Weight (){return fMUV1Weight;};
  double GetMUV2Weight (){return fMUV2Weight;};
  double GetOldMUV1Weight (){return fOldMUV1Weight;};
  double GetOldMUV2Weight (){return fOldMUV2Weight;};

  double GetLKrEnergyCorrections (){return fLKrEnergyCorrections;};
  double GetMUV1EnergyCorrections(){return fMUV1EnergyCorrections;};
  double GetMUV2EnergyCorrections(){return fMUV2EnergyCorrections;};

  double GetLKrOuterEnergy(){return fLKrOuterEnergy;};
  double GetMUV1OuterEnergy(){return fMUV1OuterEnergy;};
  int GetMUV1OuterNhits(){return fMUV1OuterNhits;};
  TVector2 GetMUV1OuterPosition(){return TVector2(fMUV1OuterPositionX,fMUV1OuterPositionY);};
  double GetMUV2OuterEnergy(){return fMUV2OuterEnergy;};
  int GetMUV2OuterNhits(){return fMUV2OuterNhits;};

  double GetMUV1InTimeEnergy(){return fMUV1InTimeEnergy;};
  double GetMUV2InTimeEnergy(){return fMUV2InTimeEnergy;};

  double GetHACShowerWidth(){ return fHACShowerWidth; }
  double GetHACWeight (){ return fHACWeight; }
  TVector2 GetHACPosition (){ return fHACPosition; }

  bool IsLKrAssociated (){ return fIsLKrMatch; };
  bool IsMUV1Associated (){ return fIsMUV1Match; };
  bool IsMUV2Associated (){ return fIsMUV2Match; };

private:

  int fTrackID;
  double fEnergy;
  double fHadronicEnergy;
  double fTime;
  double fIsElectronProbability;
  double fIsMuonProbability;
  double fIsPionProbability;
  double fIsElectronOldProbability;
  double fIsMuonOldProbability;
  double fIsPionOldProbability;

  TRecoLKrCandidate *fLKrCand;
  TRecoMUV1Candidate *fMUV1Cand;
  TRecoMUV2Candidate *fMUV2Cand;

  bool fIsLKrMatch;
  bool fIsMUV1Match;
  bool fIsMUV2Match;

  double fLKrEnergy;
  double fLKrHadronicEnergy;
  double fMUV1Energy;
  double fMUV2Energy;
  double fOldMUV1Energy;
  double fOldMUV2Energy;

  double fLKrTime;
  double fMUV1Time;
  double fMUV2Time;

  double fLKrWeight;
  double fMUV1Weight;
  double fMUV2Weight;
  double fOldMUV1Weight;
  double fOldMUV2Weight;

  double fLKrEnergyCorrections;
  double fMUV1EnergyCorrections;
  double fMUV2EnergyCorrections;

  double fLKrOuterEnergy;
  double fMUV1OuterEnergy;
  double fMUV2OuterEnergy;
  int    fMUV1OuterNhits;
  int    fMUV2OuterNhits;

  double fMUV1InTimeEnergy;
  double fMUV2InTimeEnergy;
  double fMUV1OuterPositionX,fMUV1OuterPositionY;

  double fHACWeight, fHACShowerWidth;
  TVector2 fHACPosition;

  ClassDef(CalorimeterCluster,1);
};
#endif
