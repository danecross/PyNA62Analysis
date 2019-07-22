#include <iostream>
#include <fstream>
#include <vector>
#include "TF1.h"
#include "TString.h"

#ifndef MUV12CHARGECORRECTIONS_HH
#define MUV12CHARGECORRECTIONS_HH
using namespace std;

class MUV12Corrections {

  MUV12Corrections();
  MUV12Corrections(int RunID, TString rev, bool IsMC=false);
  ~MUV12Corrections();

public:
  static MUV12Corrections* GetInstance();
  static MUV12Corrections* GetInstance(int RunID, TString rev, bool IsMC);


public:
  void NewRunID(int ID, TString rev,bool IsMC);

  double GetMUV1ChargeCorrection(int ChannelID, double position, bool WithEq=true);
  double GetMUV2ChargeCorrection(int ChannelID, double position, bool WithEq=true);

  double GetMUV1TimeCorrection(int ChannelID, double position);
  double GetMUV2TimeCorrection(int ChannelID, double position);

  int GetMUV1ChannelID (int CREAM, int ChID){ return fMUV1CREAMChannelMap[std::make_pair(CREAM,ChID)]; }
  int GetMUV2ChannelID (int CREAM, int ChID){ return fMUV2CREAMChannelMap[std::make_pair(CREAM,ChID)]; }
  pair<int,int> GetMUV1CREAMID (int ChannelID){ return fMUV1ChannelCREAMMap[ChannelID]; }
  pair<int,int> GetMUV2CREAMID (int ChannelID){ return fMUV2ChannelCREAMMap[ChannelID]; }

  pair<int,int> GetMUV1Channels(double x, double y);
  pair<int,int> GetMUV2Channels(double x, double y);

  bool IsMUV1CREAMHalfEmpty (int Slot, int half){ return fMUV1CREAMEmptyHalfSlot[make_pair(Slot,half)]; };
  bool IsMUV2CREAMHalfEmpty (int Slot, int half){ return fMUV2CREAMEmptyHalfSlot[make_pair(Slot,half)]; };

  double GetMUV1ScintillatorCenter(int ID){ if (ID%100<45) return fM1ScintillatorPositions[ID%100]; return 0; }
  double GetMUV2ScintillatorCenter(int ID){ if (ID%100<23) return fM2ScintillatorPositions[ID%100]; return 0; }

protected:
  static TString TagConvert (TString tag);
  bool TagMinMax (int &tagmin, int &tagmax);

private:
  int fRunID;
  int fYear;
  int fRevNum;
  TString fRevID;
  bool fIsMC;

  TF1 fMUV1QPosCorr;
  TF1 fMUV2QPosCorr;
  TF1 fMUV1TPosCorr;
  TF1 fMUV2TPosCorr;

  vector <double> fMUV1QPosCorrParameters[4][44];
  vector <double> fMUV1TPosCorrParameters[4][44];
  vector <double> fMUV2QPosCorrParameters[4][22];
  vector <double> fMUV2TPosCorrParameters[4][22];

  double fMUV1QEqValues[4][44],fMUV2QEqValues[4][22];

  map <int,pair<int,int>> fMUV1ChannelCREAMMap;
  map <pair<int,int>,int> fMUV1CREAMChannelMap;
  map <int,pair<int,int>> fMUV2ChannelCREAMMap;
  map <pair<int,int>,int> fMUV2CREAMChannelMap;
  map <pair<int,int>,bool> fMUV1CREAMEmptyHalfSlot;
  map <pair<int,int>,bool> fMUV2CREAMEmptyHalfSlot;

  Float_t fM1ScintillatorPositions[45],fM1ScintillatorWidth[45];
  Float_t fM2ScintillatorPositions[23],fM2ScintillatorWidth[23];


};

#endif
