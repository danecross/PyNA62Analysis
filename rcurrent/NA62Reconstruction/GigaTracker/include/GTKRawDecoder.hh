//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
//

#ifndef _GTKRawDecoder_hh_
#define _GTKRawDecoder_hh_ 1

#include "NA62VRawDecoder.hh"
#include <iostream>
#include "GigaTrackerNa62DataBlock.hh"

//#include "TGigaTrackerEvent.hh"
#include "TDCEvent.hh"
//#include "TSpecialTriggerEvent.hh"
//typedef TDCEvent TGigaTrackerDigiEvent ;

class GTKRawDecoder : public NA62VRawDecoder {
public:
  explicit GTKRawDecoder(NA62VReconstruction*);
  ~GTKRawDecoder();
  TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);
  void ParseRawDecoderSettingsFile(TString);
  int ConsistencyCheck(EventHeader*);
  void EndProcessing ();
  void StartOfBurst();
  void EndOfBurst();
  Int_t GetNCriticalErrorTypes(); //number of critical errors currently defined

private:
  GTK::GigaTrackerNa62DataBlock* mGTKBlock;

  TH1F *fChannelProfile[3]; ///< Channel profiles (periodic out-of-beam triggers)

  Bool_t fMaskNoisyPixels;
  TString fNoisyPixelListName;
  std::vector<std::pair<UInt_t, Int_t>> fReadNoisyPixelList; // <StationID, UID>
  int fErrorHeaderHitTS;
  int fDataFormat;
  int fLimiterThreshold[30];
  int fOffTimeStamp[3];

  int fErrorMissingChip;
  int fErrorIncChip;
  int fErrorIncSubId;
  int fErrorIncFCLSB[30];
  int fErrorIncTS[30];
  int fErrorIncEvtNb[30];
  int fErrorLimiter[30];
  int fErrorIncNTrigg[30];
  int fErrorDuplicated[30];

  void LoadTW();
  void LoadT0();
  Float_t fT0[3][18000];
  TGraph* fTW[3][10];

  
  enum GTKROErrorTypes{
	  HALF_BOARD_LIMITER_ACTIVE = 0
  };
};

#endif // _GTKRawDecoder_hh_
