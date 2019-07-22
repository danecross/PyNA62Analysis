// ---------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (tommaso.spadaro@lnf.infn.it) 2016-03-26
//
// ---------------------------------------------------------------

#ifndef SAVMatching_HH
#define SAVMatching_HH 1

#include "Persistency.hh"
#include <iostream>
using namespace std;

class SAVMatching  {

public:
  SAVMatching();
  ~SAVMatching();

  void SetTimeCuts(Double_t valLow, Double_t valHigh)    { SetIRCTimeCuts(valLow,valHigh); SetSACTimeCuts(valLow,valHigh);} ///< Set time cuts w/wo low & high threshold leading edges (ns)
  void SetIRCTimeCuts(Double_t valLow, Double_t valHigh) { fLowThrSAVTimeCut[0] = valLow; fHighThrSAVTimeCut[0] = valHigh;} ///< Set time cuts w/wo low & high threshold leading edges (ns)
  void SetSACTimeCuts(Double_t valLow, Double_t valHigh) { fLowThrSAVTimeCut[1] = valLow; fHighThrSAVTimeCut[1] = valHigh;} ///< Set time cuts w/wo low & high threshold leading edges (ns)

  void SetIRCNoisyChannel(Int_t val)  { fNoisyChannels[0].push_back(val); } ///< Define channel val as noisy (taken from hit->GetChannelID())
  void SetSACNoisyChannel(Int_t val)  { fNoisyChannels[1].push_back(val); } ///< Define channel val as noisy (taken from hit->GetChannelID())
  void SetReferenceTime(Double_t val) { fRefTime = val;                   } ///< Set time of reference detector to be compared with hit time

  Int_t SAVHasTimeMatching(TRecoIRCEvent*, TRecoSACEvent*, Int_t mode=0); ///< Perform time matching: bit 0,1 ON if at least 1 block found in IRC,SAC: input mode = 0-->apply slewing

  Int_t   GetNumberOfIRCMatchedBlocks() { return fNMatched[0];     }
  Int_t   GetNumberOfSACMatchedBlocks() { return fNMatched[1];     }
  Int_t*  GetIndexOfIRCMatchedBlocks()  { return fIndexMatched[0]; }
  Int_t*  GetIndexOfSACMatchedBlocks()  { return fIndexMatched[1]; }

  Int_t*  GetChannelIDOfIRCMatchedBlocks()  { return fChannelIDMatched[0]; }
  Int_t*  GetChannelIDOfSACMatchedBlocks()  { return fChannelIDMatched[1]; }

  Int_t   GetBestEdgeMaskOfIRCMatchedBlocks() { return fBestEdgeMask[0];      }
  Int_t   GetBestEdgeMaskOfSACMatchedBlocks() { return fBestEdgeMask[1];      }
  Int_t   GetEdgeMaskOfBestIRCHit()           { return fEdgeMaskOfBestHit[0]; }
  Int_t   GetEdgeMaskOfBestSACHit()           { return fEdgeMaskOfBestHit[1]; }

  Double_t GetBestTimeOfIRCMatchedBlocks() { return fDeltaTBest[0]; }
  Double_t GetBestTimeOfSACMatchedBlocks() { return fDeltaTBest[1]; }

  Double_t GetSlewingCorrection(Int_t, TRecoVHit*);///< Input iDet = 0,1 for IRC,SAC and hit object
  void Clear(); ///< Clear event-by-event counters
  void Print(); ///< Print relevant information (to be run after EvaluateTimeMatching)
  void SetIsMC(Bool_t value) { fIsMC = value;  }
  void InitializeInputForSlewing(); ///< Read of input slewing information

private:
  Bool_t fIsMC; ///< Running over MC or data?
  Bool_t fSlewingInputRetrieved; ///<True if input info have been retrieved
  Int_t fMaxTOTBins;
  Int_t fNTOTBins[2];   ///<Number of TOT-Low bins from the input slewing-correction files
  Double_t fTOTBinning; ///<Binning of TOT (identical for IRC and SAC)
  Double_t* fTOTLow[2]; ///<IRC,SAC paramterisation x axis (TOT Low)
  Double_t** fAverageCorrection[2]; ///<IRC,SAC paramterisation y axis (Average time-reference time for each of the 4 channels

  Double_t fRefTime;              ///< Reference time [ns]
  Double_t fLowThrSAVTimeCut[2];  ///< Cut wrt reference time [ns] for IRC, SAC
  Double_t fHighThrSAVTimeCut[2]; ///< Cut wrt reference time [ns] for IRC, SAC

  TRecoVEvent* fSAVEvent[2];

  Int_t    fNMatched[2];             ///< Number of matched blocks for IRC, SAC 
  Int_t    fIndexMatched[2][100];    ///< Index of matched blocks (store them up to 100) for IRC, SAC
  Int_t    fChannelIDMatched[2][100];///< ChannelID of matched blocks (store them up to 100) for IRC, SAC
  Int_t    fBestEdgeMask[2];         ///< Best edgemask matched
  Int_t    fEdgeMaskOfBestHit[2];    ///< Edgemask of the hit with the best time
  Double_t fDeltaTBest[2];           ///< Best time matching among blocks with chosen edgemasks
  vector<Int_t> fNoisyChannels[2];
  Int_t fSAVPriorityMask[16];

};
#endif
