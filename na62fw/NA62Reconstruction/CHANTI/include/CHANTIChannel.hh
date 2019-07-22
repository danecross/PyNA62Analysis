// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#ifndef CHANTIChannel_H
#define CHANTIChannel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TF1.h"

class CHANTIChannel : public NA62VChannel {

public:

  CHANTIChannel(Int_t, Int_t, Bool_t);
  ~CHANTIChannel();

  Int_t GetTellID (Int_t ROCH);
  Int_t GetTDCBID (Int_t ROCH);
  Int_t GetTDCID (Int_t ROCH);

  void InitHistograms();
  void FillPosition (Double_t, Double_t, Double_t, Double_t);
  void EvaluateSlewingCorrection();
  void SetSlewingCorrectionParameters(Double_t, Double_t);

  void Write(TFile*);

  //Double_t GetSlewingCorrection(Double_t width);
  //Double_t GetMinWidth()                   { return fMinWidth;                }
  //void     SetMinWidth(Double_t value)     { fMinWidth = value;               }
  //Double_t GetMaxWidth()                   { return fMaxWidth;                }
  //void     SetMaxWidth(Double_t value)     { fMaxWidth = value;               }
  //Double_t GetNewMinWidth()                { return fNewMinWidth;             }
  //void     SetNewMinWidth(Double_t value)  { fNewMinWidth = value;            }
  //Double_t GetNewMaxWidth()                { return fNewMaxWidth;             }
  //void     SetNewMaxWidth(Double_t value)  { fNewMaxWidth = value;            }
  //Double_t GetNewSlewingCorr_MinWidth()    { return fNewSlewingCorr_MinWidth; }
  //Double_t GetNewSlewingCorr_MaxWidth()    { return fNewSlewingCorr_MaxWidth; }


  Double_t GetThreshold()                  { return fThreshold;    }
  void     SetThreshold(Double_t value)    { fThreshold = value;   }

  void     SetSlewingCorrectionParameters (Double_t, Double_t, Double_t);
  Double_t GetSlewingCorrection (Double_t Width);
  
private:

  Double_t fThreshold;
  Double_t fp0, fp1, fp2;
  //Double_t fMinWidth, fMaxWidth;
  //Double_t fNewMinWidth, fNewMaxWidth;
  //Double_t fSlewingCorr_MinWidth, fSlewingCorr_MaxWidth;
  //Double_t fNewSlewingCorr_MinWidth, fNewSlewingCorr_MaxWidth;

  TH1D *fHDistanceFromSiPM;
  TProfile *fHTimeVsDistance;  

};

#endif
