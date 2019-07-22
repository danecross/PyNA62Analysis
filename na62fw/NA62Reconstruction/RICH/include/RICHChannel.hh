#ifndef RICHChannel_H
#define RICHChannel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TF1.h"

class RICHChannel : public NA62VChannel {

  public:
  RICHChannel(Int_t,Int_t,Int_t,Bool_t,TVector2,TVector2);
  ~RICHChannel();
  void InitHistograms();
  void FillTime(Double_t, Double_t);
  void SetSlewingCorrectionParameters(Double_t, Double_t, Double_t, Double_t);

  void Write(TFile*); 
  Double_t GetSlewingCorrection(Double_t width);
  Double_t GetMinWidth()                                       { return fMinWidth;                };
  Double_t GetMaxWidth()                                       { return fMaxWidth;                };
  Double_t GetNewSlewingCorr_MinWidth()                        { return fNewSlewingCorr_MinWidth; };
  Double_t GetNewSlewingCorr_MaxWidth()                        { return fNewSlewingCorr_MaxWidth; };
  Double_t GetMeanWidth()                                      { return fMeanWidth;               };
  void     SetMeanWidth(Double_t val)                          { fMeanWidth = val;                };

  TVector2 GetCenteredPosition()                               { return fCenteredPosition;        };
  void     SetCenteredPosition(TVector2 val)                   { fCenteredPosition = val;         };
  TVector2 GetShiftedPosition()                                { return fShiftedPosition;         };
  void     SetShiftedPosition(TVector2 val)                    { fShiftedPosition = val;          };

  private:

  Int_t  fSeqChannelID; 

  Double_t fMinWidth, fMaxWidth;
  Double_t fMeanWidth;
  Double_t fSlewingCorr_MinWidth, fSlewingCorr_MaxWidth;
  Double_t fNewSlewingCorr_MinWidth, fNewSlewingCorr_MaxWidth;
  TVector2 fCenteredPosition, fShiftedPosition;

  TH2F *fHRICHTimeVsWidth; //TProfile in unified - Roberta

};

#endif
