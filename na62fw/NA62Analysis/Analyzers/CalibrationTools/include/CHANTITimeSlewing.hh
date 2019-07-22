#ifndef CHANTITIMESLEWING_HH
#define CHANTITIMESLEWING_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class CHANTITimeSlewing : public NA62Analysis::Analyzer {
  public:
    explicit CHANTITimeSlewing(NA62Analysis::Core::BaseAnalysis *ba);
    ~CHANTITimeSlewing();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();
  protected:

    void ParseRawDecoderSettingsFile();
    void SetOldTimeSlewing();
    void FillHistoTimeReso(Int_t nHits, TClonesArray & RecoHits, Double_t RefTime);

    Int_t fMaxNBursts;
    Double_t fThrH;
    Double_t fThrL;
    Double_t fMinFitL;
    Double_t fMaxFitL;
    Double_t fMinFitH;
    Double_t fMaxFitH;
    Bool_t fUseKTAGRef;
    Bool_t fReadingData;
    TString* fStationLabels;
    TString* fChLabels;
    TString* fLayers;
    TH2F**** fCorrL_ToT;
    TH2F**** fCorrH_ToT;
    TGraph*** fPar0L;
    TGraph*** fPar1L;
    TGraph*** fPar2L;
    TGraph*** fPar0H;
    TGraph*** fPar1H;
    TGraph*** fPar2H;
    TGraph*** fSigmaSingle;
    TGraph*** fSigmaSingleNotCorr;
    TGraph*** fSigmaSingleNewCorr;
    TGraph*** fSigmaDouble;		
    std::ofstream fTimeSlewingFile;
    TCanvas* fCanvas;
    TString  fOutPDFFileName;
    TString fRawDecoderSettingsFileName;
    TString fOldTimeSlewing;
    TString fNewTimeSlewing;
    Int_t* fChannelID;	
    Int_t fNChannels;
    Double_t **fOldTSCorrection;
    Double_t **fNewTSCorrection;
    TH2F**** fHitTimeSingle_BurstID;
    TH2F**** fHitTimeNotCorrSingle_BurstID;
    TH2F**** fHitTimeNewCorrSingle_BurstID;
    TH2F**** fHitTimeDouble_BurstID;
    TH2F* fCHANTIHitTimeReso; 
    TH2F* fCHANTIHitNotCorrTimeReso;
    TH2F* fCHANTIHitNewCorrTimeReso;
    TH2F* fCHANTIHitTimeResoBothTHR; 
    TH2F* fCHANTIHitTimeResoSingleTHR; 
    TH2F* fCHANTIHitTimeResoSingleTHRNotCorr;
    TH2F* fCHANTIHitTimeResoSingleTHRNewCorr;
};
#endif
