// ---------------------------------------------------------------
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-08-17
// Created by Karim Massri (karim.massri@cern.ch) 2016-04-14
//
// ---------------------------------------------------------------
#ifndef NA62VOnlineMonitorCanvas_H
#define NA62VOnlineMonitorCanvas_H

#include "NA62VNamedModule.hh"
#include "NA62VReconstruction.hh"

#include "TEveManager.h"
#include "TEveBrowser.h"
#include "TGFrame.h"
#include "TGTab.h"
#include "TGraphErrors.h"

#include <vector>

class NA62VOnlineMonitorCanvas : public NA62VNamedModule{

  public:

    NA62VOnlineMonitorCanvas(TCanvas *, TString);
    explicit NA62VOnlineMonitorCanvas(TString);
    virtual ~NA62VOnlineMonitorCanvas();
    virtual void Update();
    virtual void Reset();
    TCanvas * GetCanvas(){ return fCanvas;};
    UInt_t GetNFrames() {return fFrames.size();}
    NA62VOnlineMonitorCanvas* GetFrame(UInt_t iFrame) {if(iFrame<fFrames.size()) return fFrames[iFrame]; else return 0;}
    NA62VOnlineMonitorCanvas* GetCurrentFrame() {if(GetFrame(fiCurrentFrame)) return GetFrame(fiCurrentFrame); else return this;}
    TString GetOnlineMonitorName(){ return fOnlineMonitorName; };
    void SetOnlineMonitorName(TString value){ fOnlineMonitorName = value; };
    TString GetReferenceFileName(){ return fReferenceFileName; };
    void SetReferenceFileName(TString value){ fReferenceFileName = value; };

  public:

    void DrawHisto(TH1*,TString Option="");
    void DrawHisto(TH2*,TString Option="");
    void DrawHisto(TGraph*,TString Option="");
    void DrawHisto(TGraphErrors*,TString Option="");
    TH1* DrawProjectionX(TH2*,TString ProjectionName,Int_t FirstYBin=0,Int_t LastYBin=-1,TString Option="");
    TH1* DrawProjectionY(TH2*,TString ProjectionName,Int_t FirstXBin=0,Int_t LastXBin=-1,TString Option="");
    void Divide(Int_t Nx,Int_t Ny);
    TVirtualPad * cd(Int_t iCanvas);

  private:

    void DrawHisto(TH1* histo,TH1* refhisto,TString Option="");
    TString FindReferenceHistoPath(TFile * ReferenceFile,TString HistoName);

  protected:

    TCanvas* fCanvas;
    std::vector<NA62VOnlineMonitorCanvas*> fFrames;
    UInt_t fiCurrentFrame;
    std::vector<TH1*> f1DHistos;
    std::vector<TH1*> f1DHistosReference;
    std::vector<TH1*> f2DHistos;
    TString fOnlineMonitorName;
    TString fReferenceFileName;

};
#endif
