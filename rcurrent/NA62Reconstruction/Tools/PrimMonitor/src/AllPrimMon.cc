#include "Riostream.h"
#include "TCanvas.h"
#include "AllPrimMon.hh"
#include "VPrimMon.hh"
#include "TVirtualFFT.h"
#include "TMath.h"
#include "TDatime.h"
#include "TPrimitive.hh"
//#include "ComputeCorrelation.hh"
#include <vector>

/*#define START            0x05B5CAC0 //2 seconds in 25ns
#define END              0x05bd5941 //3 seconds in 25 ns
#define STEP             10         //DT increment to find correlation
#define WIDTH            96         //Correlation Window
#define DELTA            10000      // Range in witch find correlation
*/
AllPrimMon::AllPrimMon(TRootBrowser* MainWin, TString Name) : VPrimMon(MainWin, Name) {

  NBurst = 0 ; 

  // set hist style
  fHIRCRICHCorr = new TH1D("hIRCRICHCorr","NewCHOD-RICH primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCRICHCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{RICH} (ns)");
  fHIRCRICHCorr->SetLineColor(kMagenta);
  fHIRCRICHCorr->SetLineWidth(2);
  fHIRCRICHCorr->SetLineStyle(6);

  fHIRCCHODCorr = new TH1D("hIRCCHODCorr","NewCHOD-CHOD primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCCHODCorr->GetXaxis()->SetTitle("time_{DET_{1}} - time_{DET_{2}} (ns)");
  fHIRCCHODCorr->SetLineColor(kGreen);
  fHIRCCHODCorr->SetLineWidth(2);
  fHIRCCHODCorr->SetLineStyle(1);

  fHIRCLAVCorr = new TH1D("hIRCLAVCorr","NewCHOD-LAV primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCLAVCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{LAV} (ns)");
  fHIRCLAVCorr->SetLineColor(kOrange);
  fHIRCLAVCorr->SetLineWidth(2);
  fHIRCLAVCorr->SetLineStyle(2);

  fHIRCMUVCorr = new TH1D("hIRCMUVCorr","NewCHOD-MUV3 primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCMUVCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{MUV3} (ns)");
  fHIRCMUVCorr->SetLineColor(kBlue);
  fHIRCMUVCorr->SetLineWidth(2);
  fHIRCMUVCorr->SetLineStyle(3);

  fHIRCLKRCorr = new TH1D("hIRCLKRCorr","NewCHOD-LKR primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCLKRCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{LKR} (ns)");
  fHIRCLKRCorr->SetLineColor(kRed);
  fHIRCLKRCorr->SetLineWidth(2);
  fHIRCLKRCorr->SetLineStyle(4);

  fHIRCTALKCorr = new TH1D("hIRCTALKCorr","NewCHOD-TALK primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHIRCTALKCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{TALK} (ns)");
  fHIRCTALKCorr->SetLineColor(kBlack);
  fHIRCTALKCorr->SetLineWidth(2);
  fHIRCTALKCorr->SetLineStyle(5);

  fHRICHCHODCorr = new TH1D("hRICHCHODCorr","RICH-CHOD primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHRICHCHODCorr->GetXaxis()->SetTitle("time_{RICH} - time_{CHOD} (ns)");
  fHRICHCHODCorr->SetLineColor(kGreen);
  fHRICHCHODCorr->SetLineWidth(2);
  fHRICHCHODCorr->SetLineStyle(1);

  fHRICHLKRCorr = new TH1D("hRICHLKRCorr","RICH-LKR primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHRICHLKRCorr->GetXaxis()->SetTitle("time_{RICH} - time_{LKR} (ns)");
  fHRICHLKRCorr->SetLineColor(kRed);
  fHRICHLKRCorr->SetLineWidth(2);
  fHRICHLKRCorr->SetLineStyle(4);

  fHRICHTALKCorr = new TH1D("hRICHTALKCorr","RICH-TALK primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHRICHTALKCorr->GetXaxis()->SetTitle("time_{RICH} - time_{TALK} (ns)");
  fHRICHTALKCorr->SetLineColor(kBlack);
  fHRICHTALKCorr->SetLineWidth(2);
  fHRICHTALKCorr->SetLineStyle(5);

  fHRICHLAVCorr = new TH1D("hRICHLAVCorr","RICH-LAV primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHRICHLAVCorr->GetXaxis()->SetTitle("time_{RICH} - time_{LAV} (ns)");
  fHRICHLAVCorr->SetLineColor(kOrange);
  fHRICHLAVCorr->SetLineWidth(2);
  fHRICHLAVCorr->SetLineStyle(2);

  fHRICHMUVCorr = new TH1D("hRICHMUVCorr","RICH-MUV primitives correlation",250, -250.*0.0974, 250.*0.098);
  fHRICHMUVCorr->GetXaxis()->SetTitle("time_{RICH} - time_{MUV} (ns)");
  fHRICHMUVCorr->SetLineColor(kBlue);
  fHRICHMUVCorr->SetLineWidth(2);
  fHRICHMUVCorr->SetLineStyle(3);

  // Primitive timing histos
  fHLAVOffsetMTP = new TH1D("hLAVOffsetMTP","LAV MTP time - primitives time",100, -10., 90.);
  fHLAVOffsetMTP->GetXaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHLAVOffsetMTP->SetLineColor(kOrange);
  fHLAVOffsetMTP->SetLineWidth(2);
  fHLAVOffsetMTP->SetLineStyle(3);

  fHLKROffsetMTP = new TH1D("hLKROffsetMTP","LKR MTP time - primitives time",100, -10., 90.);
  fHLKROffsetMTP->GetXaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHLKROffsetMTP->SetLineColor(kMagenta);
  fHLKROffsetMTP->SetLineWidth(2);
  fHLKROffsetMTP->SetLineStyle(5);

  fHIRCOffsetMTP = new TH1D("hIRCOffsetMTP","IRC MTP time - primitives time",100, -10., 90.);
  fHIRCOffsetMTP->GetXaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHIRCOffsetMTP->SetLineColor(kBlack);
  fHIRCOffsetMTP->SetLineWidth(2);
  fHIRCOffsetMTP->SetLineStyle(6);

  fHMUVOffsetMTP = new TH1D("hMUVOffsetMTP","MUV3 MTP time - primitives time",100, -10., 90.);
  fHMUVOffsetMTP->GetXaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHMUVOffsetMTP->SetLineColor(kBlue);
  fHMUVOffsetMTP->SetLineWidth(2);
  fHMUVOffsetMTP->SetLineStyle(4); 

  fHCHODOffsetMTP = new TH1D("hCHODOffsetMTP","CHOD MTP time - primitives time",100, -10., 90.);
  fHCHODOffsetMTP->GetXaxis()->SetTitle(" time_{MTP} - time_{prim}");
  fHCHODOffsetMTP->SetLineColor(kGreen);
  fHCHODOffsetMTP->SetLineWidth(2);
  fHCHODOffsetMTP->SetLineStyle(2);

  fHRICHOffsetMTP = new TH1D("hRICHOffsetMTP","RICH MTP time - primitives time",100, -10., 90.);
  fHRICHOffsetMTP->GetXaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHRICHOffsetMTP->SetLineColor(kRed);
  fHRICHOffsetMTP->SetLineWidth(2);
  fHRICHOffsetMTP->SetLineStyle(1);
  //

  fHLAVSendVsTimeStamp = new TH2F("hLAVSendVsTimeStamp","LAV MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHLAVSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHLAVSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHLAVSendVsTimeStamp->SetLineColor(kBlue);

  fHLKRSendVsTimeStamp = new TH2F("hLKRSendVsTimeStamp","LKR MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHLKRSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHLKRSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHLKRSendVsTimeStamp->SetLineColor(kBlue);

  fHIRCSendVsTimeStamp = new TH2F("hIRCSendVsTimeStamp","IRC MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHIRCSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHIRCSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHIRCSendVsTimeStamp->SetLineColor(kBlue);

  fHMUVSendVsTimeStamp = new TH2F("hMUVSendVsTimeStamp","MUV MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHMUVSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHMUVSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHMUVSendVsTimeStamp->SetLineColor(kBlue);

  fHCHODSendVsTimeStamp = new TH2F("hCHODSendVsTimeStamp","CHOD MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHCHODSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHCHODSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHCHODSendVsTimeStamp->SetLineColor(kBlue);

  fHRICHSendVsTimeStamp = new TH2F("hRICHSendVsTimeStamp","RICH MTP Offset",7000, 0, 7000., 400, -20., 20.);
  fHRICHSendVsTimeStamp->GetYaxis()->SetTitle("time_{MTP} - time_{prim}");
  fHRICHSendVsTimeStamp->GetXaxis()->SetTitle("time_{prim}");
  fHRICHSendVsTimeStamp->SetLineColor(kBlue);

  /*  fGIRCRICHCorr = new TGraph();
  fGIRCRICHCorr->SetTitle("NewCHOD-RICH correlation");
  fGIRCRICHCorr->SetName("fGIRCRICHCorr");
  fGIRCRICHCorr->SetFillColor(0);
  fGIRCRICHCorr->SetLineColor(kBlue);
  fGIRCRICHCorr->SetLineStyle(1);
  fGIRCRICHCorr->SetMarkerSize(2);
  fGIRCRICHCorr->GetXaxis()->SetLabelSize(0.05);
  fGIRCRICHCorr->GetYaxis()->SetLabelSize(0.05);
  fGIRCRICHCorr->GetXaxis()->SetTitle("time_{DET1} - time_{DET2}");
  */

  // Pave Text
  fPText = new TPaveText(0.6,0.74,0.89,0.89, "NDC");
  fPText->SetFillColor(0);
  fPText->SetLineColor(0);
  fPText->SetShadowColor(0);
  fPText->SetTextSize(0.06);
  fPText->SetTextColor(kRed);
  fPText->AddText("");
  
  // ADD a tabs to the monitor
  fLegCorr1 = new TLegend(0.6,0.5,0.89,0.89);
  fLegCorr1->SetFillColor(0);
  fLegCorr1->SetLineColor(0);
  fLegCorr1->SetTextSize(0.03);
  fLegCorr1->AddEntry(fHIRCCHODCorr,"NewCHOD-CHOD");
  fLegCorr1->AddEntry(fHIRCLAVCorr,"NewCHOD-LAV");
  fLegCorr1->AddEntry(fHIRCMUVCorr,"NewCHOD-MUV3");
  fLegCorr1->AddEntry(fHIRCLKRCorr,"NewCHOD-LKR");
  fLegCorr1->AddEntry(fHIRCTALKCorr,"NewCHOD-TALK");
  fLegCorr1->AddEntry(fHIRCRICHCorr,"NewCHOD-RICH");

  fLegCorr2 = new TLegend(0.6,0.5,0.89,0.89);
  fLegCorr2->SetFillColor(0);
  fLegCorr2->SetLineColor(0);
  fLegCorr2->SetTextSize(0.03);
  fLegCorr2->AddEntry(fHRICHCHODCorr,"RICH-CHOD");
  fLegCorr2->AddEntry(fHRICHLAVCorr,"RICH-LAV");
  fLegCorr2->AddEntry(fHRICHMUVCorr,"RICH-MUV3");
  fLegCorr2->AddEntry(fHRICHLKRCorr,"RICH-LKR");
  fLegCorr2->AddEntry(fHRICHTALKCorr,"RICH-TALK");

  // ADD a tabs to the monitor
  fLegMTP = new TLegend(0.75,0.7,0.89,0.89);
  fLegMTP->SetFillColor(0);
  fLegMTP->SetLineColor(0);
  fLegMTP->AddEntry(fHRICHOffsetMTP,"RICH");
  fLegMTP->AddEntry(fHCHODOffsetMTP,"CHOD");
  fLegMTP->AddEntry(fHIRCOffsetMTP,"NewCHOD");
  fLegMTP->AddEntry(fHLAVOffsetMTP,"LAV");
  fLegMTP->AddEntry(fHMUVOffsetMTP,"MUV3");
  fLegMTP->AddEntry(fHLKROffsetMTP,"LKR");


  TCanvas * cCorrelations = AddCanvasTab("Correlations");
  cCorrelations->Divide(2,1);
  cCorrelations->cd(1);
  fHIRCCHODCorr->GetXaxis()->SetTitleOffset(0.9);
  fHIRCCHODCorr->GetXaxis()->SetTitleSize(0.05);
  fHIRCCHODCorr->GetYaxis()->SetTitleSize(0.05);
  fHIRCCHODCorr->GetYaxis()->SetRange(0., 1.1);
  fHIRCCHODCorr->GetYaxis()->SetTitle("a.u.");
  fHIRCCHODCorr->SetTitle("Correlations with NewCHOD");
  fHIRCCHODCorr->GetXaxis()->SetTitle("time_{NewCHOD} - time_{DET} (ns)");
  fHIRCCHODCorr->Draw("C");
  fHIRCRICHCorr->Draw("C same");
  fHIRCMUVCorr->Draw("C same");
  fHIRCLAVCorr->Draw("C same");
  fHIRCTALKCorr->Draw("C same");
  fHIRCLKRCorr->Draw("C same");
  fLegCorr1->Draw();
  cCorrelations->cd(2);
  fHRICHCHODCorr->GetXaxis()->SetTitleOffset(0.9);
  fHRICHCHODCorr->GetXaxis()->SetTitleSize(0.05);
  fHRICHCHODCorr->GetYaxis()->SetTitleSize(0.05);
  fHRICHCHODCorr->GetYaxis()->SetRange(0., 1.1);
  fHRICHCHODCorr->GetYaxis()->SetTitle("a.u.");
  fHRICHCHODCorr->SetTitle("Correlations with RICH");
  fHRICHCHODCorr->GetXaxis()->SetTitle("time_{RICH} - time_{DET} (ns)");
  fHRICHCHODCorr->Draw("C");
  fHRICHMUVCorr->Draw("C same");
  fHRICHLAVCorr->Draw("C same");
  fHRICHTALKCorr->Draw("C same");
  fHRICHLKRCorr->Draw("C same");
  cCorrelations->SetGridx();
  fLegCorr2->Draw();
  
  /*  TCanvas * cCorrelations2 = AddCanvasTab("Correlations2");
  cCorrelations2->cd();
  fGIRCRICHCorr->SetPoint(0,0.,0.);
  fGIRCRICHCorr->Draw("ALP");
  */
  /*TCanvas * cOffsetMTP = */AddCanvasTab("MTP offsets");
  fHRICHOffsetMTP->GetXaxis()->SetTitleOffset(0.9);
  fHRICHOffsetMTP->GetXaxis()->SetTitleSize(0.05);
  fHRICHOffsetMTP->GetYaxis()->SetTitleSize(0.05);
  fHRICHOffsetMTP->GetYaxis()->SetRange(0., 1.8);
  fHRICHOffsetMTP->GetYaxis()->SetTitle("a.u.");
  fHRICHOffsetMTP->SetTitle("MTP time - Primitive time");
  fHRICHOffsetMTP->Draw();
  fHMUVOffsetMTP->Draw("same");
  fHLAVOffsetMTP->Draw("same");
  fHLKROffsetMTP->Draw("same");
  fHCHODOffsetMTP->Draw("same");
  fHIRCOffsetMTP->Draw("same");
  fLegMTP->Draw();

  TCanvas * cSendVsTimeStamp = AddCanvasTab("MTP vs TS");
  cSendVsTimeStamp->Divide(3,2);
  cSendVsTimeStamp->cd(1);
  fHMUVSendVsTimeStamp->Draw("colz");
  cSendVsTimeStamp->cd(2);
  fHIRCSendVsTimeStamp->Draw("colz");
  cSendVsTimeStamp->cd(3);
  fHCHODSendVsTimeStamp->Draw("colz");
  cSendVsTimeStamp->cd(4);
  fHRICHSendVsTimeStamp->Draw("colz");
  cSendVsTimeStamp->cd(5);
  fHLAVSendVsTimeStamp->Draw("colz");
  cSendVsTimeStamp->cd(6);
  fHLKRSendVsTimeStamp->Draw("colz");

  fLAVprimitive = new TPrimitive();  
  fMUVprimitive = new TPrimitive();  
  fCHODprimitive = new TPrimitive();  
  fRICHprimitive = new TPrimitive();  
  fIRCprimitive = new TPrimitive();  
  fLKRprimitive = new TPrimitive();  
  fTALKprimitive = new TPrimitive();  

  VPrimMon::CompleteTab();
}

AllPrimMon::~AllPrimMon() {
  for(UInt_t iCanvas=0;iCanvas<fCanvases.size();iCanvas++) {
     delete fCanvases[iCanvas];
     fCanvases[iCanvas] = 0;
  }  
}

void AllPrimMon::Update(TTree *lavtree, TTree *richtree, TTree *chodtree, TTree *muvtree, TTree *talktree, TTree *irctree,TTree *lkrtree, TString /*TimeRunBurst*/, TString time) {

  NBurst++ ;

  // Read ttree
  Bool_t lavin = kFALSE;  
  Bool_t lkrin = kFALSE;  
  Bool_t muvin = kFALSE;
  Bool_t richin = kFALSE; 
  Bool_t chodin = kFALSE;
  Bool_t talkin = kFALSE;
  Bool_t ircin = kFALSE;
  Int_t nlav=-1, nlkr=-1, nmuv=-1, nchod=-1, nrich=-1, ntalk=-1, nirc=-1;

  TBranch* b_lavtprimitive  = nullptr;
  TBranch* b_muvtprimitive  = nullptr;
  TBranch* b_chodtprimitive = nullptr;
  TBranch* b_richtprimitive = nullptr;
  TBranch* b_lkrtprimitive  = nullptr;
  TBranch* b_irctprimitive  = nullptr;
  TBranch* b_talktprimitive = nullptr;

  vector<Long64_t>  IRCtime;
  vector<Long64_t>  RICHtime;

  if (lavtree) {
    lavtree->SetBranchAddress("fPrimitive", &fLAVprimitive); 
    b_lavtprimitive = lavtree->GetBranch("fPrimitive") ;
    if (b_lavtprimitive) {lavin = kTRUE ; nlav = lavtree->GetEntries();}
  }
  if (muvtree) {
    muvtree->SetBranchAddress("fPrimitive", &fMUVprimitive);
    b_muvtprimitive = muvtree->GetBranch("fPrimitive") ;
    if (b_muvtprimitive) {muvin = kTRUE ; nmuv = muvtree->GetEntries();}
  }
  if (chodtree) {
    chodtree->SetBranchAddress("fPrimitive", &fCHODprimitive);
    b_chodtprimitive = chodtree->GetBranch("fPrimitive") ;
    if (b_chodtprimitive) {chodin = kTRUE ; nchod = chodtree->GetEntries();}
  }
  if (richtree) {
    richtree->SetBranchAddress("fPrimitive", &fRICHprimitive);
    b_richtprimitive = richtree->GetBranch("fPrimitive") ;
    if (b_richtprimitive) {richin = kTRUE ; nrich = richtree->GetEntries();}
  }
  if (lkrtree) {
    lkrtree->SetBranchAddress("fPrimitive", &fLKRprimitive);
    b_lkrtprimitive = lkrtree->GetBranch("fPrimitive") ;
    if (b_lkrtprimitive) {lkrin = kTRUE ; nlkr = lkrtree->GetEntries();}
  }
  if (irctree) {
    irctree->SetBranchAddress("fPrimitive", &fIRCprimitive);
    b_irctprimitive = irctree->GetBranch("fPrimitive") ;
    if (b_irctprimitive) {ircin = kTRUE ; nirc = irctree->GetEntries();}
  }
  if (talktree) {
    talktree->SetBranchAddress("fPrimitive", &fTALKprimitive);
    b_talktprimitive = talktree->GetBranch("fPrimitive") ;
    if (b_talktprimitive) {talkin = kTRUE ; ntalk = talktree->GetEntries();}
  }

  fHIRCLAVCorr->Reset();
  fHIRCCHODCorr->Reset();
  fHIRCRICHCorr->Reset();
  fHIRCLKRCorr->Reset();
  fHIRCMUVCorr->Reset();
  fHIRCTALKCorr->Reset();

  fHRICHCHODCorr->Reset();
  fHRICHMUVCorr->Reset();
  fHRICHLKRCorr->Reset();
  fHRICHTALKCorr->Reset();
  fHRICHLAVCorr->Reset();

  fHLAVOffsetMTP->Reset();
  fHRICHOffsetMTP->Reset();
  fHCHODOffsetMTP->Reset();
  fHMUVOffsetMTP->Reset();
  fHLKROffsetMTP->Reset();
  fHIRCOffsetMTP->Reset();

  fHLAVSendVsTimeStamp->Reset();
  fHRICHSendVsTimeStamp->Reset();
  fHCHODSendVsTimeStamp->Reset();
  fHMUVSendVsTimeStamp->Reset();
  fHLKRSendVsTimeStamp->Reset();
  fHIRCSendVsTimeStamp->Reset();

  fPText->Clear();
  /*
  Int_t ichod1 = 1000; // start from later primitives!
  Int_t irich1 = 1000;
  Int_t italk1 = 1000;
  Int_t ilkr1 = 1000;
  Int_t iirc1 = 1000;
  Int_t imuv1 = 1000;
  Int_t ilav1 = 1000;
  */
  Int_t ichod1 = (Int_t)nchod/2; // start from later primitives!
  Int_t irich1 = 0;
  Int_t italk1 = (Int_t)ntalk/2;
  Int_t ilkr1 = (Int_t)nlkr/2;
  Int_t iirc1 = 0;
  Int_t imuv1 = (Int_t)nmuv/2;
  Int_t ilav1 = (Int_t)nlav/2;

  Int_t diff;
  // NewCHOD-CHOD, RICH, MUV3, LAV, TALK, LKR
  if (ircin) {
    for (Int_t iirc = iirc1; iirc < nirc; iirc++) {
      b_irctprimitive->GetEntry(iirc) ;
      fHIRCSendVsTimeStamp->Fill(fIRCprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fIRCprimitive->GetSendTimeStamp() - fIRCprimitive->GetTimeStamp())/256.0 );
      fHIRCOffsetMTP->Fill(1.0*(fIRCprimitive->GetSendTimeStamp() - fIRCprimitive->GetTimeStamp())/256.0);
      //Long64_t time_tot=(fIRCprimitive->GetTimeStamp()<<8 | fIRCprimitive->GetFineTime());
      //IRCtime.push_back(time_tot);
      // chod
      if (chodin){
	for (Int_t ichod = ichod1; ichod < nchod; ichod++) {
	  b_chodtprimitive->GetEntry(ichod) ;
	  ichod1 = ichod;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fCHODprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHIRCCHODCorr->Fill(fIRCprimitive->GetTime() - fCHODprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // rich
      if (richin){
	for (Int_t irich = irich1; irich < nrich; irich++) {
	  b_richtprimitive->GetEntry(irich) ;
	  irich1 = irich;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fRICHprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHIRCRICHCorr->Fill(fIRCprimitive->GetTime() - fRICHprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // lav
      if (lavin){
	for (Int_t ilav = ilav1; ilav < nlav; ilav++) {
	  b_lavtprimitive->GetEntry(ilav) ;
	  ilav1 = ilav;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fLAVprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHIRCLAVCorr->Fill(fIRCprimitive->GetTime() - fLAVprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }

      // muv
      if (muvin){
	for (Int_t imuv = imuv1; imuv < nmuv; imuv++) {
	  b_muvtprimitive->GetEntry(imuv) ;
	  imuv1 = imuv;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fMUVprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHIRCMUVCorr->Fill(fIRCprimitive->GetTime() - fMUVprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // lkr
      if (lkrin){
	for (Int_t ilkr = ilkr1; ilkr < nlkr; ilkr++) {
	  b_lkrtprimitive->GetEntry(ilkr) ;
	  ilkr1 = ilkr;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fLKRprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) < 2 ) {
	    fHIRCLKRCorr->Fill(fIRCprimitive->GetTime() - fLKRprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }

      // talk
      if (talkin){
	for (Int_t italk = italk1; italk < ntalk; italk++) {
	  b_talktprimitive->GetEntry(italk) ;
	  italk1 = italk;   
	  diff = (Int_t)fIRCprimitive->GetTimeStamp() - (Int_t)fTALKprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHIRCTALKCorr->Fill(fIRCprimitive->GetTime() - fTALKprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
    }
  }


  // RICH-CHOD,  MUV3, LAV, TALK, LKR
  ichod1 = 1000; // start from later primitives!
  irich1 = 1000;
  italk1 = 1000;
  ilkr1 = 1000;
  iirc1 = 1000;
  imuv1 = 1000;
  ilav1 = 1000;

  ichod1 = (Int_t)nchod/2; // start from later primitives!
  irich1 = 0;
  italk1 = (Int_t)ntalk/2;
  ilkr1 = (Int_t)nlkr/2;
  iirc1 = 0;
  imuv1 = (Int_t)nmuv/2;
  ilav1 = (Int_t)nlav/2;

  if (richin) {
    for (Int_t irich = irich1; irich < nrich; irich++) {
      b_richtprimitive->GetEntry(irich) ;
      fHRICHSendVsTimeStamp->Fill(fRICHprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fRICHprimitive->GetSendTimeStamp() - fRICHprimitive->GetTimeStamp())/256.0 );
      fHRICHOffsetMTP->Fill(1.0*(fRICHprimitive->GetSendTimeStamp() - fRICHprimitive->GetTimeStamp())/256.0);
      //Long64_t time_tot=(fRICHprimitive->GetTimeStamp()<<8 | fRICHprimitive->GetFineTime());
      //RICHtime.push_back(time_tot);
      // chod
      if (chodin){
	for (Int_t ichod = ichod1; ichod < nchod; ichod++) {
	  b_chodtprimitive->GetEntry(ichod) ;
	  ichod1 = ichod;   
	  diff = (Int_t)fRICHprimitive->GetTimeStamp() - (Int_t)fCHODprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHRICHCHODCorr->Fill(fRICHprimitive->GetTime() - fCHODprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // lav
      if (lavin){
	for (Int_t ilav = ilav1; ilav < nlav; ilav++) {
	  b_lavtprimitive->GetEntry(ilav) ;
	  ilav1 = ilav;   
	  diff = (Int_t)fRICHprimitive->GetTimeStamp() - (Int_t)fLAVprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHRICHLAVCorr->Fill(fRICHprimitive->GetTime() - fLAVprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // muv
      if (muvin){
	for (Int_t imuv = imuv1; imuv < nmuv; imuv++) {
	  b_muvtprimitive->GetEntry(imuv) ;
	  imuv1 = imuv;   
	  diff = (Int_t)fRICHprimitive->GetTimeStamp() - (Int_t)fMUVprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHRICHMUVCorr->Fill(fRICHprimitive->GetTime() - fMUVprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // lkr
      if (lkrin){
	for (Int_t ilkr = ilkr1; ilkr < nlkr; ilkr++) {
	  b_lkrtprimitive->GetEntry(ilkr) ;
	  ilkr1 = ilkr;   
	  diff = (Int_t)fRICHprimitive->GetTimeStamp() - (Int_t)fLKRprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) < 2 ) {
	    fHRICHLKRCorr->Fill(fRICHprimitive->GetTime() - fLKRprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
      // talk
      if (talkin){
	for (Int_t italk = italk1; italk < ntalk; italk++) {
	  b_talktprimitive->GetEntry(italk) ;
	  italk1 = italk;   
	  diff = (Int_t)fRICHprimitive->GetTimeStamp() - (Int_t)fTALKprimitive->GetTimeStamp();  
	  if ( TMath::Abs(diff) == 0 ) {
	    fHRICHTALKCorr->Fill(fRICHprimitive->GetTime() - fTALKprimitive->GetTime() ) ;
	  } 
	  else if (diff<0 ) break;       
	}
      }
    }
  }

  // CHOD
  ichod1 = 1;
  if (chodin) {
    for (Int_t ichod = ichod1; ichod < nchod; ichod++) {
      b_chodtprimitive->GetEntry(ichod) ;
      fHCHODSendVsTimeStamp->Fill(fCHODprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fCHODprimitive->GetSendTimeStamp() - fCHODprimitive->GetTimeStamp())/256.0 );
      fHCHODOffsetMTP->Fill(1.0*(fCHODprimitive->GetSendTimeStamp() - fCHODprimitive->GetTimeStamp())/256.0);
    }
  }

  // MUV
  imuv1 = 1;
  if (muvin) {
    for (Int_t imuv = imuv1; imuv < nmuv; imuv++) {
      b_muvtprimitive->GetEntry(imuv) ;
      fHMUVSendVsTimeStamp->Fill(fMUVprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fMUVprimitive->GetSendTimeStamp() - fMUVprimitive->GetTimeStamp())/256.0 );
      fHMUVOffsetMTP->Fill(1.0*(fMUVprimitive->GetSendTimeStamp() - fMUVprimitive->GetTimeStamp())/256.0);
    }
  }

  // LAV
  ilav1 = 1;
  if (lavin) {
    for (Int_t ilav = ilav1; ilav < nlav; ilav++) {
      b_lavtprimitive->GetEntry(ilav) ;
      fHLAVSendVsTimeStamp->Fill(fLAVprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fLAVprimitive->GetSendTimeStamp() - fLAVprimitive->GetTimeStamp())/256.0 );
      fHLAVOffsetMTP->Fill(1.0*(fLAVprimitive->GetSendTimeStamp() - fLAVprimitive->GetTimeStamp())/256.0);
    }
  }

  // LKR
  ilkr1 = 1;
  if (lkrin) {
    for (Int_t ilkr = ilkr1; ilkr < nlkr; ilkr++) {
      b_lkrtprimitive->GetEntry(ilkr) ;
      fHLKRSendVsTimeStamp->Fill(fLKRprimitive->GetTimeStamp()*25.0/1e6, 1.0*(fLKRprimitive->GetSendTimeStamp() - fLKRprimitive->GetTimeStamp())/256.0 );
      fHLKROffsetMTP->Fill(1.0*(fLKRprimitive->GetSendTimeStamp() - fLKRprimitive->GetTimeStamp())/256.0);
    }
  }

  /*  // correlations
  Int_t DeltaMin  = -DELTA;
  Int_t DeltaMax  = DELTA;
  Int_t DeltaStep = STEP;
  Int_t Width     = WIDTH;
  vector <Double_t> delta;

  // preparing scan range
  for (Int_t i=DeltaMin; i<DeltaMax; i+=DeltaStep ) {
    delta.push_back(i);
  }

  // scanning
  TGraph* gtemp = (TGraph*)ComputeCorrelation(IRCtime, RICHtime, delta, Width);
  Double_t fac = gtemp->GetMaximum();
  cout << gtemp->GetN() << " POINTS IN TGRAPH" << endl;
  for (int i=0;i<gtemp->GetN();i++) {
    //gtemp->GetY()[i] *= fac;
    fGIRCRICHCorr->SetPoint(i,gtemp->GetX()[i],gtemp->GetY()[i]);
  }
  //  fGIRCRICHCorr = (TGraph*)gtemp->Clone("fGIRCRICHCorr");
  gtemp->Clear();
  delete gtemp;
  */
  if (fHIRCLAVCorr->GetEntries()>0) fHIRCLAVCorr->Scale(1./fHIRCLAVCorr->GetMaximum());
  if (fHIRCCHODCorr->GetEntries()>0)  fHIRCCHODCorr->Scale(1./fHIRCCHODCorr->GetMaximum());
  if (fHIRCRICHCorr->GetEntries()>0)  fHIRCRICHCorr->Scale(1./fHIRCRICHCorr->GetMaximum());
  if (fHIRCMUVCorr->GetEntries()>0)  fHIRCMUVCorr->Scale(1./fHIRCMUVCorr->GetMaximum());
  if (fHIRCLKRCorr->GetEntries()>0)  fHIRCLKRCorr->Scale(1./fHIRCLKRCorr->GetMaximum());
  if (fHIRCTALKCorr->GetEntries()>0)  fHIRCTALKCorr->Scale(1./fHIRCTALKCorr->GetMaximum());

  if (fHRICHLAVCorr->GetEntries()>0) fHRICHLAVCorr->Scale(1./fHRICHLAVCorr->GetMaximum());
  if (fHRICHCHODCorr->GetEntries()>0)  fHRICHCHODCorr->Scale(1./fHRICHCHODCorr->GetMaximum());
  if (fHRICHMUVCorr->GetEntries()>0)  fHRICHMUVCorr->Scale(1./fHRICHMUVCorr->GetMaximum());
  if (fHRICHLKRCorr->GetEntries()>0)  fHRICHLKRCorr->Scale(1./fHRICHLKRCorr->GetMaximum());
  if (fHRICHTALKCorr->GetEntries()>0)  fHRICHTALKCorr->Scale(1./fHRICHTALKCorr->GetMaximum());

  if (fHMUVOffsetMTP->GetEntries()>0)  fHMUVOffsetMTP->Scale(1.00/fHMUVOffsetMTP->GetMaximum());
  if (fHIRCOffsetMTP->GetEntries()>0)  fHIRCOffsetMTP->Scale(1.05/fHIRCOffsetMTP->GetMaximum());
  if (fHLAVOffsetMTP->GetEntries()>0)  fHLAVOffsetMTP->Scale(1.10/fHLAVOffsetMTP->GetMaximum());
  if (fHLKROffsetMTP->GetEntries()>0)  fHLKROffsetMTP->Scale(1.15/fHLKROffsetMTP->GetMaximum());
  if (fHCHODOffsetMTP->GetEntries()>0)  fHCHODOffsetMTP->Scale(1.20/fHCHODOffsetMTP->GetMaximum());
  if (fHRICHOffsetMTP->GetEntries()>0)  fHRICHOffsetMTP->Scale(1.25/fHRICHOffsetMTP->GetMaximum());

  cout << "Correlations done! Will now update Canvases" << endl;  

  VPrimMon::Update(time);

}
