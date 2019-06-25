#include "Riostream.h"
#include "TCanvas.h"
#include "RICHPrimMon.hh"
#include "VPrimMon.hh"
#include "TF1.h"
#include "TVirtualFFT.h"
#include "TMath.h"
#include "TDatime.h"
#include "TPrimitive.hh"

RICHPrimMon::RICHPrimMon(TRootBrowser* mainwin, TString Name) : VPrimMon(mainwin, Name) {
  NBurst = 0 ;

  // set hist style
  fHPrimTime = new TH1F("fHPrimTimeRICH","RICH primitives rate                                        ",800, 0., 8000.);
  fHPrimTime->GetXaxis()->SetTitle("ms");
  fHPrimTime->GetYaxis()->SetTitle("KHz");
  fHPrimTime->GetYaxis()->SetTitleOffset(0.4);
  fHPrimTime->GetYaxis()->SetTickLength(0.02);
  fHPrimTime->SetLineColor(kBlue);

  fHPrimTimeZoom1 = new TH1F("fHPrimTimeZoomRICH","RICH primitives rate                         ",80000, 0., 8000.);
  fHPrimTimeZoom1->GetXaxis()->SetTitle("ms");
  fHPrimTimeZoom1->GetYaxis()->SetTitle("KHz");
  fHPrimTimeZoom1->GetYaxis()->SetTitleOffset(0.4);
  fHPrimTimeZoom1->GetYaxis()->SetTickLength(0.02);
  fHPrimTimeZoom1->SetLineColor(kBlue);
  fHPrimTimeZoom2 = (TH1F*)fHPrimTimeZoom1->Clone();  
  fHPrimTimeZoom2->SetTitle("");
  fHPrimTimeZoom2->SetLineColor(kBlue);

  fHPrimTimeForEffSpill = new TH1D("fHPrimTimeForEffSpillRICH","RICH primitives rate", 1e5, 0., 1e10);

  fHPrimDelta = new TH1F("fHPrimDeltaRICH","dt of primitives (ns) ;Time Difference (ns);Counts",200000, -100000, 100000.);
  fHPrimDelta->GetXaxis()->SetTitle("ns");
  fHPrimDelta->GetYaxis()->SetTitle("");
  fHPrimDelta->SetLineColor(kRed);

  fHPrimDeltaTS = new TH1F("fHPrimDeltaTSRICH","dt of primitives (FT units) ;Time Difference (x100 ps) ;Counts",2000, -1000, 1000.);
  fHPrimDeltaTS->GetXaxis()->SetTitle("FT units");
  fHPrimDeltaTS->GetYaxis()->SetTitle("");
  fHPrimDeltaTS->SetLineColor(kOrange);

  fHPrimID = new TH1F("fHPrimIDRICH","RICH primitive types",16, -0.5, 15.5);
  fHPrimID->GetXaxis()->SetTitle("");
  fHPrimID->GetXaxis()->SetBinLabel(1,"1");  
  fHPrimID->GetXaxis()->SetBinLabel(2,"2");  
  fHPrimID->GetXaxis()->SetBinLabel(3,"3");  
  fHPrimID->GetXaxis()->SetBinLabel(4,"4");  
  fHPrimID->GetXaxis()->SetBinLabel(5,"5");  
  fHPrimID->GetXaxis()->SetBinLabel(6,"6");  
  fHPrimID->GetXaxis()->SetBinLabel(7,"7");  
  fHPrimID->GetXaxis()->SetBinLabel(8,"8");  
  fHPrimID->GetXaxis()->SetBinLabel(9,"9");  
  fHPrimID->GetXaxis()->SetBinLabel(10,"A");  
  fHPrimID->GetXaxis()->SetBinLabel(11,"B");  
  fHPrimID->GetXaxis()->SetBinLabel(12,"C");  
  fHPrimID->GetXaxis()->SetBinLabel(13,"D");  
  fHPrimID->GetXaxis()->SetBinLabel(14,"E");  
  fHPrimID->GetXaxis()->SetBinLabel(15,"All");  
  fHPrimID->GetXaxis()->SetBinLabel(16,"calib");  
  fHPrimID->GetXaxis()->LabelsOption("v");  

  fHTimeBits = new TH1F("fHTimeBitsRICH","Probability of the 40 tstamp bits (timestamp & finetime) to be high; TimeStamp & FineTime; Fraction", 40, -39.5,0.5) ;
  fHTimeBits->Sumw2() ;

  fHFineTime = new TH1F("fHFineTimeRICH","Fine Time; FT; Counts", 256, -0.5,255.5) ;

  fHTimeStamp16   = new TH1F("fHTimeStamp16RICH","Time Stamp below 4th bit;Time Stamp mod 16 ; Count", 16, -0.5,15.5) ;
  fHTimeStamp128  = new TH1F("fHTimeStamp128RICH","Time Stamp below 7th bit;Time Stamp mod 128 ; Count", 128, -0.5,127.5) ;
  fHTimeStamp1024 = new TH1F("fHTimeStamp1024RICH","Time Stamp below 10th bit;Time Stamp mod 1024 ; Count", 256, -0.5,1023.5) ;
  fHTimeStamp8192 = new TH1F("fHTimeStamp8192RICH","Time Stamp below 13th bit;Time Stamp mod 8192 ; Count", 512, -0.5,8191.5) ;

  fHFineTimeStamp128   = new TH1F("fHFineTimeStamp128RICH",
				 "Fine Time Stamp below 7th bit; Fine; Count", 128, -0.5,127.5) ;

  fHPrimFFT= new TH1D("fHPrimFFTRICH","FFT - RICH; Hz; ",65536,0.,10000.);
  fHPrimFFT->SetLineColor(kBlue);

  fHPrimTimeFFT = new TH1D("fHPrimTimeFFTRICH","RICH time profile for FFT; ns",65536,0.,6.5536E+9);

  // Pave Text
  fPText = new TPaveText(0.6,0.74,0.89,0.89, "NDC");
  fPText->SetFillColor(0);
  fPText->SetLineColor(0);
  fPText->SetShadowColor(0);
  fPText->SetTextSize(0.06);
  fPText->SetTextColor(kBlack);
  fPText->AddText("N primitives = 0");

  fPTextTime = new TPaveText(0.65,0.74,0.85,0.89, "NDC");
  fPTextTime->SetFillColor(0);
  fPTextTime->SetLineColor(0);
  fPTextTime->SetShadowColor(0);
  fPTextTime->SetTextSize(0.06);
  fPTextTime->SetTextColor(kMagenta+3);
  fPTextTime->AddText(Form("Eff. spill length = 0 s"));

  TCanvas * cRICHPrimTS = AddCanvasTab("Rate");
  cRICHPrimTS->Divide(1,3);
  cRICHPrimTS->cd(1)->SetPad(.005, .500, .995, .995);
  cRICHPrimTS->cd(1);
  fHPrimTime->Draw();
  fPText->Draw();
  cRICHPrimTS->cd(2)->SetPad(.500, .005, .995, .495);
  cRICHPrimTS->cd(2);
  cRICHPrimTS->cd(2)->SetLogy(kTRUE);
  fHPrimDeltaTS->Draw();
  cRICHPrimTS->cd(3)->SetPad(.005, .005, .495, .495);
  cRICHPrimTS->cd(3);
  cRICHPrimTS->cd(3)->SetLogy(kTRUE);
  fHPrimDelta->Draw();

  TCanvas * cRICHZoom = AddCanvasTab("Burst structure");
  cRICHZoom->Divide(1,3);
  cRICHZoom->cd(1)->SetPad(.005, .500, .500, .995 );
  cRICHZoom->cd(1);
  fHPrimTimeZoom1->Draw();
  cRICHZoom->cd(2)->SetPad(.500, .500, .995, .995);
  cRICHZoom->cd(2);
  fHPrimFFT->Draw();
  cRICHZoom->cd(3)->SetPad(.005, .005, .995, .495);
  cRICHZoom->cd(3);
  fHPrimTimeZoom2->GetXaxis()->SetRangeUser(3000.,4000.);
  fHPrimTimeZoom2->Draw();
  fPTextTime->Draw();

  TCanvas * cRICHPrimID = AddCanvasTab("Primitive IDs");
  cRICHPrimID->cd(1)->SetLogx(kTRUE);
  fHPrimID->Draw("hbar1");
  fHPrimID->SetFillColor(kGreen);
  fHPrimID->SetBarWidth(0.8);

  /*TCanvas * cRICHBits = */AddCanvasTab("Time Bits");
  fHTimeBits->GetYaxis()->SetRangeUser(0.4,0.6);
  fHTimeBits->Draw();

  TCanvas * cRICHTimeStamps = AddCanvasTab("Time Stamps");
  cRICHTimeStamps->Divide(2,2) ;
  cRICHTimeStamps->cd(1) ;
  fHTimeStamp16->Draw();
  cRICHTimeStamps->cd(2) ;
  fHTimeStamp128->Draw();
  cRICHTimeStamps->cd(3) ;
  fHTimeStamp1024->Draw();
  cRICHTimeStamps->cd(4) ;
  fHTimeStamp8192->Draw();

  /*TCanvas * cRICHFineTime = */AddCanvasTab("Fine Time");
  fHFineTime->Draw() ;

  /*TCanvas * cRICHFineTimeStamps = */AddCanvasTab("Fine Time Stamps");
  fHFineTimeStamp128->Draw();

  fTPrimitive = new TPrimitive();  
  VPrimMon::CompleteTab();
}

RICHPrimMon::~RICHPrimMon() {
  for(UInt_t iCanvas=0;iCanvas<fCanvases.size();iCanvas++) {
     delete fCanvases[iCanvas];
     fCanvases[iCanvas] = 0;
  }  
}

void RICHPrimMon::Update(TTree* tree, TString TimeRunBurst, TString time) {

  NBurst++ ;

  fTree = tree;
  // Read ttree
  ULong64_t  tstamp, fulltstamp, tstampold = 0 ;
  Double_t fulltime, timeold = 0.;  
  Int_t primID, finetime ;

  fTree->SetBranchAddress("fPrimitive", &fTPrimitive);
  TBranch* b_fTPrimitive = fTree->GetBranch("fPrimitive") ;
  if(!b_fTPrimitive) return ;
  
  int nEntries = fTree->GetEntries();   
  cout << "RICH entries " << nEntries << endl;
  
  std::set<int> badvals ; 
  fHPrimTime->Reset();
  fHPrimTimeForEffSpill->Reset();
  fHPrimDelta->Reset();
  fHPrimDeltaTS->Reset();
  fHPrimTimeZoom1->Reset();
  fHPrimTimeZoom2->Reset();
  fHPrimTimeFFT->Reset();
  fHPrimFFT->Reset();
  if(NBurst > 5){
    fHPrimID->Reset();
    fHTimeBits->Reset() ;
    fHFineTime->Reset() ;
    fHTimeStamp16->Reset() ;
    fHTimeStamp128->Reset() ;
    fHTimeStamp1024->Reset() ;
    fHTimeStamp8192->Reset() ;
    fHFineTimeStamp128->Reset();
  }
  
  fPText->Clear();
  fPTextTime->Clear();

  for (int iEnt = 0; iEnt < nEntries; iEnt++) {
    
    if(iEnt%100000==0){
      cout << "RICH: " << (100.*iEnt)/nEntries << "% of processing complete \r";
    }

    b_fTPrimitive->GetEntry(iEnt) ;
    tstamp = fTPrimitive->GetTimeStamp();
    finetime = fTPrimitive->GetFineTime();
    primID = fTPrimitive->GetPrimitiveID();
    
    std::bitset< 32 > TS = ( tstamp ) ;
    std::bitset< 8 > FT = ( finetime ) ;

    for(int j = 0 ; j <32 ; ++j){
      size_t index = 31-j ;
      if(TS[index] == 1) fHTimeBits->Fill(j-39) ;
    }
    for(int j = 0 ; j < 8 ; ++j){
      size_t index = 7 - j ;
      if(FT[index] == 1) fHTimeBits->Fill(j-7) ;
    }  
    fHFineTime->Fill(finetime) ;
    fHTimeStamp16->Fill( tstamp%16 ) ;
    fHTimeStamp128->Fill( tstamp%128 ) ;
    fHTimeStamp1024->Fill( tstamp%1024 ) ;
    fHTimeStamp8192->Fill( tstamp%8192 ) ;
    fHFineTimeStamp128->Fill( finetime%128 ) ;

    if (primID&0x1) fHPrimID->Fill(0);
    if (primID&0x2) fHPrimID->Fill(1);
    if (primID&0x4) fHPrimID->Fill(2);
    if (primID&0x8) fHPrimID->Fill(3);
    if (primID&0x10) fHPrimID->Fill(4);
    if (primID&0x20) fHPrimID->Fill(5);
    if (primID&0x40) fHPrimID->Fill(6);
    if (primID&0x80) fHPrimID->Fill(7);
    if (primID&0x100) fHPrimID->Fill(8);
    if (primID&0x200) fHPrimID->Fill(9);
    if (primID&0x400) fHPrimID->Fill(10);
    if (primID&0x800) fHPrimID->Fill(11);
    if (primID&0x1000) fHPrimID->Fill(12);
    if (primID&0x2000) fHPrimID->Fill(13);
    if (primID&0x4000) fHPrimID->Fill(14);
    if (primID&0x8000) fHPrimID->Fill(15);
    
    fulltime = (Double_t)fTPrimitive->GetTime() ;
    fulltstamp = (tstamp*256 + finetime) ;
    fHPrimTime->Fill(fulltime/1e6);
    fHPrimTimeZoom1->Fill(fulltime/1e6);
    fHPrimTimeZoom2->Fill(fulltime/1e6);
    fHPrimTimeFFT->Fill(fulltime);
    fHPrimTimeForEffSpill->Fill(fulltime);
    if(primID == 0) continue ;

    if (timeold != 0){
      fHPrimDelta->Fill(fulltime - timeold);
      fHPrimDeltaTS->Fill(fulltstamp - tstampold);
    }
    timeold = fulltime;
    tstampold = fulltstamp;
  }
  cout << endl ;

  // FFT
  TH1 *htemp =0;
  TVirtualFFT::SetTransform(0);
  htemp = fHPrimTimeFFT->FFT(htemp, "MAG");
  for (Int_t n=0; n<65536;n++) {
    Double_t binc = htemp->GetBinContent(n);
    fHPrimFFT->SetBinContent (n,binc);
  }
  fHPrimFFT->SetAxisRange(20.,350.);
  if ( TMath::Sqrt(fHPrimTimeFFT->Integral())> 0. )  fHPrimFFT->Scale(1./TMath::Sqrt(fHPrimTimeFFT->Integral()));
  delete htemp;
  //

  // FFT vs time
  fHPrimFFT->SetAxisRange(40.,60.);
  fHPrimFFT->SetAxisRange(20.,350.);

  cout << " Bad RICH primitives: " ;
  for (set<int>::iterator i = badvals.begin() ; i != badvals.end(); ++i) cout << *i << " " ;
  cout << endl ;

  fPText->AddText(Form("N primitives = %1.0f",fHPrimDelta->GetEntries()));
  fHPrimTime->Scale(1.,"width");  

  // Eff spill
  Double_t Ndt = fHPrimTimeForEffSpill->Integral();
  TH1D* hist1 = (TH1D*)fHPrimTimeForEffSpill->Clone(); 
  fHPrimTimeForEffSpill->Multiply(hist1);
  Double_t N2dt = fHPrimTimeForEffSpill->Integral();
  hist1->Clear();
  std::cout.precision(5);
  Double_t effspill = (Double_t)(Ndt*Ndt/N2dt)*1e5/1e9; // *binwidth/units 
  //
  cout << "EFF SPILL = " << effspill << endl;
  fPTextTime->AddText(Form("Eff. spill length = %4.2f s", effspill));

  fHPrimTimeZoom1->Scale(1.,"width");  
  fHPrimTimeZoom2->Scale(1.,"width");    
  fHTimeBits->Scale(1./nEntries, "width") ;
  fHTimeBits->GetYaxis()->SetRangeUser(0.4,0.6) ;

  fHFineTime->GetYaxis()->SetRangeUser(0., fHFineTime->GetBinContent( fHFineTime->GetMaximumBin() )*1.1 ) ;

  Rescale(fHFineTimeStamp128) ;

  Rescale(fHTimeStamp16) ;

  Rescale(fHTimeStamp16) ;

  Rescale(fHTimeStamp16) ;

  Rescale(fHTimeStamp16) ;

  fHPrimTime->SetTitle(Form("RICH primitives rate - %s", TimeRunBurst.Data()));
  fHPrimTimeZoom1->SetTitle(Form("RICH - %s", TimeRunBurst.Data()));

  cout << "Will now update Canvases" << endl;  
  VPrimMon::Update(time);
  
}

void RICHPrimMon::Rescale(TH1F* h){
  
  double max = 1.10 * h->GetBinContent(h->GetMaximumBin()) ;
  double min = 0.90 * h->GetBinContent(h->GetMinimumBin()) ;
  
  h->GetYaxis()->SetRangeUser( min, max ) ;
  return ;
}
