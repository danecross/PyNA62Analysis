#include "Riostream.h"
#include "TCanvas.h"
#include "MUVPrimMon.hh"
#include "VPrimMon.hh"
#include "TF1.h"
#include "TVirtualFFT.h"
#include "TMath.h"
#include "TDatime.h"
#include "TPrimitive.hh"

MUVPrimMon::MUVPrimMon(TRootBrowser* mainwin, TString Name) : VPrimMon(mainwin, Name) {
  NBurst = 0 ;

  // set hist style
  fHPrimTime = new TH1F("fHPrimTimeMUV","MUV3 primitives rate                                        ",800, 0., 8000.);
  fHPrimTime->GetXaxis()->SetTitle("ms");
  fHPrimTime->GetYaxis()->SetTitle("KHz");
  fHPrimTime->GetYaxis()->SetTitleOffset(0.4);
  fHPrimTime->GetYaxis()->SetTickLength(0.02);
  fHPrimTime->SetLineColor(kBlue);

  fHPrimTimeZoom1 = new TH1F("fHPrimTimeZoomMUV","MUV3 primitives rate                         ",80000, 0., 8000.);
  fHPrimTimeZoom1->GetXaxis()->SetTitle("ms");
  fHPrimTimeZoom1->GetYaxis()->SetTitle("KHz");
  fHPrimTimeZoom1->GetYaxis()->SetTitleOffset(0.4);
  fHPrimTimeZoom1->GetYaxis()->SetTickLength(0.02);
  fHPrimTimeZoom1->SetLineColor(kBlue);
  fHPrimTimeZoom2 = (TH1F*)fHPrimTimeZoom1->Clone();  
  fHPrimTimeZoom2->SetTitle("");
  fHPrimTimeZoom2->SetLineColor(kBlue);

  fHPrimDelta = new TH1F("fHPrimDeltaMUV","dt of primitives (ns) ;Time Difference (ns);Counts",200000, -100000, 100000.);
  fHPrimDelta->GetXaxis()->SetTitle("ns");
  fHPrimDelta->GetYaxis()->SetTitle("");
  fHPrimDelta->SetLineColor(kRed);

  fHPrimDeltaTS = new TH1F("fHPrimDeltaTSMUV","dt of primitives (FT units) ;Time Difference (x100 ps) ;Counts",2000, -1000, 1000.);
  fHPrimDeltaTS->GetXaxis()->SetTitle("FT units");
  fHPrimDeltaTS->GetYaxis()->SetTitle("");
  fHPrimDeltaTS->SetLineColor(kOrange);

  fHPrimID = new TH1F("fHPrimIDMUV","MUV3 primitive types",16, -0.5, 15.5);
  fHPrimID->GetXaxis()->SetTitle("");
  fHPrimID->GetXaxis()->SetBinLabel(1,"ML1");  
  fHPrimID->GetXaxis()->SetBinLabel(2,"MT1");  
  fHPrimID->GetXaxis()->SetBinLabel(3,"MLO1");  
  fHPrimID->GetXaxis()->SetBinLabel(4,"MTO1");  
  fHPrimID->GetXaxis()->SetBinLabel(5,"MLO2");  
  fHPrimID->GetXaxis()->SetBinLabel(6,"MTO2");  
  fHPrimID->GetXaxis()->SetBinLabel(7,"MMO2");  
  fHPrimID->GetXaxis()->SetBinLabel(8,"ML2");  
  fHPrimID->GetXaxis()->SetBinLabel(9,"MT2");  
  fHPrimID->GetXaxis()->SetBinLabel(10,"MM2");  
  fHPrimID->GetXaxis()->SetBinLabel(11,"MO1");  
  fHPrimID->GetXaxis()->SetBinLabel(12,"M1");  
  fHPrimID->GetXaxis()->SetBinLabel(13,"MO2");  
  fHPrimID->GetXaxis()->SetBinLabel(14,"M2");  
  fHPrimID->GetXaxis()->SetBinLabel(15,"All");  
  fHPrimID->GetXaxis()->SetBinLabel(16,"calib");  
  fHPrimID->GetXaxis()->LabelsOption("v");  
  
  fHPrimMask = new TH1F("fHPrimMaskMUV","16 bit MUV3 primitive masks",37, 0., 37);
  fHPrimMask->GetXaxis()->SetTitle("");
  fHPrimMask->GetYaxis()->SetTitle("");
  fHPrimMask->SetLineWidth(2);
  fHPrimMask->GetXaxis()->SetBinLabel(1,"0x4801"); 
  fHPrimMask->GetXaxis()->SetBinLabel(2,"0x4802"); 
  fHPrimMask->GetXaxis()->SetBinLabel(3,"0x4C05"); 
  fHPrimMask->GetXaxis()->SetBinLabel(4,"0x4C0A"); 
  fHPrimMask->GetXaxis()->SetBinLabel(5,"0x6881"); 
  fHPrimMask->GetXaxis()->SetBinLabel(6,"0x6902"); 
  fHPrimMask->GetXaxis()->SetBinLabel(7,"0x6903"); 
  fHPrimMask->GetXaxis()->SetBinLabel(8,"0x6983"); 
  fHPrimMask->GetXaxis()->SetBinLabel(9,"0x6A03"); 
  fHPrimMask->GetXaxis()->SetBinLabel(10,"0x6A83"); 
  fHPrimMask->GetXaxis()->SetBinLabel(11,"0x6C85"); 
  fHPrimMask->GetXaxis()->SetBinLabel(12,"0x6D07"); 
  fHPrimMask->GetXaxis()->SetBinLabel(13,"0x6D0A"); 
  fHPrimMask->GetXaxis()->SetBinLabel(14,"0x6D0B"); 
  fHPrimMask->GetXaxis()->SetBinLabel(15,"0x6D87"); 
  fHPrimMask->GetXaxis()->SetBinLabel(16,"0x6D8B"); 
  fHPrimMask->GetXaxis()->SetBinLabel(17,"0x6E07"); 
  fHPrimMask->GetXaxis()->SetBinLabel(18,"0x6E0B"); 
  fHPrimMask->GetXaxis()->SetBinLabel(19,"0x6E87"); 
  fHPrimMask->GetXaxis()->SetBinLabel(20,"0x6E8B"); 
  fHPrimMask->GetXaxis()->SetBinLabel(21,"0x7C95"); 
  fHPrimMask->GetXaxis()->SetBinLabel(22,"0x7D2A"); 
  fHPrimMask->GetXaxis()->SetBinLabel(23,"0x7D2B"); 
  fHPrimMask->GetXaxis()->SetBinLabel(24,"0x7D2F"); 
  fHPrimMask->GetXaxis()->SetBinLabel(25,"0x7D4F"); 
  fHPrimMask->GetXaxis()->SetBinLabel(26,"0x7D97"); 
  fHPrimMask->GetXaxis()->SetBinLabel(27,"0x7DAB"); 
  fHPrimMask->GetXaxis()->SetBinLabel(28,"0x7DAF"); 
  fHPrimMask->GetXaxis()->SetBinLabel(29,"0x7DBF"); 
  fHPrimMask->GetXaxis()->SetBinLabel(30,"0x7DCF"); 
  fHPrimMask->GetXaxis()->SetBinLabel(31,"0x7DDF"); 
  fHPrimMask->GetXaxis()->SetBinLabel(32,"0x7E4F"); 
  fHPrimMask->GetXaxis()->SetBinLabel(33,"0x7E97");
  fHPrimMask->GetXaxis()->SetBinLabel(34,"0x7ECF");
  fHPrimMask->GetXaxis()->SetBinLabel(35,"0x7EDF");
  fHPrimMask->GetXaxis()->SetBinLabel(36,"Bad/Unknown");  
  fHPrimMask->GetXaxis()->SetBinLabel(37,"All");  
  fHPrimMask->GetXaxis()->LabelsOption("v");  

  fHTimeBits = new TH1F("fHTimeBits","Probability of the 40 tstamp bits (timestamp & finetime) to be high; TimeStamp & FineTime; Fraction", 40, -39.5,0.5) ;
  fHTimeBits->Sumw2() ;

  fHFineTime = new TH1F("fHFineTime","Fine Time; FT; Counts", 256, -0.5,255.5) ;

  fHTimeStamp16   = new TH1F("fHTimeStamp16","Time Stamp below 4th bit;Time Stamp mod 16 ; Count", 16, -0.5,15.5) ;
  fHTimeStamp128  = new TH1F("fHTimeStamp128","Time Stamp below 7th bit;Time Stamp mod 128 ; Count", 128, -0.5,127.5) ;
  fHTimeStamp1024 = new TH1F("fHTimeStamp1024","Time Stamp below 10th bit;Time Stamp mod 1024 ; Count", 256, -0.5,1023.5) ;
  fHTimeStamp8192 = new TH1F("fHTimeStamp8192","Time Stamp below 13th bit;Time Stamp mod 8192 ; Count", 512, -0.5,8191.5) ;
  fHMultiTimeStamp16   = new TH1F("fHMultiTimeStamp16",
				 "Time Stamp below 4th bit;Time Stamp mod 16; Count", 16, -0.5,15.5) ;
  fHMultiTimeStamp128  = new TH1F("fHMultiTimeStamp128",
				 "Time Stamp below 7th bit;Time Stamp mod 128; Count", 128, -0.5,127.5) ;
  fHMultiTimeStamp1024 = new TH1F("fHMultiTimeStamp1024",
				 "Time Stamp below 10th bit;Time Stamp mod 1024; Count", 256, -0.5,1023.5) ;
  fHMultiTimeStamp8192 = new TH1F("fHMultiTimeStamp8192",
				 "Time Stamp below 13th bit;Time Stamp mod 8192; Count", 512, -0.5,8191.5) ;
  fHFineTimeStamp128   = new TH1F("fHFineTimeStamp128",
				 "Fine Time Stamp below 7th bit; Fine; Count", 128, -0.5,127.5) ;

  fHPrimFFT= new TH1D("Fourier transform - MUV3","FFT - MUV3; Hz; ",65536,0.,10000.);
  fHPrimFFT->SetLineColor(kBlue);

  fHPrimTimeFFT = new TH1D("MUV time profile for FFT","MUV time profile for FFT; ns",65536,0.,6.5536E+9);

  // Pave Text
  fPText = new TPaveText(0.6,0.74,0.89,0.89, "NDC");
  fPText->SetFillColor(0);
  fPText->SetLineColor(0);
  fPText->SetShadowColor(0);
  fPText->SetTextSize(0.06);
  fPText->SetTextColor(kBlack);
  fPText->AddText("N primitives = 0");

  fPTextTime = new TPaveText(0.55,0.74,0.85,0.89, "NDC");
  fPTextTime->SetFillColor(0);
  fPTextTime->SetLineColor(0);
  fPTextTime->SetShadowColor(0);
  fPTextTime->SetTextSize(0.06);
  //fPTextTime->SetTextColor(kMagenta+3);
  //fPTextTime->AddText(Form("Eff. spill length = 0 s"));

  TCanvas * cMUVPrimTS = AddCanvasTab("Rate");
  cMUVPrimTS->Divide(1,3);
  cMUVPrimTS->cd(1)->SetPad(.005, .500, .995, .995);
  cMUVPrimTS->cd(1);
  fHPrimTime->Draw();
  fPText->Draw();
  cMUVPrimTS->cd(2)->SetPad(.500, .005, .995, .495);
  cMUVPrimTS->cd(2);
  cMUVPrimTS->cd(2)->SetLogy(kTRUE);
  fHPrimDeltaTS->Draw();
  cMUVPrimTS->cd(3)->SetPad(.005, .005, .495, .495);
  cMUVPrimTS->cd(3);
  cMUVPrimTS->cd(3)->SetLogy(kTRUE);
  fHPrimDelta->Draw();

  TCanvas * cMUVZoom = AddCanvasTab("Burst structure");
  cMUVZoom->Divide(1,3);
  cMUVZoom->cd(1)->SetPad(.005, .500, .500, .995 );
  cMUVZoom->cd(1);
  fHPrimTimeZoom1->Draw();
  cMUVZoom->cd(2)->SetPad(.500, .500, .995, .995);
  cMUVZoom->cd(2);
  fHPrimFFT->Draw();
  cMUVZoom->cd(3)->SetPad(.005, .005, .995, .495);
  cMUVZoom->cd(3);
  fHPrimTimeZoom2->GetXaxis()->SetRangeUser(3000.,4000.);
  fHPrimTimeZoom2->Draw();

  TCanvas * cMUVPrimID = AddCanvasTab("Primitive IDs");
  cMUVPrimID->Divide(2,1);
  cMUVPrimID->cd(1);
  cMUVPrimID->cd(1)->SetLogx(kTRUE);
  fHPrimID->Draw("hbar1");
  fHPrimID->SetFillColor(kGreen);
  fHPrimID->SetBarWidth(0.8);
  cMUVPrimID->cd(2);
  cMUVPrimID->cd(2)->SetLogx(kTRUE);
  fHPrimMask->Draw("hbar1");
  fHPrimMask->SetFillColor(kRed);
  fHPrimMask->SetBarWidth(0.8);

  /*TCanvas * cMUVBits = */AddCanvasTab("Time Bits");
  fHTimeBits->GetYaxis()->SetRangeUser(0.4,0.6);
  fHTimeBits->Draw();

  TCanvas * cMUV3TimeStamps = AddCanvasTab("Time Stamps");
  cMUV3TimeStamps->Divide(2,2) ;
  cMUV3TimeStamps->cd(1) ;
  fHTimeStamp16->Draw();
  cMUV3TimeStamps->cd(2) ;
  fHTimeStamp128->Draw();
  cMUV3TimeStamps->cd(3) ;
  fHTimeStamp1024->Draw();
  cMUV3TimeStamps->cd(4) ;
  fHTimeStamp8192->Draw();

  TCanvas * cMUV3MultiTimeStamps = AddCanvasTab("MultiPad Time Stamps");
  cMUV3MultiTimeStamps->Divide(2,2) ;
  cMUV3MultiTimeStamps->cd(1) ;
  fHMultiTimeStamp16->Draw();
  cMUV3MultiTimeStamps->cd(2) ;
  fHMultiTimeStamp128->Draw();
  cMUV3MultiTimeStamps->cd(3) ;
  fHMultiTimeStamp1024->Draw() ;
  cMUV3MultiTimeStamps->cd(4) ;
  fHMultiTimeStamp8192->Draw() ;

  /*TCanvas * cMUV3FineTime = */AddCanvasTab("Fine Time");
  fHFineTime->Draw() ;

  /*TCanvas * cMUV3FineTimeStamps = */AddCanvasTab("MultiPad Fine Time Stamps");
  fHFineTimeStamp128->Draw();

  fTPrimitive = new TPrimitive();  
  VPrimMon::CompleteTab();
}

MUVPrimMon::~MUVPrimMon() {
  for(UInt_t iCanvas=0;iCanvas<fCanvases.size();iCanvas++) {
     delete fCanvases[iCanvas];
     fCanvases[iCanvas] = 0;
  }  
}

void MUVPrimMon::Update(TTree* tree, TString TimeRunBurst, TString time) {

  NBurst++ ;
  //double precTS = 24.951059536 ;
  //double precFT = precTS/256. ;

  fTree = tree;
  // Read ttree
  ULong64_t  tstamp, fulltstamp, tstampold = 0 ;
  Double_t fulltime, timeold = 0.;  
  Int_t primID, finetime ;

  fTree->SetBranchAddress("fPrimitive", &fTPrimitive);
  TBranch* b_fTPrimitive = fTree->GetBranch("fPrimitive") ;
  if(!b_fTPrimitive) return ;
  
  int nEntries = fTree->GetEntries();   
  cout << "MUV entries " << nEntries << endl;
  
  std::set<int> badvals ; 
  fHPrimTime->Reset();
  fHPrimDelta->Reset();
  fHPrimDeltaTS->Reset();
  fHPrimTimeZoom1->Reset();
  fHPrimTimeZoom2->Reset();
  fHPrimTimeFFT->Reset();
  fHPrimFFT->Reset();
  if(NBurst > 5){
    fHPrimID->Reset();
    fHPrimMask->Reset();
    fHTimeBits->Reset() ;
    fHFineTime->Reset() ;
    fHTimeStamp16->Reset() ;
    fHTimeStamp128->Reset() ;
    fHTimeStamp1024->Reset() ;
    fHTimeStamp8192->Reset() ;
    fHMultiTimeStamp16->Reset() ;
    fHMultiTimeStamp128->Reset() ;
    fHMultiTimeStamp1024->Reset() ;
    fHMultiTimeStamp8192->Reset() ;
    fHFineTimeStamp128->Reset();
  }
  
  fPText->Clear();
  fPTextTime->Clear();

  for (int iEnt = 0; iEnt < nEntries; iEnt++) {
    
    if(iEnt%100000==0){
      cout << "MUV: " << (100.*iEnt)/nEntries << "% of processing complete \r";
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

    if(primID>30000){
      fHMultiTimeStamp16->Fill( tstamp%16 ) ;
      fHMultiTimeStamp128->Fill( tstamp%128 ) ;
      fHMultiTimeStamp1024->Fill( tstamp%1024 ) ;
      fHMultiTimeStamp8192->Fill( tstamp%8192 ) ;
    }

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
    
    if (primID == 0x4801) fHPrimMask->Fill(0);
    else if (primID == 0x4802) fHPrimMask->Fill(1);
    else if (primID == 0x4c05) fHPrimMask->Fill(2);
    else if (primID == 0x4c0a) fHPrimMask->Fill(3);
    else if (primID == 0x6881) fHPrimMask->Fill(4);
    else if (primID == 0x6902) fHPrimMask->Fill(5);
    else if (primID == 0x6903) fHPrimMask->Fill(6);
    else if (primID == 0x6983) fHPrimMask->Fill(7);
    else if (primID == 0x6a03) fHPrimMask->Fill(8);
    else if (primID == 0x6a83) fHPrimMask->Fill(9);
    else if (primID == 0x6c85) fHPrimMask->Fill(10);
    else if (primID == 0x6d07) fHPrimMask->Fill(11);
    else if (primID == 0x6d0a) fHPrimMask->Fill(12);
    else if (primID == 0x6d0b) fHPrimMask->Fill(13);
    else if (primID == 0x6d87) fHPrimMask->Fill(14);
    else if (primID == 0x6d8b) fHPrimMask->Fill(15);
    else if (primID == 0x6e07) fHPrimMask->Fill(16);
    else if (primID == 0x6e0b) fHPrimMask->Fill(17);
    else if (primID == 0x6e87) fHPrimMask->Fill(18);
    else if (primID == 0x6e8b) fHPrimMask->Fill(19);
    else if (primID == 0x7c95) fHPrimMask->Fill(20);
    else if (primID == 0x7d2a) fHPrimMask->Fill(21);
    else if (primID == 0x7d2b) fHPrimMask->Fill(22);
    else if (primID == 0x7d2f) fHPrimMask->Fill(23);
    else if (primID == 0x7d4f) fHPrimMask->Fill(24);
    else if (primID == 0x7d97) fHPrimMask->Fill(25);
    else if (primID == 0x7dab) fHPrimMask->Fill(26);
    else if (primID == 0x7daf) fHPrimMask->Fill(27);
    else if (primID == 0x7dbf) fHPrimMask->Fill(28);
    else if (primID == 0x7dcf) fHPrimMask->Fill(29);
    else if (primID == 0x7ddf) fHPrimMask->Fill(30);
    else if (primID == 0x7e4f) fHPrimMask->Fill(31);
    else if (primID == 0x7e97) fHPrimMask->Fill(32);
    else if (primID == 0x7ecf) fHPrimMask->Fill(33);
    else if (primID == 0x7edf) fHPrimMask->Fill(34);
    else{
      badvals.insert(primID) ;
      fHPrimMask->Fill(35); // inconsistent mask
    } 
    fHPrimMask->Fill(36); // all

    fulltime = (Double_t)fTPrimitive->GetTime() ;
    fulltstamp = (tstamp*256 + finetime) ;
    fHPrimTime->Fill(fulltime/1e6);
    fHPrimTimeZoom1->Fill(fulltime/1e6);
    fHPrimTimeZoom2->Fill(fulltime/1e6);
    fHPrimTimeFFT->Fill(fulltime);
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
  /*  Int_t maxbin = fHPrimFFT->GetMaximumBin();
  TDatime dati; 
  Double_t y = (Double_t)fHPrimFFT->GetBinContent(maxbin); 
  Double_t x = dati.Convert(); 
  gFFTchange->SetPoint(NBurst-1,x,y ); 
  gFFTchange->GetXaxis()->SetTimeDisplay(1);
  gFFTchange->GetXaxis()->SetTimeFormat("%H:%M"); 
  gFFTchange->GetXaxis()->SetLabelSize(0.05);
  gFFTchange->GetYaxis()->SetLabelSize(0.05);
  */
  fHPrimFFT->SetAxisRange(20.,350.);

  cout << " Bad MUV3 primitives: " ;
  for (set<int>::iterator i = badvals.begin() ; i != badvals.end(); ++i) cout << *i << " " ;
  cout << endl ;

  fPText->AddText(Form("N primitives = %1.0f",fHPrimDelta->GetEntries()));
  //pText->AddText(Form("Rate (Fit) = %1.1f kHz",slope));
  fHPrimTime->Scale(1.,"width");  
  //pText->Draw() ;

  //pTextA->AddText(Form("This is burst %s",NBurst));
  //pTextA->Draw() ;
  fHPrimTimeZoom1->Scale(1.,"width");  
  fPTextTime->AddText(time.Data());
  //hPrimTimeZoom1->Draw() ;
  fHPrimTimeZoom2->Scale(1.,"width");    
  //hPrimTimeZoom2->Draw() ;
  fHTimeBits->Scale(1./nEntries, "width") ;
  fHTimeBits->GetYaxis()->SetRangeUser(0.4,0.6) ;
  //hFineTime->Draw() ;
  fHFineTime->GetYaxis()->SetRangeUser(0., fHFineTime->GetBinContent( fHFineTime->GetMaximumBin() )*1.1 ) ;

  //hFineTimeStamp128->Draw() ;
  Rescale(fHFineTimeStamp128) ;

  //hTimeStamp16->Draw() ;
  Rescale(fHTimeStamp16) ;

  //hTimeStamp128->Draw() ;
  Rescale(fHTimeStamp16) ;

  //hTimeStamp1024->Draw() ;
  Rescale(fHTimeStamp16) ;

  //hTimeStamp8192->Draw() ;
  Rescale(fHTimeStamp16) ;
  
  //hMultiTimeStamp16->Draw() ;
  Rescale(fHMultiTimeStamp16) ;

  //hMultiTimeStamp128->Draw() ;
  Rescale(fHMultiTimeStamp128) ;

  //hMultiTimeStamp1024->Draw() ;
  Rescale(fHMultiTimeStamp1024) ;

  //hMultiTimeStamp8192->Draw() ;
  Rescale(fHMultiTimeStamp8192) ;

  //hPrimDelta1Tight->Draw() ; 
 
  //hPrimDelta2Tight->Draw() ;  

  fHPrimTime->SetTitle(Form("MUV3 primitives rate - %s", TimeRunBurst.Data()));
  fHPrimTimeZoom1->SetTitle(Form("MUV3 - %s", TimeRunBurst.Data()));

  cout << "Will now update Canvases" << endl;  
  VPrimMon::Update(time);

  cout << "MUV3 Primitive counts: " << endl;
  cout << "---------------------- " << endl;
  cout << "0: ML1  " << (UInt_t)fHPrimID->GetBinContent(1) << endl;
  cout << "1: MT1  " << (UInt_t)fHPrimID->GetBinContent(2) << endl;
  cout << "2: ML01 " << (UInt_t)fHPrimID->GetBinContent(3) << endl;
  cout << "3: MT01 " << (UInt_t)fHPrimID->GetBinContent(4) << endl;
  cout << "4: ML02 " << (UInt_t)fHPrimID->GetBinContent(5) << endl;
  cout << "5: MT02 " << (UInt_t)fHPrimID->GetBinContent(6) << endl;
  cout << "6: MMO2 " << (UInt_t)fHPrimID->GetBinContent(7) << endl;
  cout << "7: ML2  " << (UInt_t)fHPrimID->GetBinContent(8) << endl;
  cout << "8: MT2  " << (UInt_t)fHPrimID->GetBinContent(9) << endl;
  cout << "9: MM2  " << (UInt_t)fHPrimID->GetBinContent(10) << endl;
  cout << "10: MO1 " << (UInt_t)fHPrimID->GetBinContent(11) << endl;
  cout << "11: M1  " << (UInt_t)fHPrimID->GetBinContent(12) << endl;
  cout << "12: MO2 " << (UInt_t)fHPrimID->GetBinContent(13) << endl;
  cout << "13: M2  " << (UInt_t)fHPrimID->GetBinContent(14) << endl;
  cout << "14: 1   " << (UInt_t)fHPrimID->GetBinContent(15) << endl;
  cout << "15: cal " << fHPrimID->GetBinContent(16) << endl;
  cout << "---------------------- " << endl;
  
}

void MUVPrimMon::Rescale(TH1F* h){
  
  double max = 1.10 * h->GetBinContent(h->GetMaximumBin()) ;
  double min = 0.90 * h->GetBinContent(h->GetMinimumBin()) ;
  
  h->GetYaxis()->SetRangeUser( min, max ) ;
  return ;
}
