#include "NA62VOnlineMonitor.hh"
#include "NA62VRawDecoder.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "TDCBRawDecoder.hh"
#include "SRBRawDecoder.hh"
#include "NA62VReconstruction.hh"
#include "SpectrometerReconstruction.hh"

#include "TRootBrowser.h"
#include "TGFrame.h"
#include "TRootEmbeddedCanvas.h"
#include "TGLayout.h"
#include "TPad.h"
#include "TCanvas.h"
#include "TPaveText.h"

#include <sys/stat.h>

NA62VOnlineMonitor::NA62VOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, TString Name) : NA62VNamedModule(Name), fReco(Reco), fHDigiTimeRawFine1D(nullptr), fROBoardLinesRawFineVsROCh(nullptr), fROBoardLinesErrors(nullptr), fDecoderErrorsString(nullptr), fDecoderErrorsValues(nullptr), fDecoderErrorsMask(0){

  // stuff to do for all OnlineMonitor pages
  fMainWindow = MainWindow;
  if(fMainWindow) fMainWindow->StartEmbedding(1,-1);
  fMainTabFrame = new TGMainFrame(gClient->GetRoot(),10,10,kMainFrame | kVerticalFrame);
  fMainTabFrame->SetName(Name);
  fMainTabFrame->SetWindowName(Name);
  fMainTab = new TGTab(fMainTabFrame,300,300);
  fMainTab->SetName(Name);

  UInt_t nROBoards;
  fDecoderErrorsValues = 0;
  if(fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder()) {
    nROBoards = fReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
    fDecoderErrorsValues = new int[nROBoards];
    for(UInt_t iVal=0;iVal<nROBoards;iVal++) {
      fDecoderErrorsValues[iVal] = 0;
    }
  }
  fDecoderErrorsString = new char[1024];
  strcpy(fDecoderErrorsString,"");
  fDecoderErrorsMask = 0;
#ifdef DIM
  std::cout << "Creating OM DIM services for " << fReco->GetName() << "..." << std::endl;
  fDecoderErrorsStringService = new DimService(Form("NA62/OM/%s/DecoderErrorsString",Reco->GetName().Data()),fDecoderErrorsString);
  fDecoderErrorsValuesService = new DimService(Form("NA62/OM/%s/DecoderErrorsValues",Reco->GetName().Data()),static_cast<char*>("I",fDecoderErrorsValues,nROBoards*sizeof(int)));
  fDecoderErrorsMaskService   = new DimService(Form("NA62/OM/%s/DecoderErrorsMask",  Reco->GetName().Data()),fDecoderErrorsMask);
  //Reset DIM services
  strcpy(fDecoderErrorsString,"");
  for(UInt_t iBoard=0;iBoard<nROBoards;iBoard++){
    fDecoderErrorsMask = 0;
    fDecoderErrorsValues[iBoard] = 0;
    strcat(fDecoderErrorsString,"0");
    if(iBoard!=nROBoards-1) strcat(fDecoderErrorsString,"|");
  }
  std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error string: " << fDecoderErrorsString << std::endl;
  std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error values: ";
  for(UInt_t iVal=0;iVal<nROBoards;iVal++){
    std::cout << fDecoderErrorsValues[iVal];
    if(iVal!=nROBoards-1) std::cout << ",";
  }
  std::cout << std::endl;
  std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error mask:   " << std::hex << fDecoderErrorsMask << std::dec << std::endl;
  fDecoderErrorsStringService->updateService();
  fDecoderErrorsValuesService->updateService();
  fDecoderErrorsMaskService->updateService();
#endif

  // Histos exclusively used in the Online Monitor
  TFile *histoFile=fReco->GetHistoFile();
  if(!histoFile) return;
  // switch to a subdirectory in the output file to avoid histogram name clash from decoders
  // base class is instantiated first

  TDirectory * monitorDir =histoFile->GetDirectory(fReco->GetName()+"Monitor");
  if(!monitorDir) histoFile->mkdir(fReco->GetName()+"Monitor")->cd();
  else monitorDir->cd();

  // -- Add here common histos
}

NA62VOnlineMonitor::~NA62VOnlineMonitor(){
  std::cout << "Deleting " << fName << " OnlineMonitor.." << std::endl;
  for(UInt_t iCanvas=0;iCanvas<fCanvases.size();iCanvas++) {
    delete fCanvases[iCanvas];
    fCanvases[iCanvas] = 0;
  }
#ifdef DIM
  if(fDecoderErrorsStringService) delete fDecoderErrorsStringService;
  if(fDecoderErrorsValuesService) delete fDecoderErrorsValuesService;
  if(fDecoderErrorsMaskService)   delete fDecoderErrorsMaskService;
#endif
}

void NA62VOnlineMonitor::AddDigiTimeRawFinePlots(){
  //find right nxROBoards and nyROBoards to accomodate all the plots
  UInt_t nROBoards = 0;
  Int_t  nxROBoards = 0;
  Int_t  nyROBoards = 0;
  UInt_t nROChannelsPerFullBoard = 0;

  if(fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder()) {
    nROBoards = fReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
    nROChannelsPerFullBoard = fReco->GetRawDecoder()->GetDecoder()->GetNROChannels()/nROBoards;
  }
  while((UInt_t)(nxROBoards*nyROBoards)<nROBoards){
    if(3*nyROBoards<2*nxROBoards) nyROBoards++;
    if((UInt_t)(nxROBoards*nyROBoards)<nROBoards) nxROBoards++;
  }

  if (fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder() && fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()) {
    NA62VOnlineMonitorCanvas * DigiTimeRawFine = AddCanvasTab("DigiTimeRawFine");
    DigiTimeRawFine->Divide(nxROBoards,nyROBoards);
    if(nROBoards) fHDigiTimeRawFine1D = new TH1D*[nROBoards];
    for(UInt_t iROBoard=0;iROBoard<nROBoards;iROBoard++){
      DigiTimeRawFine->cd(iROBoard+1);
      TH2F * hDigiTimeRawFine = fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine();
      Int_t iFirstBin = iROBoard*hDigiTimeRawFine->GetNbinsX()/nROBoards+1;
      Int_t iLastBin  = (iROBoard+1)*hDigiTimeRawFine->GetNbinsX()/nROBoards;
      fHDigiTimeRawFine1D[iROBoard] = static_cast<TH1D*>(DigiTimeRawFine->GetCurrentFrame()->DrawProjectionY(hDigiTimeRawFine,Form("%s%sBoard%02d",fReco->GetName().Data(),hDigiTimeRawFine->GetName(),iROBoard),iFirstBin,iLastBin));
      fHDigiTimeRawFine1D[iROBoard]->SetTitle(Form("%s %s Board%02d",fReco->GetName().Data(),hDigiTimeRawFine->GetName(),iROBoard));
      if(!(fReco->GetName()=="Spectrometer")) gPad->SetLogy(1);
    }
  }

  if (fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder() && fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFineVsROChannel()) {
    NA62VOnlineMonitorCanvas * DigiTimeRawFineVsROChannel = AddCanvasTab("DigiTimeRawFineVsROChannel");
    TH2F* hDigiTimeRawFineVsROChannel = fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFineVsROChannel();
    hDigiTimeRawFineVsROChannel->SetStats(0);
    DigiTimeRawFineVsROChannel->GetCurrentFrame()->DrawHisto(fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFineVsROChannel(),"COLZ");
    gPad->SetGrid(kFALSE,kTRUE);
    gPad->SetLogz(1);
    if(nROBoards>1){
      fROBoardLinesRawFineVsROCh = new TLine*[nROBoards-1];
      for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
        fROBoardLinesRawFineVsROCh[iROBoard] = new TLine((iROBoard+1)*nROChannelsPerFullBoard-0.5,hDigiTimeRawFineVsROChannel->GetYaxis()->GetXmin(),
            (iROBoard+1)*nROChannelsPerFullBoard-0.5,hDigiTimeRawFineVsROChannel->GetYaxis()->GetXmax());
        fROBoardLinesRawFineVsROCh[iROBoard]->SetLineStyle(2);
        fROBoardLinesRawFineVsROCh[iROBoard]->Draw();
      }
    }
  }
}

void NA62VOnlineMonitor::AddDecoderErrorsPlots(){
  //find right nxROBoards and nyROBoards to accomodate all the plots
  UInt_t nROBoards = 0;
  Int_t  nxROBoards = 0;
  Int_t  nyROBoards = 0;
  UInt_t nROMezzaninesPerFullBoard = 0;

  if(fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder()) {
    nROBoards = fReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
    nROMezzaninesPerFullBoard = fReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
  }
  while((UInt_t)(nxROBoards*nyROBoards)<nROBoards){
    if(3*nyROBoards<2*nxROBoards) nyROBoards++;
    if((UInt_t)(nxROBoards*nyROBoards)<nROBoards) nxROBoards++;
  }

  if (fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder() && fReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()) {
    NA62VOnlineMonitorCanvas * DecoderErrors = AddCanvasTab("DecoderErrors");
    gPad->SetLeftMargin(0.12);
    TH2F * hDecoderErrors = fReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors();
    hDecoderErrors->SetStats(0);
    DecoderErrors->GetCurrentFrame()->DrawHisto(hDecoderErrors,"COLZ");
    gPad->SetGrid(kFALSE,kTRUE);
    gPad->SetLogz(1);
    if(nROBoards>1){
      fROBoardLinesErrors = new TLine*[nROBoards-1];
      for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
        fROBoardLinesErrors[iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmin(),
            (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmax());
        fROBoardLinesErrors[iROBoard]->SetLineStyle(2);
        fROBoardLinesErrors[iROBoard]->Draw();
      }
    }
  }

  //if (fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder() && fReco->GetRawDecoder()->GetDecoder()->GetName() == "TDCB"){
  //  // Additional TEL62 error tab for TEL62-based detectors
  //  NA62VOnlineMonitorCanvas * TEL62Errors = AddCanvasTab("TEL62Errors");
  //  gPad->SetLeftMargin(0.12);
  //  TH2F * hTEL62Errors = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetHTEL62Errors();
  //  hTEL62Errors->SetStats(0);
  //  TEL62Errors->GetCurrentFrame()->DrawHisto(hTEL62Errors,"COLZ");
  //  gPad->SetGrid(kFALSE,kTRUE);
  //  gPad->SetLogz(1);
  //}
}

void NA62VOnlineMonitor::CompleteTab(){
  fMainTab->SetTab(0);
  fMainTab->Resize();
  fMainTabFrame->AddFrame(fMainTab, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));

  fMainTabFrame->SetMWMHints(kMWMDecorAll, kMWMFuncAll, kMWMInputModeless);
  fMainTabFrame->MapSubwindows();
  fMainTabFrame->Resize();
  fMainTabFrame->MapWindow();
  if(fMainWindow) fMainWindow->StopEmbedding(fName);
}

NA62VOnlineMonitorCanvas * NA62VOnlineMonitor::AddCanvasTab(TString Name){
  // container of Tab
  TGCompositeFrame *SubTabCompositeFrame = fMainTab->AddTab(Name);
  SubTabCompositeFrame->SetLayoutManager(new TGVerticalLayout(SubTabCompositeFrame));

  // embedded canvas
  TRootEmbeddedCanvas *RootEmbeddedCanvas = new TRootEmbeddedCanvas(0,SubTabCompositeFrame,800,600);
  SubTabCompositeFrame->AddFrame(RootEmbeddedCanvas, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));

  fCanvases.push_back(new NA62VOnlineMonitorCanvas(RootEmbeddedCanvas->GetCanvas(),Name));
  fCanvases.back()->SetOnlineMonitorName(fName);
  fCanvases.back()->SetReferenceFileName(static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetOnlineMonitorReferenceFileName());
  return fCanvases.back();
}

void NA62VOnlineMonitor::Update(Int_t /*BurstID*/) {

  if (fReco && fReco->GetRawDecoder() && fReco->GetRawDecoder()->GetDecoder()) {
    //TH2F * hDigiTimeRaw                = fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw();
    TH2F * hDigiTimeRawFine            = fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine();
    //TH2F * hDigiTimeRawFineVsROChannel = fReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFineVsROChannel();
    TH2F * hDecoderErrors              = fReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors();
    UInt_t nROBoards = fReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
    UInt_t nROMezzaninesPerFullROBoard = fReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();

    //DigiTimeRawFine histogram and projection
    if(fHDigiTimeRawFine1D && hDigiTimeRawFine){
      for(UInt_t iROBoard=0;iROBoard<nROBoards;iROBoard++){
        Int_t iFirstBin = iROBoard*hDigiTimeRawFine->GetNbinsX()/nROBoards+1;
        Int_t iLastBin  = (iROBoard+1)*hDigiTimeRawFine->GetNbinsX()/nROBoards;
        TH1D* hDigiTimeRawFine1D = hDigiTimeRawFine->ProjectionY(Form("%s%sBoard%02d",fReco->GetName().Data(),hDigiTimeRawFine->GetName(),iROBoard),iFirstBin,iLastBin);
        hDigiTimeRawFine1D->SetTitle(Form("%s %s Board%02d",fReco->GetName().Data(),hDigiTimeRawFine->GetName(),iROBoard));
        if(fName == "Spectrometer"){
          hDigiTimeRawFine1D->Rebin(SpectrometerReconstruction::GetRebinFactor_DigiTimeRaw());
        }
        hDigiTimeRawFine1D->Copy(*fHDigiTimeRawFine1D[iROBoard]);
        fHDigiTimeRawFine1D[iROBoard]->SetLineColor(kBlue);
        fHDigiTimeRawFine1D[iROBoard]->SetLineWidth(1);
        fHDigiTimeRawFine1D[iROBoard]->SetFillColor(kOrange-3);
      }
    }

    //Update DIM services
    strcpy(fDecoderErrorsString,"");
    TH1D* hDecoderErrors1D = 0;
    int iLastBin = fReco->GetRawDecoder()->GetDecoder()->GetNCriticalErrorTypes();
    if(hDecoderErrors) hDecoderErrors1D = hDecoderErrors->ProjectionX(Form("%s%s1D",fReco->GetName().Data(),hDecoderErrors->GetName()),1,iLastBin);
    for(UInt_t iBoard=0;iBoard<nROBoards;iBoard++){
      Int_t NErrorsInBoard = 0;
      Int_t ErrorID = 0;
      for(UInt_t iMezzanine=0;iMezzanine<nROMezzaninesPerFullROBoard;iMezzanine++){
        NErrorsInBoard += hDecoderErrors1D->GetBinContent(hDecoderErrors1D->FindBin(iBoard*nROMezzaninesPerFullROBoard+iMezzanine));
      }
      ErrorID = NErrorsInBoard; //temporary
      fDecoderErrorsValues[iBoard] = ErrorID;
      strcat(fDecoderErrorsString,Form("%d",ErrorID));
      if(iBoard!=nROBoards-1) strcat(fDecoderErrorsString,"|");
      if(NErrorsInBoard) fDecoderErrorsMask |= (1<<iBoard);
    }
    delete hDecoderErrors1D;
    std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error string: " << fDecoderErrorsString << std::endl;
    std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error values: ";
    for(UInt_t iVal=0;iVal<nROBoards;iVal++){
      std::cout << fDecoderErrorsValues[iVal];
      if(iVal!=nROBoards-1) std::cout << ",";
    }
    std::cout << std::endl;
    std::cout << "["<< fReco->GetName() << "OnlineMonitor] DIM Error mask:   " << std::hex << fDecoderErrorsMask << std::dec << std::endl;
#ifdef DIM
    fDecoderErrorsStringService->updateService();
    fDecoderErrorsValuesService->updateService();
    fDecoderErrorsMaskService->updateService();
#endif
  }

  // update all the tabs
  for(UInt_t iCanvas = 0; iCanvas < fCanvases.size(); iCanvas++){
    fCanvases[iCanvas]->Update();
  }
}

void NA62VOnlineMonitor::Print(Int_t BurstID) {
  // save tabs as png files
  Int_t  RunID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  time_t BurstTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstTime();
  struct stat filestat;
  const TString OMPlotsDir = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetSaveOnlineMonitorPlotsDir();
  if(stat(OMPlotsDir, &filestat)!=0) mkdir(OMPlotsDir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
  if(stat(OMPlotsDir+Form("/Run%06i",RunID), &filestat)!=0) mkdir(OMPlotsDir+Form("/Run%06i",RunID), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
  for(UInt_t iCanvas = 0; iCanvas < fCanvases.size(); iCanvas++){
    gPad = fCanvases[iCanvas]->GetCanvas(); //needed for print function
    fCanvases[iCanvas]->GetCanvas()->Print(OMPlotsDir+Form("/Run%06i",RunID)+"/"+fReco->GetName()+"_"+fCanvases[iCanvas]->GetName()+"_"+Form("%lu-%06i-%04i",BurstTime,RunID,BurstID)+".png");
  }
}

void NA62VOnlineMonitor::BurstReset(Int_t BurstID) {

  strcpy(fDecoderErrorsString,"");
  fDecoderErrorsMask = 0;

  if(fReco->GetOnlineMonitorAccumulation()<=0) return; //histo are never reset
  if(BurstID % fReco->GetOnlineMonitorAccumulation()) return;

  for(UInt_t iCanvas=0; iCanvas<fCanvases.size(); iCanvas++) fCanvases[iCanvas]->Reset();

}
