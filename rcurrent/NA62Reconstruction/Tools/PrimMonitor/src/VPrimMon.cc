#include "VPrimMon.hh"
#include "TGFrame.h"
#include "TRootEmbeddedCanvas.h"
#include "TGLayout.h"
#include "TPad.h"
#include "TCanvas.h"

VPrimMon::VPrimMon(TRootBrowser* mainwin, TString Name) {
  fMainWindow = mainwin;
  SetName(Name);
  fSavePng = 0;
  fPlotsWebDir.Clear();
  fMainWindow->StartEmbedding(1,-1);
  fMainTabFrame = new TGMainFrame(gClient->GetRoot(),10,10,kMainFrame | kVerticalFrame);
  fMainTabFrame->SetName(Name);
  fMainTab = new TGTab(fMainTabFrame,300,300);
}

VPrimMon::~VPrimMon(){
  for(UInt_t iCanvas=0;iCanvas<fCanvases.size();iCanvas++) {
    delete fCanvases[iCanvas];
    fCanvases[iCanvas] = 0;
  }
}

void VPrimMon::CompleteTab(){
  fMainTab->SetTab(0);
  fMainTab->Resize();
  fMainTabFrame->AddFrame(fMainTab, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));
  fMainTabFrame->SetMWMHints(kMWMDecorAll, kMWMFuncAll, kMWMInputModeless);
  fMainTabFrame->MapSubwindows();
  fMainTabFrame->Resize();
  fMainTabFrame->MapWindow();
  fMainWindow->StopEmbedding(fName);
}

TCanvas * VPrimMon::AddCanvasTab(TString Name){
  TGCompositeFrame *SubTabCompositeFrame = fMainTab->AddTab(Name);
  SubTabCompositeFrame->SetLayoutManager(new TGVerticalLayout(SubTabCompositeFrame));
  TRootEmbeddedCanvas *RootEmbeddedCanvas = new TRootEmbeddedCanvas(0,SubTabCompositeFrame,300,300);
  SubTabCompositeFrame->AddFrame(RootEmbeddedCanvas, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));
  fCanvases.push_back(RootEmbeddedCanvas->GetCanvas());
  return RootEmbeddedCanvas->GetCanvas();
}

TCanvas * VPrimMon::AddCanvas(TString Name){
  fCanvases.push_back(new TCanvas(Name, Name, 300, 300));
  return fCanvases.back();
}

void VPrimMon::Update(TString filename) {
  // Update all the tabs

  //loop through all canvases
  for(UInt_t iCanvas = 0; iCanvas < fCanvases.size(); iCanvas++){
    
    //get number of primitives in the Canvas
    Int_t NFrames = fCanvases[iCanvas]->GetListOfPrimitives()->GetEntries();

    //loop through all primitives
    for(Int_t iFrame = 0; iFrame < NFrames; iFrame++){
      
      //check that the primitive is a TPad
      TObject* obj = (TObject*)fCanvases[iCanvas]->GetListOfPrimitives()->At(iFrame);
      if( obj->IsA() == TPad::Class()){

	//tell ROOT that the Pad was modified
	TPad* Pad = (TPad*)obj ;      
	if(Pad) Pad->Modified();
      }
    }
    
    // update the canvas and all pads it contains.
    // set canvas to Modified in case there's only one pad on the canvas
    fCanvases[iCanvas]->Modified() ;
    fCanvases[iCanvas]->Update();
    
  }
  // SavePrimitiveMonitorPlots flag in Primitives.conf
  // 1 - saving canvases for every burst. No png files for the web page.
  // 2 - png for every burst and special png files to show on the web page
  // 3 - save only the png files for the web page
  if ( (fSavePng==1 || fSavePng==2) && fCanvases[0] && fCanvases[1] && fCanvases[2]) {
    Printf("printing canvases");
    gPad = fCanvases[0]->GetCanvas(); //needed for print function
    fCanvases[0]->GetCanvas()->SaveAs(Form(filename +  fName +"1.png"),"png");
    gPad = fCanvases[1]->GetCanvas(); 
    fCanvases[1]->GetCanvas()->SaveAs(Form(filename +  fName +"2.png"),"png");
    gPad = fCanvases[2]->GetCanvas(); 
    fCanvases[2]->GetCanvas()->SaveAs(Form(filename +  fName +"3.png"),"png");
  } 
  if ( (fSavePng==2 || fSavePng==3) && fCanvases[0] && fCanvases[1] && fCanvases[2]) {
    Printf("printing canvases for web: "+fPlotsWebDir);
    gPad = fCanvases[1]->GetCanvas(); // use it only when running on na62farmlogin1
    if (fName.Contains("All")) fCanvases[0]->GetCanvas()->SaveAs(Form(fPlotsWebDir +  fName +".png"),"png"); 
    gPad = fCanvases[1]->GetCanvas(); 
    if (fName.Contains("NewCHOD") || fName.Contains("RICH")) fCanvases[1]->GetCanvas()->SaveAs(Form(fPlotsWebDir +  fName +".png"),"png");
    gPad = fCanvases[2]->GetCanvas(); 
    if (fName.Contains("NewCHOD")) fCanvases[2]->GetCanvas()->SaveAs(Form(fPlotsWebDir + "/FFT.png"),"png");
  }
 
}
