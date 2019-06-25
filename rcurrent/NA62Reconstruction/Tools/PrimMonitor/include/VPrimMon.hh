#ifndef VPrimMon_H
#define VPrimMon_H

#include "TRootBrowser.h"
#include "TEveManager.h"
#include "TEveBrowser.h"
#include "TGFrame.h"
#include "TGTab.h"
#include <vector>

#include "TString.h"
#include "TCanvas.h"

using namespace std;

class VPrimMon
{

public:

  VPrimMon(TRootBrowser*, TString);
  ~VPrimMon();
  void         Update(TString filename);
  //void BurstReset(Int_t) = 0;

public:

  TString      GetName()  { return fName; };
  void         SetName(TString value)  { fName = value; };
  void         SetSavePngFiles(Int_t value)  { fSavePng = value; };
  void         SetPngFilesWebDir(TString value)  { fPlotsWebDir = value; };
  TCanvas     *AddCanvasTab(TString);
  TCanvas     *AddCanvas(TString);
 
protected:
        
  void         CompleteTab();
  TGMainFrame *fMainTabFrame;
  TGTab       *fMainTab;
  vector<TCanvas*> fCanvases;
  TString      fName;
  TString      fPlotsWebDir;
  TRootBrowser* fMainWindow;
  Int_t       fSavePng;
};

#endif
