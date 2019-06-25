// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
//----------------------------------------------------------------

#ifndef NewCHODTile_H
#define NewCHODTile_H 1

#include "NA62Global.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TFile.h"

class NewCHODTile {

public:

  NewCHODTile(Int_t, Int_t, Int_t, Int_t, Int_t, Bool_t, Bool_t);
  ~NewCHODTile();

  void InitHistograms();
  void FillDeltaTime(Double_t, Double_t);
  void Write(TFile*);

  Int_t GetTileID()               { return fTileID;      }
  void  SetTileID(Int_t val)      { fTileID = val;       }
  Int_t GetPositionID1()          { return fPositionID1; }
  void  SetPositionID1(Int_t val) { fPositionID1 = val;  }
  Int_t GetPositionID2()          { return fPositionID2; }
  void  SetPositionID2(Int_t val) { fPositionID2 = val;  }
  Int_t GetROID1()                { return fROID1;       }
  void  SetROID1(Int_t val)       { fROID1 = val;        }
  Int_t GetROID2()                { return fROID2;       }
  void  SetROID2(Int_t val)       { fROID2 = val;        }
  void  Print();
  
private:

  Int_t    fTileID;
  Int_t    fPositionID1, fPositionID2, fROID1, fROID2;
  Bool_t   fEnabled, fFillHistograms;
  TH1D     *fHDeltaTime;
  TH2F     *fHTime2D;
};

#endif
