// -------------------------------------------------------------//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-27
//
// -------------------------------------------------------------//

#ifndef MUV3Tile_H
#define MUV3Tile_H 1

#include "NA62Global.hh"
#include "TFile.h"
#include "TH1D.h"
#include "TH2F.h"

class MUV3Tile {

public:

  MUV3Tile(Int_t, Int_t, Int_t, Int_t, Int_t, Bool_t, Bool_t);
  ~MUV3Tile();

  void InitHistograms();
  void FillDeltaTime(Double_t, Double_t);
  void Write(TFile*);

  Int_t    GetTileID()               { return fTileID;      }
  void     SetTileID(Int_t val)      { fTileID = val;       }
  Int_t    GetPositionID1()          { return fPositionID1; }
  void     SetPositionID1(Int_t val) { fPositionID1 = val;  }
  Int_t    GetPositionID2()          { return fPositionID2; }
  void     SetPositionID2(Int_t val) { fPositionID2 = val;  }
  Int_t    GetROID1()                { return fROID1;       }
  void     SetROID1(Int_t val)       { fROID1 = val;        }
  Int_t    GetROID2()                { return fROID2;       }
  void     SetROID2(Int_t val)       { fROID2 = val;        }
  Int_t    GetHVGroup()              { return fHVGroup;     }
  void     SetHVGroup(Int_t val)     { fHVGroup = val;      }
  Double_t GetT0()                   { return fT0;          }
  void     SetT0(Double_t val)       { fT0 = val;           }
  void     Print();

private:

  Int_t    fTileID;
  Int_t    fPositionID1, fPositionID2, fROID1, fROID2, fHVGroup;
  Bool_t   fEnabled, fFillHistograms;
  Double_t fT0;         ///< T0 correction to be subtracted from the "raw" candidate time
  TH1D*    fHDeltaTime; ///< Differences of channel times (for the pdf report)
  TH2F*    fHTime2D;    ///< Correlations of channel times (for the pdf report)

//   ClassDef(MUV3Tile,1);
};

#endif
