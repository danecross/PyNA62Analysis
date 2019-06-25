#ifndef MUV3Geometry_H
#define MUV3Geometry_H 1

#include "TVector2.h"
#include <vector>

class MUV3Geometry {

public:

  MUV3Geometry();
  static MUV3Geometry* GetInstance();

  Double_t GetTileCentreX(Int_t i)
  { return (i>=0 && i<=151 && i!=65 && i!=66 && i!=77 && i!=78) ?
      fTileCentreX[i] : 9999.; }
  Double_t GetTileCentreY(Int_t i)
  { return (i>=0 && i<=151 && i!=65 && i!=66 && i!=77 && i!=78) ?
      fTileCentreY[i] : 9999.; }

  Double_t GetPMOffsetX (Int_t, Int_t);
  Double_t GetPMOffsetY (Int_t, Int_t);
  TVector2 GetTileCentre(Int_t i)
  { return TVector2(GetTileCentreX(i), GetTileCentreY(i)); }
  Int_t  GetTileID(TVector2);
  Bool_t FindNeighbours(Int_t iTile1, Int_t iTile2); ///< Check if a pair of OUTER tiles are neighbours
  Bool_t IsRight(Int_t iTileID); ///< Check if an OUTER tile has x>0
  Bool_t LeftRight(Int_t iTile1, Int_t iTile2); ///< Check for a left-right pair of tiles
  Bool_t LeftRight(std::vector<Int_t> &vTileIDs);

private:
  void CreateGeometry();

  static MUV3Geometry* fInstance;
  Double_t fStandardSize;     ///< Linear size of a large pad
  Double_t fGapWidth;         ///< Vertical Gap width
  Double_t fTileCentreX[152]; ///< Tile centres X
  Double_t fTileCentreY[152]; ///< Tile centres Y
  Double_t fPMCentreX[152];
  Double_t fPMCentreY[152];
  Double_t fPMOffsetXG[2];
  Double_t fPMOffsetYG[2];
  Double_t fPMOffsetXP[2][4];
  Double_t fPMOffsetYP[2][4];
};

#endif
