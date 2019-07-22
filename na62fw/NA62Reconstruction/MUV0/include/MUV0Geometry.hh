#ifndef MUV0Geometry_H
#define MUV0Geometry_H 1

#include "TVector3.h"
#include "TROOT.h"
#include "Riostream.h"

class MUV0Geometry
{

public:

  MUV0Geometry();
  static MUV0Geometry* GetInstance();

  Double_t GetTileXmin(Int_t);
  Double_t GetTileXmax(Int_t);
  Double_t GetTileYmin(Int_t);
  Double_t GetTileYmax(Int_t);
  Double_t GetTileCenterX(Int_t);
  Double_t GetTileCenterY(Int_t);

  Int_t GetTileID(TVector2);
  Int_t GetTileID(Double_t x, Double_t y) {return GetTileID(TVector2(x, y));}

private:

  static MUV0Geometry* fInstance;
  void CreateGeometry();

  Double_t TileXmin[9]; // Tile x min
  Double_t TileXmax[9]; // Tile x max
  Double_t TileYmin[9]; // Tile y min
  Double_t TileYmax[9]; // Tile y max

  Double_t XCenter; // The RR should not touch RICHMirrorWindowOuterFlange (R<1510mm)
  Double_t XLength;
  Double_t YLength;
  Double_t TileCenterX[9];
  Double_t TileCenterY[9];
  Double_t ScintillatorThickness;
  Double_t DetectorXPosition;
  Double_t DetectorYPosition;
  Double_t DetectorZPosition;

  Double_t FrameInnerSize;
  Double_t FrameThickness;
  Double_t CoverThickness;

};
#endif
