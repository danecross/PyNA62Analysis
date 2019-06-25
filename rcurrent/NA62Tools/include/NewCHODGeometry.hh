// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#ifndef NewCHODGeometry_H
#define NewCHODGeometry_H 1

#include "TVector3.h"

class NewCHODGeometry {

public:
  NewCHODGeometry();
  static NewCHODGeometry* GetInstance();

  Double_t GetBrickCentreX(Int_t);
  Double_t GetBrickCentreY(Int_t);
  
  Double_t GetTileXmin    (Int_t);
  Double_t GetTileXmax    (Int_t);
  Double_t GetTileYmin    (Int_t);
  Double_t GetTileYmax    (Int_t);
  Double_t GetTileCentreX (Int_t);
  Double_t GetTileCentreY (Int_t);

  Int_t GetScintMap (Int_t i) { return fScintMap[i]; }
  Int_t GetTileID   (TVector2);
  Int_t GetTileID   (Double_t x, Double_t y) { return GetTileID(TVector2(x, y)); }

private:

  void CreateGeometry();
  static NewCHODGeometry* fInstance;

  Double_t fBrickSizeX, fBrickSizeY;
  Double_t fInnerRadius, fOuterRadius;
  Int_t fScintMap[100];   ///< Tile geometrical IDs corresponding to each brick ID
  Double_t fTileX[38];    ///< Tile x centre: defined arbitrarily for irregular tiles
  Double_t fTileY[38];    ///< Tile x centre: defined arbitrarily for irregular tiles
  Double_t fTileXmin[38]; ///< Tile x min (quadrant 1) assuming rectangular tiles
  Double_t fTileXmax[38]; ///< Tile x max (quadrant 1) assuming rectangular tilesa
  Double_t fTileYmin[38]; ///< Tile y min (quadrant 1) assuming rectangular tiles
  Double_t fTileYmax[38]; ///< Tile y max (quadrant 1) assuming rectangular tiles
};

#endif
