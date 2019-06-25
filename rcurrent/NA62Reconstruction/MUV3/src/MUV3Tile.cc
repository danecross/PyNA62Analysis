// -------------------------------------------------------------//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-27
//
// -------------------------------------------------------------//
/// \class MUV3Tile
/// \Brief
/// MUV3 tile
/// \EndBrief
/// \Detailed
/// Contains the IDs of the two channels reading the tile, the T0 correction, monitoring histograms, etc.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "Riostream.h"
#include "MUV3Tile.hh"

MUV3Tile::MUV3Tile(Int_t TileID, Int_t PositionID1, Int_t PositionID2,
		   Int_t ROID1, Int_t ROID2, Bool_t Enabled, Bool_t FillHistograms) :
  fTileID(TileID), fPositionID1(PositionID1), fPositionID2(PositionID2),
  fROID1(ROID1), fROID2(ROID2), fEnabled(Enabled), fFillHistograms(FillHistograms), fT0(0.200) {

  // High voltage supply group: from 0 to 9. There are up to 32 channels per group.
  if (TileID<144) fHVGroup = ((TileID%12)/4) + 3*(TileID/48);
  else fHVGroup = 9;

  fHDeltaTime = nullptr;
  fHTime2D    = nullptr;

  if (fFillHistograms) InitHistograms();
}

MUV3Tile::~MUV3Tile() {
  if (fHDeltaTime) delete fHDeltaTime;
  if (fHTime2D)    delete fHTime2D;
  fHDeltaTime = 0;
  fHTime2D    = 0;
}

void MUV3Tile::InitHistograms() {
  if (!fFillHistograms) return;

  TString Name1 = Form("Tile %03d dT", fTileID);
  fHDeltaTime = new TH1D (Name1, Name1, 410, -205.5*TdcCalib, 204.5*TdcCalib);
  fHDeltaTime->GetXaxis()->SetTitle("Time(high PMT) - Time(low PMT), ns");

  TString Name2 = Form("Tile %03d T1,T2", fTileID);
  fHTime2D = new TH2F (Name2, Name2, 80, -20, +20, 80, -20, 20);
  fHTime2D->GetXaxis()->SetTitle("Time (low PMT), ns");
  fHTime2D->GetYaxis()->SetTitle("Time (high PMT), ns");
}

void MUV3Tile::FillDeltaTime(Double_t t1, Double_t t2) {
  // t1: for low number PMT, t2: for high number PMT
  if (!fFillHistograms) return;
  if (!fEnabled) return;
  fHDeltaTime->Fill(t2-t1);
  fHTime2D->Fill(t1, t2);
}

void MUV3Tile::Print() {
  std::cout <<"Geom IDs: "<<fPositionID1<<" "<<fPositionID2<<
    "; RO IDs: "<<fROID1<<" "<<fROID2<< std::endl;
}

/////////////////////////
// Save output histograms

void MUV3Tile::Write(TFile* HistoFile) {
  if (!fFillHistograms) return;
  HistoFile->cd("MUV3Monitor/MUV3Tiles/dT");
  fHDeltaTime->Write();
  HistoFile->cd("MUV3Monitor/MUV3Tiles/Time2D");
  fHTime2D->Write();
}
