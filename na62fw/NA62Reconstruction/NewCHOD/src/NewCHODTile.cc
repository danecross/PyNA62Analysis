//----------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
//----------------------------------------------------------------
/// \class NewCHODTile
/// \Brief
/// NewCHOD tile
/// \EndBrief
/// \Detailed
/// Contains the IDs of the two channels reading the tile, monitoring histograms, etc.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "NewCHODTile.hh"
#include "Riostream.h"

NewCHODTile::NewCHODTile(Int_t TileID, Int_t PositionID1, Int_t PositionID2,
		   Int_t ROID1, Int_t ROID2, Bool_t Enabled, Bool_t FillHistograms) :
  fTileID        (TileID),
  fPositionID1   (PositionID1),
  fPositionID2   (PositionID2),
  fROID1         (ROID1),
  fROID2         (ROID2),
  fEnabled       (Enabled),
  fFillHistograms(FillHistograms),
  fHDeltaTime    (nullptr),
  fHTime2D       (nullptr)
{
  if (fFillHistograms) InitHistograms();
}

NewCHODTile::~NewCHODTile() {
  if (fHDeltaTime) delete fHDeltaTime;
  if (fHTime2D)    delete fHTime2D;
  fHDeltaTime = 0;
  fHTime2D    = 0;
}

void NewCHODTile::InitHistograms() {
  if (!fFillHistograms) return;

  TString Name1 = Form("Tile %03d dT", fTileID);
  fHDeltaTime = new TH1D (Name1, Name1, 410, -205.5*TdcCalib, 204.5*TdcCalib);
  fHDeltaTime->GetXaxis()->SetTitle("Time(high PMT) - Time(low PMT), ns");

  TString Name2 = Form("Tile %03d T1,T2", fTileID);
  fHTime2D = new TH2F (Name2, Name2, 100, -20, +20, 100, -20, 20);
  fHTime2D->GetXaxis()->SetTitle("Time (low PMT), ns");
  fHTime2D->GetYaxis()->SetTitle("Time (high PMT), ns");
}

void NewCHODTile::FillDeltaTime (Double_t t1, Double_t t2) {
  // t1: for low number PMT, t2: for high number PMT
  if (!fFillHistograms) return;
  if (!fEnabled) return;
  fHDeltaTime->Fill(t2-t1);
  fHTime2D->Fill(t1, t2);
}

void NewCHODTile::Print() {
  std::cout <<"Geom IDs: "<<fPositionID1<<" "<<fPositionID2<<
    "; RO IDs: "<<fROID1<<" "<<fROID2<< std::endl;
}

/////////////////////////
// Save output histograms

void NewCHODTile::Write (TFile* HistoFile) {
  if (!fFillHistograms) return;
  HistoFile->cd("NewCHODMonitor/NewCHODTiles/dT");
  fHDeltaTime->Write();
  HistoFile->cd("NewCHODMonitor/NewCHODTiles/Time2D");
  fHTime2D->Write();
}
