// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "LKrCoarseT0.hh"
#include "LKrBadCells.hh"

LKrCoarseT0::LKrCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "LKr") {
  fNROMezzaninesPerFullBoard = 16; // CREAM-readout
}

// redefine EndOfJobUser to spot and mask hot cells before the CoarseT0 evaluation
void LKrCoarseT0::EndOfJobUser(){

  std::vector<LKrCell> HotCells;
  HotCells.clear();

  // Get list of Hot cells
  Int_t BadCellsMask = *(Int_t*) GetOutput("LKrBadCells.BadCellsMask");
  std::vector<LKrCell> HotCellsFromHitMap          = *(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromHitMap");
  std::vector<LKrCell> HotCellsFromQualityWarnings = *(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromQualityWarnings");
  std::vector<LKrCell> HotCellsFromPedestals       = *(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromPedestals");

  if(BadCellsMask&(1<<LKrBadCells::kHotHitMapBit)) {
    for(UInt_t iCell=0;iCell<HotCellsFromHitMap.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<HotCells.size();jCell++){
        if(HotCells[jCell].x==HotCellsFromHitMap[iCell].x && HotCells[jCell].y==HotCellsFromHitMap[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) HotCells.push_back(HotCellsFromHitMap[iCell]); //add bad cell
    }
  }
  if(BadCellsMask&(1<<LKrBadCells::kHotQualityWarningsBit)) {
    for(UInt_t iCell=0;iCell<HotCellsFromQualityWarnings.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<HotCells.size();jCell++){
        if(HotCells[jCell].x==HotCellsFromQualityWarnings[iCell].x && HotCells[jCell].y==HotCellsFromQualityWarnings[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) HotCells.push_back(HotCellsFromQualityWarnings[iCell]); //add bad cell
    }
  }
  if(BadCellsMask&(1<<LKrBadCells::kHotPedestalsBit)) {
    for(UInt_t iCell=0;iCell<HotCellsFromPedestals.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<HotCells.size();jCell++){
        if(HotCells[jCell].x==HotCellsFromPedestals[iCell].x && HotCells[jCell].y==HotCellsFromPedestals[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) HotCells.push_back(HotCellsFromPedestals[iCell]); //add bad cell
    }
  }

  // Retrieve the ROCh for each hot cell
  std::vector<Bool_t> BadChannelFound(512,false);
  std::vector<UInt_t> BadROChannels;
  LKrChannelID* LKrChID = new LKrChannelID();
  for(UInt_t iCell=0; iCell<HotCells.size(); iCell++){
    LKrChID->SetXCellID(HotCells[iCell].x);
    LKrChID->SetYCellID(HotCells[iCell].y);
    Int_t ChID = LKrChID->EncodeChannelID();
    BadROChannels.push_back(fChannelRO[ChID]);
    BadChannelFound[fChannelRO[ChID]/32] = true;
  }
  delete LKrChID;

  //-------------- Exclude any bad channels from the histos for the Coarse-T0 evaluation --------------//
  if(HotCells.size()){
    const Int_t NEntries = fHDigiTimeRawFine->GetEntries();
    for(UInt_t iMezzanine=0;iMezzanine<512;iMezzanine++){
      if(!BadChannelFound[iMezzanine]) continue;
      Int_t ixBin = fHDigiTimeRawFine->GetXaxis()->FindBin(iMezzanine);
      // Remove the bin contents of the mezzanines corresponding to the bad channels 
      for(Int_t iyBin=1;iyBin<=fHDigiTimeRawFine->GetNbinsY();iyBin++) fHDigiTimeRawFine->SetBinContent(ixBin,iyBin,0.);
      // Fill the bin with the other good channels of the mezzanine
      for(UInt_t iCh=0;iCh<32;iCh++){
        Bool_t BadROCh = false;
        Int_t ixChBin = fHDigiTimeRawFineVsROChannel->GetXaxis()->FindBin(iMezzanine*32+iCh);
        for(UInt_t iBadCh=0;iBadCh<BadROChannels.size();iBadCh++) {
          if(iMezzanine*32+iCh==BadROChannels[iBadCh]) {
            BadROCh = true;
            break;
          }
        }
        if(BadROCh) continue;
        for(Int_t iyChBin=1;iyChBin<=fHDigiTimeRawFineVsROChannel->GetNbinsY();iyChBin++){ 
          Int_t iyBin = fHDigiTimeRawFine->GetYaxis()->FindBin(fHDigiTimeRawFineVsROChannel->GetYaxis()->GetBinCenter(iyChBin)); 
          fHDigiTimeRawFine->SetBinContent(ixBin,iyBin,fHDigiTimeRawFine->GetBinContent(ixBin,iyBin)+fHDigiTimeRawFineVsROChannel->GetBinContent(ixChBin,iyChBin));
        }
      }
    }
    fHDigiTimeRawFine->SetEntries(NEntries); // restore original number of entries
  }
  //---------------------------------------------------------------------------------------------------//
  
  CoarseT0Evaluation::EndOfJobUser();
}
