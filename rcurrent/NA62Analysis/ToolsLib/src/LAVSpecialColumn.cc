#include <cmath>
using namespace std;

#include <Rtypes.h>
#include "LAVSpecialColumn.hh"
#include "LAVConst.hh"


LAVSpecialColumn::LAVSpecialColumn(Int_t Column) {

  fLAVGeometry = LAVStaticGeometry::GetInstance(); 

  fStation = Column/100;
  fPosition = Column%100;
  fNLayers = fLAVGeometry->LayersPerStation(fStation);
  fNPositions = 4*fLAVGeometry->BananasPerLayer(fStation);
  
  for (Int_t iLayer = 0; iLayer < fNLayers; iLayer++) {

    fColumn[iLayer].TAvg = 1e30;
    fColumn[iLayer].DT = 1e30;
    
    for (Int_t iBlock = 0; iBlock < 3; iBlock++) {
      fColumn[iLayer].Block[iBlock].RefLayer = -1;
      fColumn[iLayer].Block[iBlock].RefPosition = -1;
      fColumn[iLayer].Block[iBlock].Type = 0;
      fColumn[iLayer].Block[iBlock].Lead = 0;
      fColumn[iLayer].Block[iBlock].nLead = 0;
      fColumn[iLayer].Block[iBlock].nTrail = 0;
      fColumn[iLayer].Block[iBlock].ToT = 0;
    }

  }  

}


void LAVSpecialColumn::AddHit(Int_t Flag, Int_t Layer, Int_t Step, Int_t Type, Double_t Lead, Double_t ToT) {

  Int_t nStore, iStore[2], iValue[2];
  
  if (Flag == 0) {
    nStore = 1;
    iStore[0] = 0;
    iValue[0] = Layer;
  } else {
    if (Layer == 0) {
      nStore = 1;
      if (Step != 0) { // Called to store block on first layer as control block for last layer in adjacent column
	iStore[0] = 2;
	iValue[0] = fNLayers - 1;
      } else {
        iStore[0] = 1;
	iValue[0] = Layer + 1;
      }
    } else if (Layer == fNLayers - 1) {
      nStore = 1;
      if (Step != 0) { // Called to store block on last layer as control block for first layer in adjacent column
	iStore[0] = 1;
	iValue[0] = 0;
      } else {
	iStore[0] = 2;
	iValue[0] = Layer - 1;
      }
    } else {
      nStore = 2;
      iStore[0] = 1;
      iValue[0] = Layer + 1;
      iStore[1] = 2;
      iValue[1] = Layer - 1;
    }
  }
  
  for (Int_t i = 0; i < nStore; i++) { 

    Int_t iLayer = iValue[i];
    Int_t iBlock = iStore[i];

    Int_t StoredType = fColumn[iLayer].Block[iBlock].Type;
    Bool_t Earlier = Lead < fColumn[iLayer].Block[iBlock].Lead;

    fColumn[iLayer].Block[iBlock].RefLayer = Layer;
    fColumn[iLayer].Block[iBlock].RefPosition = fPosition - Step;

    if ((StoredType == 3 && (Type == 3 && Earlier)) || 
	(StoredType == 1 && (Type == 3 || (Type == 1 && Earlier))) ||
	(StoredType == 2 && (Type == 3 || Type == 1 || (Type == 2 && Earlier))) ||
	(StoredType == 0)) {
      fColumn[iLayer].Block[iBlock].Type = Type;
      fColumn[iLayer].Block[iBlock].Lead = Lead;
      if ((Type & 3) == 3) fColumn[iLayer].Block[iBlock].ToT = ToT;
    }

    if (Type & 1) fColumn[iLayer].Block[iBlock].nLead++;
    if (Type & 2) fColumn[iLayer].Block[iBlock].nTrail++;

  }

}
  

Bool_t LAVSpecialColumn::IsGood(Int_t Layer) {

  if (fColumn[Layer].Block[1].Type == 3 && fColumn[Layer].Block[2].Type == 3) {

    Double_t T1 = fColumn[Layer].Block[1].Lead;
    Double_t T2 = fColumn[Layer].Block[2].Lead;
    fColumn[Layer].TAvg = 0.5*(T1 + T2);
    fColumn[Layer].DT = T2 - T1;

    return (fColumn[Layer].TAvg > T1_ACCIDENTAL && fColumn[Layer].TAvg < T2_ACCIDENTAL && abs(fColumn[Layer].DT) < DT_MAX_COLUMN);

  }

  return kFALSE;

}


Bool_t LAVSpecialColumn::IsFound(Int_t Layer, Int_t TypeMask) {

  if ((fColumn[Layer].Block[0].Type & TypeMask) == TypeMask) {
    if (abs(fColumn[Layer].Block[0].Lead - fColumn[Layer].TAvg) < DT_MAX_HIT) return kTRUE;
  }

  return kFALSE;
  
}
