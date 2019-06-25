#ifndef LAVSpecialColumn_H
#define LAVSpecialColumn_H

#include <Rtypes.h>
#include "LAVStaticGeometry.hh"


class LAVSpecialColumn {

public:

  explicit LAVSpecialColumn(Int_t Column);
  Int_t GetStation() {return fStation;}
  Int_t GetPosition() {return fPosition;}
  void AddHit(Int_t Threshold, Int_t Layer, Int_t Step, Int_t Type, Double_t Lead, Double_t ToT);
  Bool_t IsGood(Int_t Layer);
  Bool_t IsFound(Int_t Layer, Int_t TypeMask);

private:

  LAVStaticGeometry* fLAVGeometry;

  Int_t fStation;
  Int_t fPosition;
  Int_t fNLayers;
  Int_t fNPositions;

  struct EffColumn {

    Double_t TAvg;
    Double_t DT;
    
    struct EffColumnBlock {
      Int_t RefLayer;
      Int_t RefPosition;
      Int_t Type;
      Double_t Lead;
      Double_t ToT;
      Int_t nLead;
      Int_t nTrail;
    } Block[3];
    
  } fColumn[MAX_LAYER];
    
};

#endif
