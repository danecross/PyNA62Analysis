// Created by Vito Palladino 20/1/2011
// Modified by E.Leonardi 2014/06/13

#ifndef LAVphotocathode_H 
#define LAVphotocathode_H 1

#include "TROOT.h"
#include "TF1.h"

using namespace std;

class LAVPhotocathode{

public:

  explicit LAVPhotocathode(TF1 *);
  ~LAVPhotocathode() {}

  Bool_t ApplyQE(Double_t);

  void SetBlockId(Int_t);

private:

  Int_t fBlockId;
  TF1* fgPhotocathode;
      
};


#endif
