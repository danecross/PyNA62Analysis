#ifndef TH2Sparse_H
#define TH2Sparse_H 1

#include "THnSparse.h"

template <class CONT>
class TH2SparseT : public THnSparseT<CONT> {
private:

  Int_t dim;

 public:
   TH2SparseT() {}
   TH2SparseT(const char* name, const char* title,
              const Int_t* nbins, const Double_t* xmin = 0,
              const Double_t* xmax = 0, Int_t chunksize = 1024 * 16): 
     THnSparseT<CONT>(name, title, 3, nbins, xmin, xmax, chunksize) {}

  Long_t Fill(Double_t x, Double_t y, Double_t weight = 1.) {
    Double_t tx[3]={x,y,0};
    return THnSparseT<CONT>::Fill(tx, weight);
  }

};

typedef TH2SparseT<TArrayD> TH2SparseD;
typedef TH2SparseT<TArrayF> TH2SparseF;
typedef TH2SparseT<TArrayL> TH2SparseL;
typedef TH2SparseT<TArrayI> TH2SparseI;
typedef TH2SparseT<TArrayS> TH2SparseS;
typedef TH2SparseT<TArrayC> TH2SparseC;

#endif
