#ifndef TH1Sparse_H
#define TH1Sparse_H 1

#include "THnSparse.h"

template <class CONT>
class TH1SparseT : public THnSparseT<CONT> {
private:

  Int_t dim;

 public:
   TH1SparseT() {}
   TH1SparseT(const char* name, const char* title,
              const Int_t* nbins, const Double_t* xmin = 0,
              const Double_t* xmax = 0, Int_t chunksize = 1024 * 16): 
     THnSparseT<CONT>(name, title, 2, nbins, xmin, xmax, chunksize) {}

  Long_t Fill(Double_t x, Double_t weight = 1.) {
    Double_t tx[2]={x,0};
    return THnSparseT<CONT>::Fill(tx, weight);
  }

};

typedef TH1SparseT<TArrayD> TH1SparseD;
typedef TH1SparseT<TArrayF> TH1SparseF;
typedef TH1SparseT<TArrayL> TH1SparseL;
typedef TH1SparseT<TArrayI> TH1SparseI;
typedef TH1SparseT<TArrayS> TH1SparseS;
typedef TH1SparseT<TArrayC> TH1SparseC;

#endif
