#ifndef LAVRecoBlock_H
#define LAVRecoBlock_H 1

#define MAX_RECOHITS_PER_BLOCK 1000

#include "iostream"
#include "LAVRecoHit.hh"
#include "TLAVDigi.hh"

#include <vector>
#include <algorithm>

using namespace std;

struct Edge{
  Int_t Threshold;
  Int_t Type;
  Double_t Time;
  TLAVDigi* Digi;
};

struct myComparison{
  Bool_t operator() (Edge edgeL, Edge edgeR) {return (edgeL.Time < edgeR.Time);};
};

class LAVRecoBlock{

public:

  explicit LAVRecoBlock(Int_t); // pass the BlockID without trheshold identifier
  ~LAVRecoBlock();

  void SetEdge(Int_t threshold, Int_t type, Double_t time, TLAVDigi* digi){Edge edge; edge.Threshold = threshold; edge.Type = type; edge.Time = time; edge.Digi = digi; fEdgeCollection.push_back(edge);}
  void SortEdges(){std::sort(fEdgeCollection.begin(), fEdgeCollection.end(), EdgeComparison); fEdgesAreSorted = 1;}
  void Print();
  void CreateHits();
  void ChargeReconstruct();

  Int_t GetBlockID(){return fBlockID;}
  Int_t GetNRecoHits(){return fNHits;}
  LAVRecoHit* GetRecoHit(Int_t iHit){if (iHit>=0 && iHit<fNHits){return fRecoHits[iHit];} else {return NULL;}} 


protected:
  myComparison  EdgeComparison;
  void CloseHit();
  Bool_t OpenHit();

  Int_t fBlockID;

  typedef std::vector<Edge> EdgeCollection;
  EdgeCollection fEdgeCollection;

  Bool_t fEdgesAreSorted;
  Int_t fNHits;
  Bool_t fOverFlow;
  Bool_t fOpenHit;
  LAVRecoHit* fRecoHits[MAX_RECOHITS_PER_BLOCK]; 
};

#endif

