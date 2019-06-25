// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-07-14
// ---------------------------------------------------------------

#ifndef EMULATEDL0PRIMITIVE_HH
#define EMULATEDL0PRIMITIVE_HH

#include "TString.h"
#include <map>

class EmulatedL0Primitive;
typedef std::vector<EmulatedL0Primitive> HitVec;

class EmulatedL0Primitive{
  
public:
  
  EmulatedL0Primitive(Int_t L0Detector, Double_t time);
  EmulatedL0Primitive(Int_t L0Detector, Double_t time, Int_t pp); // must use int !!
  EmulatedL0Primitive(Int_t L0Detector, Double_t time,
					 Bool_t Tight, Bool_t Inner, Int_t Quadrant);
  EmulatedL0Primitive(Int_t L0Detector, Double_t time, Double_t energy);
  ~EmulatedL0Primitive();

  Double_t GetTime();
  Double_t GetFirstTime() const;

  Double_t GetAverageTime();
  Double_t GetSumDT();
  Int_t    GetNHits();
  void     SetNHits(Int_t nhits);
  Int_t    GetPP();
  Int_t    GetL0Detector();

  void DropQuadrants();
  void SetTimeToAverageTime();
  void RemoveEventTime(Double_t eventTime);

  Bool_t GetPrimID(TString a);
  void SetPrimID(TString, Bool_t);
  std::map<TString, Bool_t> GetPrimIDs();
  void MergePrimitive(EmulatedL0Primitive& input);

  void SetEdges(Double_t L0Window, Bool_t underflow=false, Double_t splitTime=0.0);
  Bool_t Compare(HitVec::iterator hitit);
  void AddToCluster(HitVec::iterator hitit);

  void GetHitInfo(std::vector<Bool_t>& quads, std::vector<Int_t>& hits);

  void PrintPrimitives();
  void PrintInfo();
  
private:
  
  Int_t    fL0Detector;
  Double_t fFirstTime;
  Double_t fSumDT;
  Int_t    fNHits;
  Int_t    fPP;
  Int_t    fNInnerTight;
  Int_t    fNInnerLoose;
  Int_t    fNOuterTight;
  Int_t    fNOuterLoose;
  Double_t fEnergy;
  std::vector<Bool_t> fQuadrants;
  std::map<TString, Bool_t> fPrimID;

  Double_t fEdgeU;
  Double_t fEdgeL;
};

#endif
