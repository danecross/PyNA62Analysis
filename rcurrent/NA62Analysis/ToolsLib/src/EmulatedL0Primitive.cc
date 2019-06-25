// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-07-14
// ---------------------------------------------------------------

/// \class EmulatedL0Primitive
/// \Brief
/// This class is used to act as primitives (or clusters that will become primitives) in the
/// suite of L0Emulators. The class has several constructors. Each constructor corresponds
/// to a different L0 primitive, thus requests different information.
/// \EndBrief
/// \Detailed
/// This class is used to act as primitives (or clusters that will become primitives) in the
/// suite of L0Emulators. The class has several constructors. Each constructor corresponds
/// to a different L0 primitive, thus requests different information.
///
/// The class encodes certain properties of the L0Primitives, for example, that the
/// MUV3/NewCHOD primitives (clusters) can contain up to 15 hits, while the RICH/CHOD
/// primitives can contain up to 255 hits.
///
/// The class contains a map in which the primitive bits (e.g. QX, MO2) can be filled
/// by the L0Emulators.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <iostream>
#include "NA62Global.hh"
#include "EmulatedL0Primitive.hh"

///////////////////////////////////////////////////////////////////////////////////
///// Constructor for LAV12 primitives
///////////////////////////////////////////////////////////////////////////////////
EmulatedL0Primitive::EmulatedL0Primitive(Int_t L0Detector, Double_t time)
  : fL0Detector(L0Detector), fFirstTime(time), fSumDT(0.0), fNHits(1), fPP(0),
    fNInnerTight(0), fNInnerLoose(0), fNOuterTight(0), fNOuterLoose(0), fEnergy(0.0),
    fEdgeU(0.0), fEdgeL(0.0)
{
  fPrimID.clear();
  fQuadrants.clear();
  fQuadrants.resize(4,false);
}

///////////////////////////////////////////////////////////////////////////////////
///// Constructor for RICH and CHOD primitives
///////////////////////////////////////////////////////////////////////////////////
EmulatedL0Primitive::EmulatedL0Primitive(Int_t L0Detector, Double_t time, Int_t pp)
  : fL0Detector(L0Detector), fFirstTime(time), fSumDT(0.0), fNHits(1), fPP(pp),
    fNInnerTight(0), fNInnerLoose(0), fNOuterTight(0), fNOuterLoose(0), fEnergy(0.0),
    fEdgeU(0.0), fEdgeL(0.0)
{
  fPrimID.clear();
  fQuadrants.clear();
  fQuadrants.resize(4,false);
}

///////////////////////////////////////////////////////////////////////////////////
///// Constructor for MUV3 and NewCHOD primitives
///////////////////////////////////////////////////////////////////////////////////
EmulatedL0Primitive::EmulatedL0Primitive(Int_t L0Detector, Double_t time,
					 Bool_t Tight, Bool_t Inner, Int_t Quadrant)
  : fL0Detector(L0Detector), fFirstTime(time), fSumDT(0.0), fNHits(1), fPP(0),
    fNInnerTight(0), fNInnerLoose(0), fNOuterTight(0), fNOuterLoose(0), fEnergy(0.0),
    fEdgeU(0.0), fEdgeL(0.0)
{
  fPrimID.clear();
  fQuadrants.clear();
  fQuadrants.resize(4,false);

  // can only be between 0 and 3. MUV3 inner tiles take value -1
  if(Quadrant>=0 && Quadrant<4) fQuadrants[Quadrant]=true;

  if( Tight && !Inner) fNOuterTight++;
  if( Tight &&  Inner) fNInnerTight++;
  if(!Tight && !Inner) fNOuterLoose++;
  if(!Tight &&  Inner) fNInnerLoose++;
}

///////////////////////////////////////////////////////////////////////////////////
///// Constructor for L0Calo primitives
///////////////////////////////////////////////////////////////////////////////////
EmulatedL0Primitive::EmulatedL0Primitive(Int_t L0Detector, Double_t time, Double_t energy)
  : fL0Detector(L0Detector), fFirstTime(time), fSumDT(0.0), fNHits(1), fPP(0),
    fNInnerTight(0), fNInnerLoose(0), fNOuterTight(0), fNOuterLoose(0), fEnergy(energy),
    fEdgeU(0.0), fEdgeL(0.0)
{
  fPrimID.clear();
  fQuadrants.clear();
  fQuadrants.resize(4,false);
}

EmulatedL0Primitive::~EmulatedL0Primitive(){
  fQuadrants.clear();
  fPrimID.clear();
}

void EmulatedL0Primitive::SetEdges(Double_t Window, Bool_t underflow, Double_t splitTime){
  fEdgeU = fFirstTime+Window;
  fEdgeL = fFirstTime-Window;

  if(!underflow) return;

  // no need to account for 1Unit differences, window was actually 63 units!
  Double_t dtSplit = fFirstTime - splitTime - Window;
  if(dtSplit>=0.0) return;

  // here is the underflow of fEdgeL...
  // The lower edge (edgeL) is the first time, plus the max value of the underflowing
  // 12 bit number (4096-1), PLUS the (must be negative) time difference between the first time and the
  // split time minus the window. This last quantity, dtSplit, says how far the low edge would have been
  // into the previous split, which has now wrapped around to be about 4 splits in the future.
  // cppcheck-suppress redundantAssignment
  fEdgeL = fFirstTime + (4095*TdcCalib) + dtSplit;
}

Bool_t EmulatedL0Primitive::Compare(HitVec::iterator hitit){
  Double_t time = hitit->GetTime();
  if(time>fEdgeL && time<fEdgeU){
    AddToCluster(hitit);
    return true;
  }
  return false;
}

void EmulatedL0Primitive::AddToCluster(HitVec::iterator hitit){

  std::vector<Bool_t> quads;
  std::vector<Int_t> hits;
  hitit->GetHitInfo(quads, hits);

  // number of hits (RICH, CHOD, LAV12)

  if( (fL0Detector == kL0NewCHOD || fL0Detector == kL0MUV3) && fNHits==15){
    // don't increase fNHits or fSumDT above 15 for MUV3 and NewCHOD
  }
  else if(fL0Detector == kL0LAV && fNHits==14){
    return; // don't do anything for LAV if cluster has 14 hits.
  }
  else if( (fL0Detector == kL0RICH || fL0Detector == kL0CHOD) && fNHits==255){
    return ; // don't do anything if N==255 for RICH and CHOD
  }
  else{
    fNHits += hits[0];
    fSumDT += ((hitit->GetTime())-fFirstTime)*hits[0]; // for RICH/CHOD
  }

  // hit types and quadrants (MUV3, NewCHOD)
  fNInnerTight += hits[1];
  fNInnerLoose += hits[2];
  fNOuterTight += hits[3];
  fNOuterLoose += hits[4];

  fQuadrants[0] = fQuadrants[0] || quads[0];
  fQuadrants[1] = fQuadrants[1] || quads[1];
  fQuadrants[2] = fQuadrants[2] || quads[2];
  fQuadrants[3] = fQuadrants[3] || quads[3];

  // energy (L0Calo)
  fEnergy      += hits[5];
}

void EmulatedL0Primitive::GetHitInfo(std::vector<Bool_t>& quads, std::vector<Int_t>& hits){
  quads.clear();
  quads = fQuadrants;

  hits.clear();
  hits.push_back(fNHits);
  hits.push_back(fNInnerTight);
  hits.push_back(fNInnerLoose);
  hits.push_back(fNOuterTight);
  hits.push_back(fNOuterLoose);
  hits.push_back(fEnergy);
}

Double_t EmulatedL0Primitive::GetTime(){
  return fFirstTime;
}

Double_t EmulatedL0Primitive::GetFirstTime() const {
  return fFirstTime;
}

Double_t EmulatedL0Primitive::GetAverageTime(){
  return fFirstTime + (fSumDT/fNHits);
}

Double_t EmulatedL0Primitive::GetSumDT(){
  return fSumDT;
}

Int_t EmulatedL0Primitive::GetNHits(){
  return fNHits;
}

void EmulatedL0Primitive::SetNHits(Int_t nhits){
  fNHits = nhits;
}

Int_t EmulatedL0Primitive::GetPP(){
  return fPP;
}

Int_t EmulatedL0Primitive::GetL0Detector(){
  return fL0Detector;
}

void EmulatedL0Primitive::DropQuadrants(){
  fQuadrants.resize(1);
  fQuadrants.resize(4, false);
}

void EmulatedL0Primitive::SetTimeToAverageTime(){
  fFirstTime = fFirstTime + (fSumDT/fNHits);
  fSumDT = 0.0;
}

void EmulatedL0Primitive::RemoveEventTime(Double_t eventTime){
  fFirstTime -= eventTime;
}

std::map<TString, Bool_t> EmulatedL0Primitive::GetPrimIDs(){
  return fPrimID;
}

void EmulatedL0Primitive::SetPrimID(TString a, Bool_t b){
  fPrimID.insert( std::pair<TString, Bool_t>(a,b) );
}

Bool_t EmulatedL0Primitive::GetPrimID(TString a){
  std::map<TString, Bool_t>::iterator it;
  it = fPrimID.find(a);
  if(it != fPrimID.end()) return it->second;
  else{
    std::cout << "[EmulatedL0Primitive][WARNING] Could not find primitive ID: " << a << std::endl;
    return false;
  }
}

void EmulatedL0Primitive::MergePrimitive(EmulatedL0Primitive& input){
  std::map<TString, Bool_t> inputIDs = input.GetPrimIDs();
  std::map<TString, Bool_t>::iterator it;
  it = inputIDs.begin();
  for(; it!=inputIDs.end(); ++it){
    fPrimID.insert( *it );
  }
}

void EmulatedL0Primitive::PrintPrimitives(){
  std::map<TString, Bool_t>::iterator it;
  it = fPrimID.begin();
  for( ; it != fPrimID.end(); ++it){
    std::cout << " Prim name " << it->first << " Value " << it->second << std::endl;
  }
}

void EmulatedL0Primitive::PrintInfo(){
  std::cout << "===============" << std::endl;
  std::cout << " Emulated primitive for detector "
	    << fL0Detector << std::endl;
  std::cout << " First time: " << fFirstTime << " N hits: " << fNHits
	    << " sum_dt: " << fSumDT
	    << " average time: " << this->GetAverageTime() << std::endl;
  PrintPrimitives();
}
