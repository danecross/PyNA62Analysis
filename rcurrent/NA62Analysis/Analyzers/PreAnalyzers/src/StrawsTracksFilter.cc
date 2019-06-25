#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "StrawsTracksFilter.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;



StrawsTracksFilter::StrawsTracksFilter(Core::BaseAnalysis *ba) : Analyzer(ba, "StrawsTracksFilter")
{
  fAlgoFilter = new StrawsTracksFilterAlgo(ba,this,"MyFilter");
  //Algorithm algo(ba, this,""); //FIXME temporary fix
}

void StrawsTracksFilter::InitOutput(){
}

void StrawsTracksFilter::InitHist(){
}

void StrawsTracksFilter::DefineMCSimple(){
}

void StrawsTracksFilter::StartOfRunUser(){
}

void StrawsTracksFilter::StartOfBurstUser(){
}

void StrawsTracksFilter::Process(int ){
  if (!GetIsTree()) return;
  fAlgoFilter->FilterGoodTrack();
}

void StrawsTracksFilter::PostProcess(){

}

void StrawsTracksFilter::EndOfBurstUser(){
}

void StrawsTracksFilter::EndOfRunUser(){

}

void StrawsTracksFilter::EndOfJobUser(){
  if (!GetIsTree()) return;
fAlgoFilter->SaveAllPlots();
}

void StrawsTracksFilter::DrawPlot(){
}

StrawsTracksFilter::~StrawsTracksFilter(){
}
