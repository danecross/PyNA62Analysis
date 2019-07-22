// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-02-12
//
// ---------------------------------------------------------------

#include "BeamData.hh"

ClassImp(BeamData)

BeamData::BeamData(): TObject(){
  Clear();
}

BeamData::BeamData(const BeamData& c) : TObject(c), fInstantaneousIntensity(c.fInstantaneousIntensity),fInstantaneousIntensityError(c.fInstantaneousIntensityError){}

BeamData::~BeamData(){
}

void BeamData::Clear(Option_t* /*option*/){
  fInstantaneousIntensity.clear();
  fInstantaneousIntensityError.clear();
}
