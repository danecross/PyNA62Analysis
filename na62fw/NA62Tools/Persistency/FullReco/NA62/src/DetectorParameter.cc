// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "DetectorParameter.hh"

#include "Riostream.h"


ClassImp(DetectorParameter)

DetectorParameter::DetectorParameter(const char* Name, const char* Value, const char* Description, TObjArray Data) : 
  fName(Name), fValue(Value), fDescription(Description), fData(Data)
{
}

void DetectorParameter::Clear(Option_t*)
{;}

void DetectorParameter::Print(Option_t*) const
{
  std::cout << fName.Data() << " = " << fValue.Data() << "\t" << fDescription.Data() << std::endl;
}


