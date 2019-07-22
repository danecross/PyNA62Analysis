// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "TVChannelID.hh"

#include "Riostream.h"


ClassImp(TVChannelID)

void TVChannelID::Print(Option_t *) const {
  std::cout << "ChannelID = " << fChannelID << std::endl;
}
void TVChannelID::Clear(Option_t* /*option*/) {
  fChannelID=-1;
}
