// ----------------------------------------------------------------//
// History:
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-20
// Created by Karim Massri (karim.massri@cern.ch)          2014-02-03
//
// ----------------------------------------------------------------//

/// \class MUV3Channel
/// \Brief
/// MUV3 readout channel
/// \EndBrief
/// \Detailed
/// Contains the channel's position and readout IDs, the T0 correction, etc.
/// \EndDetailed

#include "MUV3Channel.hh"

MUV3Channel::MUV3Channel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms) :
  NA62VChannel(GeoChannelID, ROChannelID, FillHistograms, "MUV3"),
  fTileID(GeoChannelID%200)
{

  // High voltage supply group: from 0 to 9. There are up to 32 channels per group.
  if (fTileID<144) fHVGroup = ((fTileID%12)/4) + 3*(fTileID/48);
  else fHVGroup = 9;
}
