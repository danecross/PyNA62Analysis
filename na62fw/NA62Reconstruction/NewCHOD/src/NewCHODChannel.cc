// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

/// \class NewCHODChannel
/// \Brief
/// NewCHOD readout channel
/// \EndBrief
/// \Detailed
/// Contains the channel's position and readout IDs, the T0 correction, monitoring histograms, etc.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "NewCHODChannel.hh"

NewCHODChannel::NewCHODChannel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms) :
  NA62VChannel(GeoChannelID, ROChannelID, FillHistograms, "NewCHOD"),
  fIsHigh(GeoChannelID%100 >= 50)
{
}
