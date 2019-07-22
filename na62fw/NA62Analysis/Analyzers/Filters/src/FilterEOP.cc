// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

/// \class FilterEOP
/// \Brief
/// Flexible event filtering: at least N tracks with E/p>x and Zvertex(track-beam)>y.
/// \EndBrief
/// \Detailed
/// An event is sent to the filter output if at least N spectrometer tracks with energy-to-momentum ratio
/// x1<E/p<x2 are found. The default values are set for the one-electron filter: N=1, x1=0.6, x2=999.
/// A lower cut on the track to nominal beam axis vertex Z coordinate can be set additionally.
/// The filter can be used in the usual way:
/// \code
/// ./MyApp -i <input_file> -o <output_file> --filter
/// \endcode
/// The parameter values can be specified as command line arguments. For example,
/// to keep all events with at least one track potentially originating from kaon decay
/// in the fiducial decay region, one may use
/// \code
/// ./MyApp -i <input_file> -o <output_file> -p "FilterEOP:MinEOP=-999;MinZvertex=110000" --filter
/// \endcode
/// Here is an example of a tight di-electron filter:
/// \code
/// ./MyApp -i <input_file> -o <output_file> -p "FilterEOP:MinNTracks=2;MinEOP=0.8;MaxEOP=1.2" --filter
/// \endcode
/// The class inherits from the FilterEOPBase class.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "FilterEOP.hh"

FilterEOP::FilterEOP(Core::BaseAnalysis *ba) : FilterEOPBase(ba, "FilterEOP") {
  AddParam("MinNTracks", &fMinNTracks,      1);
  AddParam("MinEOP",     &fMinEOP,        0.6);
  AddParam("MaxEOP",     &fMaxEOP,      999.0);
  AddParam("MinZvertex", &fMinZvertex, -999.9); // track-beam axis vertex condition
}

/* To keep the preprocessor happy: */
#pragma dep GetOutput("DownstreamTrackBuilder.hello");
