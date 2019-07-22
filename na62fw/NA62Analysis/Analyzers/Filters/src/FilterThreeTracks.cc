// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

/// \class FilterThreeTracks
/// \Brief
/// Event filtering: at least 3 tracks
/// \EndBrief
/// \Detailed
/// An event is sent to the filter output if at least 3 spectrometer tracks are found.
/// The class inherits from the FilterEOPBase class.
/// Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// The output can be read in the following way:
/// \code
/// Bool_t Selected = *(Bool_t*)GetOutput("FilterThreeTracks.EventSelected");
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "FilterThreeTracks.hh"

FilterThreeTracks::FilterThreeTracks(Core::BaseAnalysis *ba) : FilterEOPBase(ba, "FilterThreeTracks") {
  fMinNTracks =      3;
  fMinEOP     = -999.9;
  fMaxEOP     =  999.9;
  fMinZvertex = -999.9; // track-beam axis vertex condition
}

/* To keep the preprocessor happy: GetOutput("DownstreamTrackBuilder.hello"); */
