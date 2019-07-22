// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

/// \class FilterElectron
/// \Brief
/// Event filtering: at least one "electron candidate" track with E/p>0.6
/// \EndBrief
/// \Detailed
/// An event is sent to the filter output if at least one spectrometer track with
/// energy-to-momentum ratio E/p>0.6 (summed over all clusters in the search area,
/// see SpectrometerLKrAssociation) is found. This class inherits from the FilterEOPBase class.
/// Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// The output can be read in the following way:
/// \code
/// Bool_t Selected = *(Bool_t*)GetOutput("FilterElectron.EventSelected");
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "FilterElectron.hh"

FilterElectron::FilterElectron(Core::BaseAnalysis *ba) : FilterEOPBase(ba, "FilterElectron") {
  fMinNTracks =      1;
  fMinEOP     =    0.6;
  fMaxEOP     =  999.9;
  fMinZvertex = -999.9; // track-beam axis vertex condition
}

/* To keep the preprocessor happy: GetOutput("DownstreamTrackBuilder.hello"); */
