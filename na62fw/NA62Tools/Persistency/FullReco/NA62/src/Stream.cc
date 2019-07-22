// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-02-01
//
// --------------------------------------------------------------

/// \class Stream
/// \Brief
/// Container of MCInfo, RecoInfo and AnalysisInfo data
/// \EndBrief

#include "Stream.hh"

#include <stdlib.h>
#include "Riostream.h"

ClassImp(Stream)

Stream::Stream() {
  Clear();
}

void Stream::Clear(Option_t*) {
  fMCInfo.Clear("");
  fRecoInfo.Clear("");
  fAnalysisInfo.Clear("");
}

void Stream::Print(Option_t*) const {
  fMCInfo.Print("");
  fRecoInfo.Print("");
  fAnalysisInfo.Print("");
}

void Stream::UpdateUniqueAttributes(Stream& s) {
	fMCInfo.UpdateUniqueAttributes(s.fMCInfo);
	fRecoInfo.UpdateUniqueAttributes(s.fRecoInfo);
	fAnalysisInfo.UpdateUniqueAttributes(s.fAnalysisInfo);
}

void Stream::MergeJobAttributes(Stream& s) {
	fMCInfo.MergeJobAttributes(s.fMCInfo);
	fRecoInfo.MergeJobAttributes(s.fRecoInfo);
	fAnalysisInfo.MergeJobAttributes(s.fAnalysisInfo);
}

void Stream::UpdateAndMergeAttributes(Stream& s) {
	fMCInfo.UpdateAndMergeAttributes(s.fMCInfo);
	fRecoInfo.UpdateAndMergeAttributes(s.fRecoInfo);
	fAnalysisInfo.UpdateAndMergeAttributes(s.fAnalysisInfo);
}
