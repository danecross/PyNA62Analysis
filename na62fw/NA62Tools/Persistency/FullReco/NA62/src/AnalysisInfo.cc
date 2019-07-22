// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// ---------------------------------------------------------------

/// \class AnalysisInfo
/// \Brief
/// Container of settings specified in NA62MC macro file
/// \EndBrief

#include "AnalysisInfo.hh"

#include <stdlib.h>
#include "Riostream.h"
#include "TemplateFunctions.hh"

ClassImp(AnalysisInfo)

AnalysisInfo::AnalysisInfo() {
  Clear();
}

AnalysisInfo::AnalysisInfo(AnalysisInfo &c) :
		TObject(c),
		fInputFileList(c.fInputFileList)
{
  Clear();
}

void AnalysisInfo::Clear(Option_t*) {
	fAnalyzerList.clear();
	fInputFileList.clear();
	fRevisionList.clear();
}

void AnalysisInfo::Print(Option_t*) const {
	std::cout << "AnalysisInfo" << std::endl;
	std::cout << "Analyzer history:" << std::endl;
	for(auto identifier : fAnalyzerList) std::cout << " -> " << identifier.GetAnalyzerName().GetString();
	std::cout << std::endl;
	std::cout << "Streams history:" << std::endl;
	for(auto stream : fStreamList) std::cout << " -> " << stream;
	std::cout << std::endl;
	std::cout << "Input files:" << std::endl;
	for(unsigned int iFile=0; iFile<fInputFileList.size(); ++iFile) {
		std::cout << "  - " 						<< fInputFileList[iFile] << std::endl;
		std::cout << std::setw(10) << "Rev.: "    	<< fRevisionList[iFile] << std::endl;
	}
}

void AnalysisInfo::UpdateAndMergeAttributes(AnalysisInfo &s) {
	UpdateUniqueAttributes(s);
	MergeJobAttributes(s);
}

void AnalysisInfo::UpdateUniqueAttributes(AnalysisInfo &s) {
	fAnalyzerList.clear();
	fAnalyzerList.insert(fAnalyzerList.begin(), s.fAnalyzerList.begin(), s.fAnalyzerList.end());

	fStreamList.clear();
	fStreamList.insert(fStreamList.begin(), s.fStreamList.begin(), s.fStreamList.end());
}

void AnalysisInfo::MergeJobAttributes(AnalysisInfo& c) {
	if(!all_equal(c.fInputFileList.size(), c.fRevisionList.size())){
		std::cout << "Warning: merging AnalysisInfo job attributes with inconsistent number of entries." << std::endl;
	}

	fInputFileList.insert(fInputFileList.end(), c.fInputFileList.begin(), c.fInputFileList.end());
	fRevisionList.insert(fRevisionList.end(), c.fRevisionList.begin(), c.fRevisionList.end());
}

void AnalysisInfo::AddJobInfo(TString fileName, TString rev) {
	AddInputFile(fileName);
	AddRevision(rev);
}
