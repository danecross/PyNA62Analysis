// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef ANALYSISINFO_h
#define ANALYSISINFO_h 1

#include "TObject.h"
#include "TString.h"
#include "AnalyzerIdentifier.hh"

class AnalysisInfo : public TObject {

public:

  AnalysisInfo();
  AnalysisInfo(AnalysisInfo &c);
  virtual ~AnalysisInfo() {};
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(AnalysisInfo &s);
  void MergeJobAttributes(AnalysisInfo &s);
  void UpdateAndMergeAttributes(AnalysisInfo &s);
  void AddJobInfo(TString fileName, TString rev);

  std::vector<NA62Analysis::Core::AnalyzerIdentifier> GetAnalyzers() { return fAnalyzerList;             }
  void AddAnalyzer(NA62Analysis::Core::AnalyzerIdentifier anIdent)   { fAnalyzerList.push_back(anIdent); }
  std::vector<TString> GetStreamName()                               { return fStreamList;               }
  void AddStreamName(TString name)                                   { fStreamList.push_back(name);      }

  std::vector<TString>  GetRevisions()  { return fRevisionList;               }
  void AddRevision(TString val)         { fRevisionList.push_back(val);	      }
  std::vector<TString>	GetInputFiles() { return fInputFileList;              }
  void AddInputFile(TString fileName)   { fInputFileList.push_back(fileName); }

private:
  // Unique attributes
  std::vector<NA62Analysis::Core::AnalyzerIdentifier> fAnalyzerList; ///< List of analyzer than ran on these data
  std::vector<TString> fStreamList;                                  ///< List of output stream that created these data

  // Job dependent attributes
  std::vector<TString> fRevisionList;   ///< List of software revision
  std::vector<TString> fInputFileList;	///< List of input files

  ClassDef(AnalysisInfo,2)
};

#endif
