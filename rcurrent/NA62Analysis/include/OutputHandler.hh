/*
 * OutputHandler.hh
 *
 *  Created on: Jul 15, 2016
 *      Author: nlurkin
 */

#ifndef INCLUDE_OUTPUTHANDLER_HH_
#define INCLUDE_OUTPUTHANDLER_HH_

#include <TFile.h>
#include <map>

#include "TimeCounter.hh"
#include "Verbose.hh"
#include "containers.hh"

class TTree;
class TChain;
class Stream;
class TSlimRecoVEvent;
class TRecoVEvent;

namespace NA62Analysis {

class Analyzer;

namespace Core {

/// \class OutputHandler
/// \Brief
/// Class containing and handling every Output object
/// \EndBrief
///
/// \Detailed
/// Implements the Output methods\n
/// Manage output files (creating, writing tree during filtering)\n
/// \EndDetailed
class OutputHandler : public Verbose{
public:
	OutputHandler();
	explicit OutputHandler(const std::string &name);
	OutputHandler(const OutputHandler& c);

	virtual ~OutputHandler();

	virtual bool OpenOutput(TString outFileName);
	TString GetOutputFileName() const;
	//Writing
	void MkOutputDir(TString name) const;
	void PrepareOutputTreesReco(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree);
	void PrepareOutputTreesSlim(Long64_t *filterWord, TChain* treeList, TChain *referenceTree);
	void PrepareUserTrees(std::map<TString, TTree*> treeList, bool firstCopy);
	void WriteEvent(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree);
	void WriteSpecialEvent(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree);
	void WriteUserEvent(std::map<TString, TTree*> treeList, bool firstCopy, TString name="");
	void WriteTree() const;
	void Finalise();

	const TimeCounter_us& GetIoTimeCount() const {
		/// \MemberDescr
		/// \return Reference to the internal IOTimeCount
		/// \EndMemberDescr
		return fIOTimeCount;
	}

	void SetOutputFileAsCurrent(TString dirName="") const {
		/// \MemberDescr
		/// \param dirName: optional directory name which should be made as current directory in the output file
		///
		/// Set outputFile as current file (gFile)
		/// \EndMemberDescr
		if(fOutFile){
			gFile = fOutFile;
			if(dirName.Length()>0)
				fOutFile->cd(dirName);
			else if(gDirectory != fOutFile)
				fOutFile->cd();
		}
	}

	void WriteAnalyzerList(std::vector<Analyzer*> anList);

	void AddFileName(std::string name);
	void AddStreamEvent(Stream* streamEvent);
	void SetDisabledTrees(const std::vector<TString> &disabled) { fDisabledTrees = disabled; };

  TFile * GetOutputFile() {
    /// \MemberDescr ///
    /// \return Returns a pointer to the TFile instance corresponding to the output ROOT file
    /// \EndMemberDescr
      return fOutFile;
  }

private:
	void AddFilterBranch(TTree* exportTree, Long64_t *filterWord);

	char *fCurrentDir; ///< Current work directory

	TFile *fOutFile; ///< Output file
	TString fOutFileName; ///< Output fileName
	TTree *fStreamTree;
	Stream *fStreamEvent;

	NA62Analysis::NA62Map<TString, TTree*>::type fExportTrees; ///< Container for TTrees for exporting
	NA62Analysis::NA62Map<TString, TTree*>::type fUserTrees; ///< Container for export user  TTrees

	mutable TimeCounter_us fIOTimeCount; ///< Counter for the time spent in IO
	std::vector<TString> fDisabledTrees;
	std::vector<std::pair<TSlimRecoVEvent*, TRecoVEvent*>> fSlimObjects;
};

} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* INCLUDE_OUTPUTHANDLER_HH_ */
