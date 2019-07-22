/*
  * IOHandler.hh
 *
 *  Created on: 22 Jan 2014
 *      Author: ncl
 */

#ifndef IOHANDLER_HH_
#define IOHANDLER_HH_

#include <fstream>

#include <TString.h>
#include <TFile.h>
#include "TMutex.h"
#include "MCInfo.hh"
#include "RecoInfo.hh"

#include "TimeCounter.hh"
#include "Verbose.hh"
#include "AnalyzerIdentifier.hh"

class TTree;
class TChain;
class Stream;

namespace NA62Analysis {

class Analyzer;

namespace Core {

/// Type of IOHandler: Not specified yet, histogram IO, TTree IO
enum class IOHandlerType {kNOIO, kHISTO, kTREE};

/// \class InputHandler
/// \Brief
/// Class containing and handling every Input object
/// \EndBrief
///
/// \Detailed
/// Implements the base Input methods\n
/// Manage input event files (loading, specific processing before closing a file or after opening it)\n
/// \EndDetailed
class InputHandler : public Verbose {
public:
	/// \struct keyPair_t
	/// \Brief
	/// Structure defining an input ROOT file key (name, class name)
	/// \EndBrief
	///
	/// \Detailed
	/// Structure defining an input ROOT file key (name, class name)
	/// \EndDetailed
	typedef struct keyPair_t {
		keyPair_t(TString n, TString cn):
			name(n), className(cn){
			/// \MemberDescr
			/// \param name: Name of the object
			/// \param className: ClassName of the object
			/// \return Constructor
			/// \EndMemberDescr
		};
		TString name; ///< Name of the object
		TString className; ///< Class name of the object
	} keyPair; ///< Typedef to struct keyPair_t

	InputHandler();
	explicit InputHandler(const std::string &name);
	InputHandler(const InputHandler& c);
	virtual ~InputHandler();

	//IO Files
	bool AddInputFiles(TString inFileName, int nFiles);
	virtual bool OpenInput(bool specialOnly=false);
	virtual bool CheckNewFileOpened();
	bool CheckDirExists(TString dir) const;
	int GetCurrentFileNumber() const;
	int GetInputFileNumber() const {
		/// \MemberDescr
		/// \return Number of input files
		/// \EndMemberDescr
		return fInputfiles.size();
	};
	virtual bool LoadEvent(Long64_t&) {
		/// \MemberDescr
		/// \param : Event index to load
		/// \return true
		///
		/// Dummy LoadEvent
		/// \EndMemberDescr

		return true;
	};
	virtual bool Rewind() {
		/// \MemberDescr
		/// \return true
		///
		/// Dummy Rewind
		/// \EndMemberDescr

		return true;
	}
	virtual bool ReloadLatest() {
		/// \MemberDescr
		/// \return true
		///
		/// Dummy ReloadLatest
		/// \EndMemberDescr

		return true;
	}
	std::vector<keyPair> GetListOfKeys(TString dir);
	std::vector<TString> GetListOfDirs(TString dir);
	std::vector<TString> GetListOfTH1(TString dir);
	std::vector<TString> GetListOfTH2(TString dir);
	std::vector<TString> GetListOfTGraph(TString dir);
	std::vector<TString> GetListOfTEfficiency(TString dir);
	std::vector<TString> GetListOfHistos(TString dir);
	virtual Long64_t GetNEvents();

	virtual void FileSkipped(int, TString fileName);
	void SkippedAllFiles();
	bool IsLastFileReached() const;
	void Finalise(bool truncate);

	//Printing
	virtual void PrintInitSummary() const;

	IOHandlerType GetIOType() const {
		/// \MemberDescr
		/// \return Type of IO handler
		/// \EndMemberDescr
		return fIOType;
	}

	const TimeCounter_us& GetIoTimeCount() const {
		/// \MemberDescr
		/// \return Reference to the internal IOTimeCount
		/// \EndMemberDescr
		return fIOTimeCount;
	}

	void SetContinuousReading(bool continuousReading) {
		/// \MemberDescr
		/// \param continuousReading: Value to set
		///
		/// Set continuous reading flag
		/// \EndMemberDescr
		fContinuousReading = continuousReading;
	}

	void SignalExit() const {
		/// \MemberDescr
		/// Raise exit signal
		/// \EndMemberDescr

		fSignalExit = true;
	}

	TString ReadCurrentFileRevision();

	void SetMutex(TMutex *m){
		/// \MemberDescr
		/// \param m: Pointer to the mutex to use
		///
		/// Set the mutex
		/// \EndMemberDescr

		fGraphicalMutex = m;
	}

	void SetFastStart(bool fastStart) {
		/// \MemberDescr
		/// \param fastStart : true/false
		///
		/// Enabled/Disable the fastStart option
		/// \EndMemberDescr
		fFastStart = fastStart;
	}

	bool IsFastStart() const {
		/// \MemberDescr
		/// \return True if fastStart is enabled
		/// \EndMemberDescr
		return fFastStart;
	}

	void SetSkipFatal(bool fatal) {
		/// \MemberDescr
		/// \param fatal : true/false
		///
		/// Enabled/Disable the skip is fatal option (abort processing if unable to read input file)
		/// \EndMemberDescr
		fSkipIsFatal = fatal;
	}

	bool IsSkipFatal() const {
		/// \MemberDescr
		/// \return True if skip is fatal is enabled
		/// \EndMemberDescr
		return fSkipIsFatal;
	}

	std::vector<AnalyzerIdentifier> GetAnalyzerList() const;
	AnalyzerIdentifier GetAnalyzerFromBit(unsigned int bitNumber) const;
	int GetBitFromAnalyzer(TString analyzerID) const;

	void BranchStream();
	Stream* GetStreamInfo() {return fStreamEvent;};
	void LoadMCStream(Long64_t fileIndex);

	TFile * GetCurrentFile() {
		/// \MemberDescr
		/// \return Pointer to the currently opened TFile
		/// \EndMemberDescr
		return fCurrentFile;
	}

	void SetSkipBadBurst(bool skipBadBurst) {
		/// \MemberDescr
		/// \param skipBadBurst : Enable/Disable the skip bad burst feature
		/// \EndMemberDescr
		fSkipBadBurst = skipBadBurst;
	}

	virtual bool GetWithMC() const = 0;
	virtual int GetRunID();
	virtual int GetBurstID();
	virtual int GetBurstTime();
	virtual TString GetRevision();
	virtual TString GetMCRevision();
	virtual TString GetRecoRevision();
	virtual MCInfo* GetMCInfo();
	virtual RecoInfo* GetRecoInfo();
protected:
	void NewFileOpened(int index, TFile* currFile);
	TString CheckProtocols(TString OldStr);

	bool fContinuousReading; ///< Continuous reading enabled?
	bool fFastStart; ///< Fast start flag enabled? (Start processing directly without checking files)
	bool fSkipBadBurst; ///< Controls the activation of the skip bad burst feature
	bool fSkipIsFatal; ///< Flag to indicate whether a skipped file is fatal and should interrupt the processing
	int  fHasMCStream; ///< Indicates the presence of a valid MC Stream (meaning the file is MC). -1 is not initalized, 1 means it is MC.
	mutable bool fSignalExit; ///< Signal from main thread to exit
	IOHandlerType fIOType; ///< Type of IO handler

	int fCurrentFileNumber; ///< Index of the current opened file in the TChain
	Long64_t fCurrentEvent; ///< Index of the current "event" loaded. It is guaranteed to be either -1, or a good event
	Long64_t fPreviousEvent; ///< Index of the previous "event" loaded. It is guaranteed to be either -1, or a good event

	TFile *fCurrentFile; ///< Pointer to the currently opened file in the TChain
	TChain *fStreamTree;
	Stream *fStreamEvent;

	std::vector<TString> fInputfiles; ///< Vector of input file path

	std::ofstream fSkippedFD; ///< Skipped files output stream

	mutable TimeCounter_us fIOTimeCount; ///< Counter for the time spent in IO
	TMutex *fGraphicalMutex; ///< Mutex to prevent graphical objects to change while processing GUI events
};

bool TestIsTextFile(TString fileName);
bool TestASCIIChar(unsigned char c);
bool TestMultiByteChar(unsigned char c);
TString Canonicalize(TString path);
std::vector<TString> GetCWD();

} /* namespace Core */
} /* namespace NA62Analysis */
#endif /* IOHANDLER_HH_ */
