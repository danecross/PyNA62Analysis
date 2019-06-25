/*
 * IOHandler.cc
 *
 *  Created on: 22 Jan 2014
 *      Author: ncl
 */

#include "InputHandler.hh"

#include <iostream>
#include <signal.h>
#include <sys/stat.h>
#include <numeric>

#include <TFile.h>
#include <TObjString.h>
#include <TKey.h>
#include <TSystem.h>
#include <TThread.h>
#include <TRegexp.h>
#include <TTree.h>

#include "GitRevision.hh"
#include "ConfigSettings.hh"
#include "TermManip.hh"
#include "NA62Exceptions.hh"
#include "Analyzer.hh"
#include "Indexer.hh"
#include "Stream.hh"

namespace NA62Analysis {
namespace Core {

InputHandler::InputHandler() :
		fContinuousReading(false), fFastStart(false), fSkipBadBurst(true), fSkipIsFatal(false), fHasMCStream(-1), fSignalExit(false),
		fIOType(IOHandlerType::kNOIO), fCurrentFileNumber(-1), fCurrentEvent(-1), fPreviousEvent(-1), fCurrentFile(NULL),
		fStreamTree(new TChain("Streams")), fStreamEvent(new Stream), fGraphicalMutex(NULL) {
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr
}

InputHandler::InputHandler(const std::string &name) :
		Verbose(name), fContinuousReading(false), fFastStart(false),
		fSkipBadBurst(true), fSkipIsFatal(false), fHasMCStream(-1), fSignalExit(false), fIOType(IOHandlerType::kNOIO),
		fCurrentFileNumber(-1), fCurrentEvent(-1), fPreviousEvent(-1), fCurrentFile(NULL), fStreamTree(new TChain("Streams")),
		fStreamEvent(new Stream), fGraphicalMutex(NULL) {
	/// \MemberDescr
	/// \param name : Display name
	///
	/// Constructor with name
	/// \EndMemberDescr
}

InputHandler::InputHandler(const InputHandler& c) :
		Verbose(c), fContinuousReading(false), fFastStart(c.fFastStart),
		fSkipBadBurst(c.fSkipBadBurst), fSkipIsFatal(c.fSkipIsFatal), fHasMCStream(c.fHasMCStream), fSignalExit(false), fIOType(c.GetIOType()),
		fCurrentFileNumber(c.fCurrentFileNumber), fCurrentEvent(c.fCurrentEvent), fPreviousEvent(c.fPreviousEvent),
		fCurrentFile(c.fCurrentFile), fStreamTree(c.fStreamTree), fStreamEvent(c.fStreamEvent), fGraphicalMutex(c.fGraphicalMutex) {
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	///
	/// Copy constructor
	/// \EndMemberDescr
}

InputHandler::~InputHandler() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr

	if (fSkippedFD.is_open()) {
		fSkippedFD.close();
	}
}

bool InputHandler::CheckDirExists(TString dir) const {
	/// \MemberDescr
	/// \param dir : Directory to check
	/// \return True if the directory exists in the input file (and the input file is open)
	///
	/// Check if a directory exists in the input ROOT file.
	/// \EndMemberDescr
	if (!fCurrentFile)
		return false;

	fIOTimeCount.Start();
	bool retVal = fCurrentFile->GetDirectory(dir);
	fIOTimeCount.Stop();
	return retVal != 0;
}

bool InputHandler::CheckNewFileOpened() {
	/// \MemberDescr
	/// \return False
	/// \EndMemberDescr

	return false;
}

int InputHandler::GetCurrentFileNumber() const {
	/// \MemberDescr
	/// \return Index of the currently opened file
	/// \EndMemberDescr

	return fCurrentFileNumber;
}

std::vector<TString> InputHandler::GetListOfDirs(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of directories in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> dirs;

	for (auto k : keys) {
		if (k.className.CompareTo("TDirectoryFile") >= 0)
			dirs.push_back(k.name);
	}
	return dirs;
}

std::vector<TString> InputHandler::GetListOfHistos(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of histograms in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> histo;

	for (auto k : keys) {
		if (k.className.CompareTo("TH1") >= 0)
			histo.push_back(k.name);
		else if (k.className.CompareTo("TH2") >= 0)
			histo.push_back(k.name);
		else if (k.className.CompareTo("TGraph") >= 0)
			histo.push_back(k.name);
		else if (k.className.CompareTo("TEfficiency") >= 0)
			histo.push_back(k.name);
	}
	return histo;
}

std::vector<InputHandler::keyPair> InputHandler::GetListOfKeys(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of keys in the searched directory in the input ROOT file. The key contains
	/// the name of the object (key.name) and the className of the object (key.className)
	/// \EndMemberDescr

	std::vector<InputHandler::keyPair> keys;

	if (!CheckDirExists(dir)) {
		std::cout << normal() << "The requested directory " << dir
				<< " does not exist in input file" << std::endl;
		return keys;
	}
	fIOTimeCount.Start();
	TList *kList = fCurrentFile->GetDirectory(dir)->GetListOfKeys();
	fIOTimeCount.Stop();

	for (int i = 0; i < kList->GetEntries(); i++) {
		TKey *k = (TKey*) kList->At(i);
		keys.push_back(keyPair_t(k->GetName(), k->GetClassName()));
	}

	return keys;
}

std::vector<TString> InputHandler::GetListOfTGraph(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of TGraph in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> histo;

	for (auto k : keys) {
		if (k.className.CompareTo("TGraph") >= 0)
			histo.push_back(k.name);
	}
	return histo;
}

std::vector<TString> InputHandler::GetListOfTEfficiency(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of TGraph in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> histo;

	for (auto k : keys) {
		if (k.className.CompareTo("TEfficiency") >= 0)
			histo.push_back(k.name);
	}
	return histo;
}

std::vector<TString> InputHandler::GetListOfTH1(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of TH1 in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> histo;

	for (auto k : keys) {
		if (k.className.CompareTo("TH1") >= 0)
			histo.push_back(k.name);
	}

	return histo;
}

std::vector<TString> InputHandler::GetListOfTH2(TString dir) {
	/// \MemberDescr
	/// \param dir : Directory to search
	/// \return A list of TH2 in the searched directory in the input ROOT file.
	/// \EndMemberDescr

	fIOTimeCount.Start();
	std::vector<InputHandler::keyPair> keys = GetListOfKeys(dir);
	fIOTimeCount.Stop();
	std::vector<TString> histo;

	for (auto k : keys) {
		if (k.className.CompareTo("TH2") >= 0)
			histo.push_back(k.name);
	}
	return histo;
}

void InputHandler::NewFileOpened(int index, TFile* currFile) {
	/// \MemberDescr
	/// \param index : Index of the new file
	/// \param currFile : Pointer to the new file
	///
	/// Method called by TChain when opening a new file.\n
	/// It will signal a new burst to the analyzers
	/// \EndMemberDescr

	if (index != (fCurrentFileNumber + 1)) {
		for (int i = fCurrentFileNumber + 1; i < index; i++) {
			std::cout << normal() << "File " << i << ": " << fInputfiles[i]
					<< " has been skipped" << std::endl;
			FileSkipped(i, fInputfiles[i]);
		}
	}
	std::cout << normal() << "Opening file " << index << ": "
			<< currFile->GetName() << std::endl;
	fCurrentFileNumber = index;
	fCurrentFile = currFile;

	LoadMCStream(index);

	TString fileRevision = ReadCurrentFileRevision(); //Read revision from file
	TString persistencyRevisionString = GetCurrentGitRevision(); //Read revision from persistency
	if (persistencyRevisionString.Length() > 0
			&& fileRevision.CompareTo("") != 0) {
		//Revision could be found in both the file and the persistency.
		//Can compare!
		if (fileRevision.CompareTo(persistencyRevisionString) != 0) {
			std::cout << manip::brown << manip::bold << normal()
					<< "WARNING: File revision (" << fileRevision
					<< ") and Persistency revision ("
					<< persistencyRevisionString << ") are different."
					<< std::endl;
			std::cout << normal() << "This might lead to inconsistencies."
					<< std::endl;
			std::cout << manip::reset; // #NOQA_VERBOSE
		}
	}
}

void InputHandler::Finalise(bool truncate) {
	/// \MemberDescr
	/// \param truncate: Flag to indicate if we are truncating the number of events to read
	///
	/// Finalise the IO: check that the last file has been read.
	/// \EndMemberDescr

	if (!IsLastFileReached() && !truncate) {
		for (unsigned int i = fCurrentFileNumber + 1; i < fInputfiles.size();
				i++) {
			std::cout << normal() << "File " << i << ":" << fInputfiles[i]
					<< " has been skipped" << std::endl;
			FileSkipped(i, fInputfiles[i]);
		}
	}
}

bool InputHandler::AddInputFiles(TString inFileName, int nFiles) {
	/// \MemberDescr
	/// \param inFileName : Path to the input file
	/// \param nFiles : Number of files to open
	/// \return true if success, else false
	///
	/// Register the input files.
	/// \EndMemberDescr

	bool fileError;

	if (inFileName.Length() == 0) {
		std::cout << always() << "No input file specified" << std::endl;
		return false;
	}
	if (nFiles == 0) {
		if (fContinuousReading) {
			// Continuous reading needs a list of files, not a single file
			std::cout << standard()
					<< "[ERROR] Continuous reading enabled but no list file provided..."
					<< std::endl;
			throw LogicException();
		}

		inFileName = Canonicalize(inFileName);
		// Use new address format for castor and eos
		fileError = false;
		if (inFileName.Contains("/castor/") || inFileName.Contains("/eos/")) {
			//Does file exists
			FileStat_t bufStat;
			if (gSystem->GetPathInfo(CheckProtocols(inFileName), bufStat) != 0)
				fileError = true;
		} else {
			//Does file exists
			struct stat bufStat;
			if(stat(inFileName.Data(), &bufStat)<0)
				fileError = true;
		}

		if(fileError){
			std::cout << always() << "File " << CheckProtocols(inFileName) << " is not found" << std::endl;
			return false;
		}
		else{
			std::cout << extended() << "Adding file " << CheckProtocols(inFileName) << std::endl;
			fInputfiles.push_back(CheckProtocols(inFileName));
		}
	} else {
		//Reading a list of files
		TString inputFileName;
		fIOTimeCount.Start();
		//Check it is indeed a text file and not a binary file
		if (!TestIsTextFile(inFileName)) {
			std::cout << always() << "Input list file " << inFileName
					<< " cannot be read as a text file." << std::endl;
			fIOTimeCount.Stop();
			return false;
		}
		std::ifstream inputList;
		int counter = 0;
		char roll[4] = { '|', '/', '-', '\\' };
		int inputFileNumber = 0;
		do {
			//If we already read an input list, close it first and retry (has already been processed)
			if (inputList.is_open())
				inputList.close();
			TThread::CancelPoint();
			inputList.open(inFileName.Data());

			if (fContinuousReading) { // Display waiting wheel
				std::cout << standard()
						<< "Waiting for a valid List File to be ready "
						<< roll[counter % 4] << "\r" << std::flush;
				counter++;
				gSystem->Sleep(500);
				if(fSignalExit){
					fIOTimeCount.Stop();
					return false;
				}
			}
			//Try to read the file as long as we can read more in the list and that we didn't reach the limit
			while (inputFileName.ReadLine(inputList)
					&& (nFiles < 0 || inputFileNumber < nFiles)) {
				fileError = false;
				inputFileName = inputFileName.Strip();
				//Skip commented out files
				if (inputFileName.BeginsWith("#")){
					fIOTimeCount.Stop();
					continue;
				}

				inputFileName = Canonicalize(inputFileName);
				// Use new address format for castor and eos
				if (inputFileName.Contains("/castor/") || inputFileName.Contains("/eos/")){
					//Does file exists
					FileStat_t bufStat;
					if(gSystem->GetPathInfo(CheckProtocols(inputFileName), bufStat)!=0)
						fileError = true;
				}
				else{
					//Does file exists
					struct stat bufStat;
					if(stat(inputFileName.Data(), &bufStat)<0)
						fileError = true;
				}
				if(fileError)
					std::cout << always() << "File " << CheckProtocols(inputFileName) <<
							" is not found" << std::endl;
				else{
					std::cout << extended() << "Adding file " << CheckProtocols(inputFileName) << std::endl;
					fInputfiles.push_back(CheckProtocols(inputFileName));
					++inputFileNumber;
				}
			}

			//If list file is empty or we did not manage to read at least 1 file, abort.
			//Unless continuous reading, in such case, we just retry until it works
			if (!fContinuousReading && inputFileNumber == 0) {
				std::cout << always() << "No valid input file in the list "
						<< inFileName << std::endl;
				fIOTimeCount.Stop();
				return false;
			}
			// If continuous reading, loop until we read at least 1 file
		} while (!fSignalExit && fContinuousReading
				&& (inputFileNumber == 0 || !inputList.is_open()));

		//Close and eventually delete input list
		inputList.close();
		if (fContinuousReading)
			unlink(inFileName.Data());
	}
	fIOTimeCount.Stop();
	return true;
}

void InputHandler::PrintInitSummary() const {
	/// \MemberDescr
	///
	/// Print the summary after initialization
	/// \EndMemberDescr
}

void InputHandler::FileSkipped(int, TString fileName) {
	/// \MemberDescr
	/// \param fileName: Name of the skipped file
	///
	/// File has been skipped for whatever reason. Notify it in the .skipped file
	/// \EndMemberDescr

	//Strip fileName from the castor/eos pre- and post-fix
	bool fileError = false;
	if (fileName.Contains("/castor/") || fileName.Contains("/eos/")) {
		//Does file exists
		FileStat_t bufStat;
		if (gSystem->GetPathInfo(CheckProtocols(fileName), bufStat) != 0)
			fileError = true;
	} else {
		//Does file exists
		struct stat bufStat;
		if(stat(fileName.Data(), &bufStat)<0)
			fileError = true;
	}

	TFile *fd = TFile::Open(CheckProtocols(fileName), "READ");
	if(!fileError && fd!=nullptr){ // File exists and file is readable
		fd->Close();
		return;
	}

	if(fSkipIsFatal){
		std::cout << always() << "Fatal error: Unable to read file " << fileName << std::endl;
		throw ReadException();
	}
	int svc_sign = fileName.First("?");
	if(svc_sign>0)
		fileName = fileName.Remove(svc_sign);
	if(fileName.BeginsWith("root://castorpublic"))
		fileName = fileName.Remove(0, TString("root://castorpublic.cern.ch/").Length());
	if(fileName.BeginsWith("root://eosna62"))
		fileName = fileName.Remove(0, TString("root://eosna62.cern.ch/").Length());
        if(fileName.BeginsWith("root://eosuser"))
		fileName = fileName.Remove(0, TString("root://eosuser.cern.ch/").Length());

	fIOTimeCount.Start();
	if (!fSkippedFD.is_open())
		fSkippedFD.open(
				(Configuration::ConfigSettings::global::fSkippedName
						+ ".skipped").data(), std::ios::out);
	if (!fSkippedFD.is_open())
		std::cout << normal() << "Unable to open skipped file "
				<< Configuration::ConfigSettings::global::fSkippedName
				<< ".skipped" << std::endl;
	else
		fSkippedFD << fileName << std::endl;
	fIOTimeCount.Stop();
}

TString InputHandler::ReadCurrentFileRevision() {
	/// \MemberDescr
	/// \return Revision number embedded in the input ROOT file or -1 if not found
	/// \EndMemberDescr

	fIOTimeCount.Start();
	TList *keys = fCurrentFile->GetListOfKeys();
	fIOTimeCount.Stop();
	TString revValue = "";
	for (int kIndex = 0; kIndex < keys->GetEntries(); kIndex++) {
		TKey *k = static_cast<TKey*>(keys->At(kIndex));
		if (TString(k->GetName()).BeginsWith("Revision:")) {
			// Old svn style
			revValue = TString(k->GetName())(TRegexp("[0-9]+"));
		}
		if (TString(k->GetName()).BeginsWith("r")) {
			// New git develop style
			revValue = TString(k->GetName())(TRegexp("[0-9]+"));
		}
		if (TString(k->GetName()).BeginsWith("v")) {
			// New git master style
			revValue = TString(k->GetName())(TRegexp("[0-9]+.[0-9]+.[0-9]+"));
		}
	}
	return revValue;
}

bool TestIsTextFile(TString fileName) {
	/// \MemberDescr
	/// \param fileName : Path to the file to test
	/// \return True is the file has been found to be a text file, else false
	///
	/// Test all the characters in the first 1KB chunk of the file. If all of them
	/// are found to be valid text characters (ASCII or 8-bit variable length encoding),
	/// the file is considered as being a valid text file.
	/// \EndMemberDescr
	unsigned char buffer[1000];
	std::ifstream fd(fileName.Data(), std::ifstream::binary);

	fd.read((char*) buffer, 1000);
	int nread = fd.gcount();
	fd.close();
	for (int i = 0; i < 1000 && i < nread; i++) {
		//Is it pure ASCII
		if (!TestASCIIChar(buffer[i])) {
			//No, test 8-bit or variable length encoding
			if (!TestMultiByteChar(buffer[i]))
				return false; //Still some other special text encoding possible but we don't care at this point (UTF-16, UTF-32, ...)
		}
	}

	// Every single byte in the first 1K chunk of the file is compatible with either pure ascii or
	// 8-bit or variable lenght encoding. It is therefore considered as a text file.
	return true;
}

bool TestASCIIChar(unsigned char c) {
	/// \MemberDescr
	/// \param c : char to test
	/// \return True if the given char is ASCII
	/// \EndMemberDescr
	if ((c > 9 && c < 13) || (c > 32 && c < 126))
		return true;
	return false;
}

bool TestMultiByteChar(unsigned char c) {
	/// \MemberDescr
	/// \param c : char to test
	/// \return True if the given char is a multibyte char
	/// \EndMemberDescr

	if (c > 128 && c < 255)
		return true;
	return false;
}

bool InputHandler::IsLastFileReached() const {
	/// \MemberDescr
	/// \return True if the current file is the last file of the list
	/// \EndMemberDescr
	if (fCurrentFileNumber == (int) fInputfiles.size() - 1)
		return true;
	return false;
}

Long64_t InputHandler::GetNEvents() {
	/// \MemberDescr
	/// \return Total number of input files.
	/// Overloaded in IOTree.
	/// \EndMemberDescr
	return GetInputFileNumber();
}

std::vector<AnalyzerIdentifier> InputHandler::GetAnalyzerList() const {
		/// \MemberDescr
		/// \return vector containing sorted history of analyzers from input file
		/// \EndMemberDescr
		return fStreamEvent->GetAnalysisInfo().GetAnalyzers();
}

AnalyzerIdentifier InputHandler::GetAnalyzerFromBit(unsigned int bitNumber) const {
	/// \MemberDescr
	/// \param bitNumber : Bit position (start at 0)
	/// \return Identifier of the analyzer corresponding to the given bit number
	/// \EndMemberDescr

	std::vector<AnalyzerIdentifier> anList = GetAnalyzerList();
	if (bitNumber < anList.size())
		return anList[bitNumber];
	else
		return ANIDNone;
}

int InputHandler::GetBitFromAnalyzer(TString analyzerID) const {
	/// \MemberDescr
	/// \param analyzerID : identification string of the analyzer (analyzer name)
	/// \return Bit position corresponding the the given analyzer
	/// \EndMemberDescr

	std::vector<AnalyzerIdentifier> anList = GetAnalyzerList();
	for (unsigned int i = 0; i < anList.size(); ++i) {
		if (anList[i] == analyzerID)
			return i;
	}
	return -1;
}

void InputHandler::SkippedAllFiles(){
	for(auto file : indexer(fInputfiles))
		FileSkipped(file.first, file.second);
}

void InputHandler::BranchStream() {
	fStreamTree->SetBranchAddress("Stream", &fStreamEvent);
}

void InputHandler::LoadMCStream(Long64_t fileIndex) {
	fIOTimeCount.Start();
	fStreamTree->GetEntry(fileIndex);
	if(GetMCInfo()){
		if(fHasMCStream==0 && GetMCInfo()->GetRevision().CompareTo("")!=0){
			std::cout << always() << "Detected mixed Data/MC file list ... Aborting" << std::endl;
			throw LogicException();
		}
		else if(fHasMCStream==-1)
			fHasMCStream = GetMCInfo()->GetRevision().CompareTo("")==0 ? 0 : 1;
	}
	fIOTimeCount.Stop();
}

bool InputHandler::OpenInput(bool /*specialOnly*/) {
	int success = 0;
	int offset = fCurrentFileNumber>=0 ? fCurrentFileNumber+1 : 0;
	for (auto fileName : skip<decltype(fInputfiles)>(fInputfiles, offset)) {
		fIOTimeCount.Start();
		success += fStreamTree->AddFile(fileName);
		fIOTimeCount.Stop();
	}
	std::cout << extended() << "Streams tree request... " <<
			success << "/" << fInputfiles.size() << " " <<
			((uint)success==fInputfiles.size() ? "success" : "failed") <<std::endl;
	return (uint)success==fInputfiles.size();
}

int InputHandler::GetRunID() {
	/// \MemberDescr
	/// \return RunID from the Stream tree if found (in RecoInfo first, then MCInfo), else -1.
	/// \EndMemberDescr
	if(fStreamEvent){
		std::vector<UInt_t> rID = fStreamEvent->GetRecoInfo().GetRunID();
		if(rID.size()>0)
			return rID[0];
		std::vector<Int_t> mcID = fStreamEvent->GetMCInfo().GetRunNumber();
		if(mcID.size()>0)
			return mcID[0];
	}
	return -1;
}

int InputHandler::GetBurstID() {
	/// \MemberDescr
	/// \return BurstID from the Stream tree if found (in RecoInfo), else -1.
	/// \EndMemberDescr
	if(fStreamEvent){
		std::vector<UInt_t> bID = fStreamEvent->GetRecoInfo().GetBurstID();
		if(bID.size()>0)
			return bID[0];
	}
	return -1;
}

int InputHandler::GetBurstTime() {
	/// \MemberDescr
	/// \return Burst timestamp from the Stream tree if found (in RecoInfo), else -1.
	/// \EndMemberDescr
	if(fStreamEvent){
		std::vector<UInt_t> bID = fStreamEvent->GetRecoInfo().GetBurstTime();
		if(bID.size()>0)
			return bID[0];
	}
	return -1;
}

TString InputHandler::GetRevision() {
	/// \MemberDescr
	/// \return Revision from the Stream tree if found (in RecoInfo first, then MCInfo), else "".
	/// \EndMemberDescr
	if(fStreamEvent){
		TString rID = GetRecoRevision();
		if(rID!="") return rID;
		TString mcID = GetMCRevision();
		if(mcID!="") return mcID;
	}
	return "";
}

TString InputHandler::GetMCRevision() {
	/// \MemberDescr
	/// \return Revision from the Stream MC tree if found, else "".
	/// \EndMemberDescr
	if(fStreamEvent){
		return fStreamEvent->GetMCInfo().GetRevision();
	}
	return "";
}

TString InputHandler::GetRecoRevision() {
	/// \MemberDescr
	/// \return Revision from the Stream Reco tree if found, else "".
	/// \EndMemberDescr
	if(fStreamEvent)
		return fStreamEvent->GetRecoInfo().GetRevision();
	return "";
}

MCInfo* InputHandler::GetMCInfo() {
	/// \MemberDescr
	/// \return MCInfo* from the Stream tree if found, else nullptr.
	/// \EndMemberDescr
	if(fStreamEvent) return &(fStreamEvent->GetMCInfo());
	return nullptr;
}

RecoInfo* InputHandler::GetRecoInfo() {
	/// \MemberDescr
	/// \return RecoInfo* from the Stream tree if found, else nullptr.
	/// \EndMemberDescr
	if(fStreamEvent) return &(fStreamEvent->GetRecoInfo());
	return nullptr;
}

TString InputHandler::CheckProtocols(TString OldStr){
  TString NewStr=OldStr;
  if(NewStr.EndsWith("\r")) NewStr.Remove(NewStr.Last('\r')); // Remove any residual EOL special character (^M) [for Windows-DOS compatibility]
  if(NewStr.BeginsWith("/eos/") && !NewStr.Contains("root://")){
    if(NewStr.Contains("/eos/experiment")) NewStr = "root://eosna62.cern.ch/"+NewStr;
    else if(NewStr.Contains("/eos/user"))  NewStr = "root://eosuser.cern.ch/"+NewStr;
  }
  else if(NewStr.BeginsWith("/castor/")){
    if(!NewStr.Contains("root://")){
      NewStr = "root://castorpublic.cern.ch/"+NewStr;
    }
    if(!NewStr.Contains("svcClass")){
      NewStr = NewStr+"?svcClass="+Configuration::ConfigSettings::global::fSvcClass;
    }
  }
  return NewStr;
}

TString Canonicalize(TString path){
	if(!path.BeginsWith("/") && !path.BeginsWith(".")) path = "./" + path;
	TObjArray *arr = path.Tokenize("/");

	std::vector<TString> final;
	for(int iToken=0; iToken<arr->GetEntries(); ++iToken){
		TString val = static_cast<TObjString*>(arr->At(iToken))->GetString();
		if(val.CompareTo(".")==0){
			if(final.size()==0){
				std::vector<TString> cwd = GetCWD();
				final.insert(final.end(), cwd.begin(), cwd.end());
			}
			else
				continue;
		}
		else if(val.CompareTo("..")==0){
			if(final.size()==0){
				std::vector<TString> cwd = GetCWD();
				final.insert(final.end(), cwd.begin(), cwd.end());
				final.pop_back();
			}
			else
				final.pop_back();
		}
		else
			final.push_back(val);
	}

	delete arr;

	TString ret;
	if(final.size()==1)
		ret = final[0];
	else{
		ret = std::accumulate(final.begin(), final.end(), TString(""), [](TString a, TString b) { return a + "/" + b; });
	}
	return ret;
}

std::vector<TString> GetCWD(){
	char* cwd = getcwd(NULL,0);
	TString tcwd(cwd);
	delete cwd;
	TObjArray *arr = tcwd.Tokenize("/");
	std::vector<TString> final;
	for(int iToken=0; iToken<arr->GetEntries(); ++iToken)
		final.push_back(static_cast<TObjString*>(arr->At(iToken))->GetString());
	return final;
}

} /* namespace Core */
} /* namespace NA62Analysis */
