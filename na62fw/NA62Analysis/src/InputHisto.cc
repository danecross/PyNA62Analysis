/*
 * IOHisto.cc
 *
 *  Created on: Mar 9, 2015
 *      Author: ncl
 */

#include "InputHisto.hh"

#include <iostream>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TChain.h>

#include "Misc.hh"
#include "BadBursts.hh"
#include "Stream.hh"
#include "NA62ConditionsService.hh"

namespace NA62Analysis {
namespace Core {

InputHisto::InputHisto() :
		InputHandler("IOHisto"), fNewFileOpened(false), fWithMC(false) {
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr
	fIOType = IOHandlerType::kHISTO;
}

InputHisto::InputHisto(const std::string &name) :
		InputHandler(name), fNewFileOpened(false), fWithMC(false) {
	/// \MemberDescr
	/// \param name : Display name
	///
	/// Constructor with name
	/// \EndMemberDescr
	fIOType = IOHandlerType::kHISTO;
}

InputHisto::InputHisto(const InputHisto &c) :
		InputHandler(c), fNewFileOpened(false), fWithMC(false), fInputHistoAdd(c.fInputHistoAdd), fInputHisto(
				c.fInputHisto), fReferenceFileName(c.fReferenceFileName) {
	/// \MemberDescr
	/// \param c: Reference to the object to copy
	///
	/// Copy Constructor
	/// \EndMemberDescr

	fIOType = c.GetIOType();
}

InputHisto::~InputHisto() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr

	for (auto h : fInputHisto)
		delete h.second;
	fInputHisto.clear();
	for (auto h : fInputHistoAdd)
		delete h.second;
	fInputHistoAdd.clear();
}

TH1* InputHisto::GetReferenceTH1(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the requested reference histogram
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the reference histogram from the reference file
	/// \EndMemberDescr

	TFile *fd;
	TH1* tempHisto, *returnHisto = NULL;

	fIOTimeCount.Start();
	TFile* currFile = gFile;
	TString oldDirectory = gDirectory->GetName();
	if (directory.CompareTo("") == 0)
		directory = oldDirectory;
	fIOTimeCount.Stop();

	if (fReferenceFileName.IsNull())
		return NULL;

	fIOTimeCount.Start();
	fd = TFile::Open(fReferenceFileName, "READ");
	fIOTimeCount.Stop();
	if (!fd) {
		std::cout << normal() << "Unable to open reference file "
				<< fReferenceFileName << std::endl;
		return NULL;
	}

	fIOTimeCount.Start();
	tempHisto = static_cast<TH1*>(fd->Get(directory + "/" + name));

	currFile->cd(oldDirectory);
	fIOTimeCount.Stop();
	if (tempHisto) {
		returnHisto = static_cast<TH1*>(tempHisto->Clone(name + "_ref"));
		delete tempHisto;
	} else
		std::cout << extended() << "Histogram " << directory << "/" << name
				<< " not found in reference file" << std::endl;
	fIOTimeCount.Start();
	fd->Close();
	fIOTimeCount.Stop();
	delete fd;
	return returnHisto;
}
TH2* InputHisto::GetReferenceTH2(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the requested reference histogram
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the reference histogram from the reference file
	/// \EndMemberDescr

	TFile *fd;
	TH2* tempHisto, *returnHisto = NULL;

	fIOTimeCount.Start();
	TFile *currFile = gFile;
	TString oldDirectory = gDirectory->GetName();
	if (directory.CompareTo("") == 0)
		directory = oldDirectory;
	fIOTimeCount.Stop();

	if (fReferenceFileName.IsNull())
		return NULL;

	fIOTimeCount.Start();
	fd = TFile::Open(fReferenceFileName, "READ");
	fIOTimeCount.Stop();
	if (!fd) {
		std::cout << normal() << "Unable to open reference file "
				<< fReferenceFileName << std::endl;
		return NULL;
	}

	fIOTimeCount.Start();
	tempHisto = static_cast<TH2*>(fd->Get(directory + "/" + name));

	currFile->cd(oldDirectory);
	fIOTimeCount.Stop();

	if (tempHisto) {
		returnHisto = static_cast<TH2*>(tempHisto->Clone(name + "_ref"));
		delete tempHisto;
	} else
		std::cout << extended() << "Histogram " << directory << "/" << name
				<< " not found in reference file" << std::endl;
	fIOTimeCount.Start();
	fd->Close();
	fIOTimeCount.Stop();
	delete fd;
	return returnHisto;
}

TGraph* InputHisto::GetReferenceTGraph(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the requested reference histogram
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the reference histogram from the reference file
	/// \EndMemberDescr

	TFile *fd;
	TGraph* tempHisto, *returnHisto = NULL;

	fIOTimeCount.Start();
	TFile *currFile = gFile;
	TString oldDirectory = gDirectory->GetName();
	if (directory.CompareTo("") == 0)
		directory = oldDirectory;
	fIOTimeCount.Stop();

	if (fReferenceFileName.IsNull())
		return NULL;

	fIOTimeCount.Start();
	fd = TFile::Open(fReferenceFileName, "READ");
	fIOTimeCount.Stop();
	if (!fd) {
		std::cout << normal() << "Unable to open reference file "
				<< fReferenceFileName << std::endl;
		return NULL;
	}

	fIOTimeCount.Start();
	tempHisto = static_cast<TGraph*>(fd->Get(directory + "/" + name));

	currFile->cd(oldDirectory);
	fIOTimeCount.Stop();

	if (tempHisto) {
		returnHisto = static_cast<TGraph*>(tempHisto->Clone(name + "_ref"));
		delete tempHisto;
	} else
		std::cout << extended() << "Histogram " << directory << "/" << name
				<< " not found in reference file" << std::endl;
	fIOTimeCount.Start();
	fd->Close();
	fIOTimeCount.Stop();
	delete fd;
	return returnHisto;
}

TH1* InputHisto::GetInputHistogram(TString directory, TString name, bool append) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file where this histogram will be searched
	/// \param name : Name of the searched histogram
	/// \param append : \n
	///  - If set to true : When a new file is opened by the TChain the value of the new histogram extracted from this file will be appended to the existing histogram.\n
	///  - If set to false : When a new file is opened by the TChain the current histogram will be replaced by the new one.
	/// \return A pointer to the requested histogram if it was found, else a null pointer.
	///
	/// Request histograms from input file. If already exists, directly return the pointer.
	/// \EndMemberDescr

	TString fullName = directory + TString("/") + name;

	NA62Analysis::NA62MultiMap<TString, TH1*>::type::iterator it;
	if ((it = fInputHisto.find(fullName)) != fInputHisto.end() && !append)
		return it->second;
	else if ((it = fInputHistoAdd.find(fullName)) != fInputHistoAdd.end()
			&& append)
		return it->second;
	if (!fCurrentFile) {
		std::cout << normal() << "Unable to open reference file "
				<< fReferenceFileName << std::endl;
		return nullptr;
	}

	TH1* tempHisto, *returnHisto = nullptr;

	fIOTimeCount.Start();
	tempHisto = static_cast<TH1*>(fCurrentFile->Get(fullName));
	fIOTimeCount.Stop();

	if (tempHisto) {
		returnHisto = static_cast<TH1*>(tempHisto->Clone(name));
		delete tempHisto;
		if (append) {
			fInputHistoAdd.insert(
					std::pair<const TString, TH1*>(fullName, returnHisto));
		} else {
			fInputHisto.insert(std::pair<const TString, TH1*>(fullName, returnHisto));
		}
	} else
		std::cout << extended() << "Histogram " << fullName
				<< " not found in reference file" << std::endl;
	return returnHisto;
}

void InputHisto::SetReferenceFileName(TString fileName) {
	/// \MemberDescr
	/// \param fileName : Path to the reference file
	///
	/// Set the path to the reference file
	/// \EndMemberDescr
	fReferenceFileName = fileName;
}

void InputHisto::UpdateInputHistograms() {
	/// \MemberDescr
	///
	/// Update the input histograms with the one coming from the current input file.
	/// \EndMemberDescr

	std::cout << extended() << "Updating input histograms..." << std::endl;
	NA62Analysis::NA62MultiMap<TString, TH1*>::type::iterator it;
	TString histoPath = "-1";
	TH1* histoPtr = NULL;

	//Update input histograms by appending to existing one
	for (it = fInputHistoAdd.begin(); it != fInputHistoAdd.end(); ++it) {
		//If needed, fetch the histogram in file
		if (histoPath.CompareTo(it->first) != 0) {
			std::cout << debug() << "Appending " << it->first << std::endl;
			if (histoPtr)
				delete histoPtr;
			fIOTimeCount.Start();
			histoPtr = static_cast<TH1*>(fCurrentFile->Get(it->first));
			fIOTimeCount.Stop();
			histoPath = it->first;
		}
		if(it->second->InheritsFrom("TGraph")) {
			TList list;
			list.Add(reinterpret_cast<TGraph*>(histoPtr));
			reinterpret_cast<TGraph*>(it->second)->Merge(&list);
		}
		else it->second->Add(histoPtr, 1.0);
	}
	if (histoPtr)
		delete histoPtr;

	//Update input histograms by replacing the existing one
	histoPath = "-1";
	histoPtr = nullptr;
	for (it = fInputHisto.begin(); it != fInputHisto.end(); ++it) {
		//If needed, fetch the histogram in file
		if (histoPath.CompareTo(it->first) != 0) {
			std::cout << debug() << "Replacing " << it->first << std::endl;
			if (histoPtr)
				delete histoPtr;
			fIOTimeCount.Start();
			histoPtr = static_cast<TH1*>(fCurrentFile->Get(it->first));
			fIOTimeCount.Stop();
			histoPath = it->first;
		}
		it->second->Reset();
		it->second->Add(histoPtr, 1.0);
	}
	if (histoPtr)
		delete histoPtr;

}

bool InputHisto::CheckNewFileOpened() {
	/// \MemberDescr
	/// \return true if a new file has been opened. false otherwise.
	///
	/// Check if a new file has been opened
	/// \EndMemberDescr

	bool ret = fNewFileOpened;
	if (fNewFileOpened) {
		NewFileOpened(fCurrentFileNumber, fCurrentFile);
	}
	fNewFileOpened = false;
	return ret;
}

bool InputHisto::LoadEvent(Long64_t &iEvent) {
	/// \MemberDescr
	/// \param iEvent : Index of the file to load
	/// \param doItFast : Load the event immediately without further check. Assume it is valid.
	/// \return true if the file was loaded successfully, else false
	///
	/// Load the file at index iEvent
	/// \EndMemberDescr
	if (iEvent < GetInputFileNumber()) {
		if (fCurrentFile) {
			fIOTimeCount.Start();
			fCurrentFile->Close();
			fIOTimeCount.Stop();
			delete fCurrentFile;
			fPreviousEvent = fCurrentEvent;
		}
		fIOTimeCount.Start();
		fCurrentFile = TFile::Open(fInputfiles[iEvent], "READ");

		if (!fCurrentFile || checkBadFile()) {
			FileSkipped(iEvent, fInputfiles[iEvent]);
			fIOTimeCount.Stop();
			return false;
		}

                MCInfo MCinfo = GetStreamInfo()->GetMCInfo();
                fWithMC = MCinfo.GetNEvents().size()>0 ? true : false;

                if (fSkipBadBurst && !fWithMC) {
			Long64_t goodEvent = SkipBadBurst(iEvent);

			if(goodEvent==-1){ //No more good files to check
				fIOTimeCount.Stop();
				return false;
			}
			iEvent = goodEvent;
		}
		LoadMCStream(iEvent);
		fIOTimeCount.Stop();
		fCurrentFileNumber = iEvent;
		fNewFileOpened = true;
		fCurrentEvent = fCurrentFileNumber;
	}

	return true;
}

bool InputHisto::Rewind() {
	/// \MemberDescr
	/// \return true if the file was loaded successfully, else false
	///
	/// Load the previous good event without changin any of the other internal state variables.
	/// \EndMemberDescr

	if(fPreviousEvent==-1) return false;

	if (fCurrentFile) {
		fIOTimeCount.Start();
		fCurrentFile->Close();
		fIOTimeCount.Stop();
		delete fCurrentFile;
	}
	fIOTimeCount.Start();
	fCurrentFile = TFile::Open(fInputfiles[fPreviousEvent], "READ");

	fStreamTree->GetEntry(fPreviousEvent);
	fIOTimeCount.Stop();
	return true;
}

bool InputHisto::ReloadLatest() {
	/// \MemberDescr
	/// \return true if the file was loaded successfully, else false
	///
	/// Load the current good event without changin any of the other internal state variables.
	/// \EndMemberDescr

	if(fCurrentEvent==-1) return false;

	if (fCurrentFile) {
		fIOTimeCount.Start();
		fCurrentFile->Close();
		fIOTimeCount.Stop();
		delete fCurrentFile;
	}
	fIOTimeCount.Start();
	fCurrentFile = TFile::Open(fInputfiles[fCurrentEvent], "READ");

	fStreamTree->GetEntry(fCurrentEvent);
	fIOTimeCount.Stop();
	return true;
}

bool InputHisto::GetWithMC() const {
	/// \MemberDescr
	/// \return True if the Streams.MCInfo is filled
	///
	/// Do we have MC available in the files?
	/// \EndMemberDescr

	return fWithMC;
}

bool InputHisto::OpenInput(bool specialOnly){
	return InputHandler::OpenInput(specialOnly);
}

bool InputHisto::checkBadFile() {
/// \MemberDescr
/// \return True if the file is suspected to be empty, else false.
///
/// Check if the current file is suspected to be empty. It is suspected to be
/// empty if it contains only TDirectoryFile entries.
/// \EndMemberDescr

	std::vector<keyPair> dirs = GetListOfKeys("/");
	std::vector<keyPair> items;

	for (auto k : dirs) {
		if (k.className.CompareTo("TDirectoryFile") != 0)
			return false; //We have an object different than a directory
		else {
			items = GetListOfKeys(k.name);
			if(std::any_of(items.begin(), items.end(), [](keyPair &sub){ return sub.className.CompareTo("TDirectoryFile")!=0;}))
				return false;
		}
	}
	std::cout << standard() << "The file " << fCurrentFile->GetName()
			<< " seems to be empty: skipping." << std::endl;
	return true;
}

Long64_t InputHisto::SkipBadBurst(Long64_t currentEntry, bool) {
/// \MemberDescr
/// \param currentEntry : Current event entry processed
/// \return currentEntry
///
/// Do no do any check for histo mode
/// \EndMemberDescr

        if ((UInt_t)currentEntry>=fInputfiles.size()) //No more files to check
		return -1;

	LoadMCStream(currentEntry);
	RecoInfo info = GetStreamInfo()->GetRecoInfo();

	for(UInt_t iEntry=0; iEntry<info.GetBurstID().size(); ++iEntry){
                if(info.GetRunID()[iEntry]!=(UInt_t)NA62ConditionsService::GetInstance()->GetCurrentRunID()){ //update ConditionsService info
                  NA62ConditionsService::GetInstance()->SetCurrentRunID(info.GetRunID()[iEntry]);
                  NA62ConditionsService::GetInstance()->SetCurrentBurstID(info.GetBurstID()[iEntry]);
                  TString CDBSubDir = fWithMC ? "MC" : "Data";
                  NA62ConditionsService::GetInstance()->SetCDBTag(CDBSubDir+"/"+info.GetRevision());
                }
		BadBursts *badBurst = BadBursts::GetInstance();
		if (badBurst->IsBad(info.GetBurstID()[iEntry])) {
			std::cout << normal() << "Skipping bad burst at file "
					<< currentEntry << " (" << fInputfiles[currentEntry] << "): "
					<< "contains run " << info.GetRunID()[iEntry] << " burst " << info.GetBurstID()[iEntry]
					<< std::endl;
			return SkipBadBurst(currentEntry+1); //This entry is not good, check the next one
		}
	}

	return currentEntry;
}

} /* namespace Core */
} /* namespace NA62Analysis */
