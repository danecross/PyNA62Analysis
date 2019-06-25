/*
 * OutputHandler.cc
 *
 *  Created on: Jul 15, 2016
 *      Author: nlurkin
 */

#include "OutputHandler.hh"

#include <iostream>
#include <sys/stat.h>

#include <TFile.h>
#include <TSystem.h>

#include "Analyzer.hh"
#include "NA62Exceptions.hh"
#include "Stream.hh"
#include "GitRevision.hh"
#include "ConfigSettings.hh"

#include "TRecoVEvent.hh"
#include "TSlimRecoVEvent.hh"
#include "PersistencyChanger.hh"

namespace NA62Analysis {
namespace Core {

OutputHandler::OutputHandler() :
		fOutFile(0), fStreamTree(nullptr), fStreamEvent(new Stream) {
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr

	fCurrentDir = get_current_dir_name();
}

OutputHandler::OutputHandler(const std::string &name) :
		Verbose(name), fOutFile(0), fStreamTree(nullptr), fStreamEvent(new Stream) {
	/// \MemberDescr
	/// \param name : Display name
	///
	/// Constructor with name
	/// \EndMemberDescr

	fCurrentDir = get_current_dir_name();
}

OutputHandler::OutputHandler(const OutputHandler& c) :
		Verbose(c), fCurrentDir(c.fCurrentDir), fOutFile(c.fOutFile), fOutFileName(
				c.fOutFileName), fStreamTree(c.fStreamTree), fStreamEvent(
				c.fStreamEvent), fExportTrees(c.fExportTrees) {
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	///
	/// Copy constructor
	/// \EndMemberDescr
}

OutputHandler::~OutputHandler() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr

	// #NOQA_VERBOSE
	free(fCurrentDir);
	if (fOutFile) {
		fIOTimeCount.Start();
		std::cout << "############# Writing " << fOutFileName << " #############"
				<< std::endl;
		fOutFile->Purge();
		fOutFile->Close();
		std::cout << "#############        DONE         #############"
				<< std::endl;
		fIOTimeCount.Stop();
	}
}

TString OutputHandler::GetOutputFileName() const {
	/// \MemberDescr
	/// \return Base name of the output file
	/// \EndMemberDescr

	return fOutFileName;
}

void OutputHandler::MkOutputDir(TString name) const {
	/// \MemberDescr
	/// \param name : Name of the directory
	///
	/// Create a new directory in the output file
	/// \EndMemberDescr

	fIOTimeCount.Start();
	if (!fOutFile->FindKey(name))
		fOutFile->mkdir(name);
	fIOTimeCount.Stop();
}

bool OutputHandler::OpenOutput(TString outFileName) {
	/// \MemberDescr
	/// \param outFileName : Path to the output file
	/// \return true if success, else false
	///
	/// Open the output file
	/// \EndMemberDescr

	struct stat s;
	if(stat(outFileName.Data(), &s)==0){
		if(S_ISDIR(s.st_mode))
			outFileName += "/OutFile.root";
	}
	else if(outFileName.EndsWith("/", TString::kIgnoreCase)){
		std::cout << always() << "The specified output directory " << outFileName << " does not exist." << std::endl;
		throw WriteException();
	}
	else {
		TString dirName = gSystem->DirName(outFileName.Data());
		if(stat(dirName.Data(), &s)!=0){
			std::cout << always() << "The specified output directory " << dirName << " does not exist." << std::endl;
			throw WriteException();
		}
	}

	std::cout << extended() << "Opening output file " << outFileName << std::endl;
	fOutFileName = outFileName;
	//fOutFileName.ReplaceAll(".root", "");
	fIOTimeCount.Start();
	fOutFile = new TFile(outFileName, "RECREATE");
	fIOTimeCount.Stop();

	if (!fOutFile)
		return false;
	return true;
}

void OutputHandler::Finalise() {
	/// \MemberDescr
	/// Finalise the IO: purge the output file.
	/// \EndMemberDescr

	SetOutputFileAsCurrent();
	fIOTimeCount.Start();
	fStreamTree = new TTree("Streams", "Streams");
	fStreamTree->Branch("Stream", "Stream", fStreamEvent);
	if(fStreamTree->Fill()<=0){
		std::cout << normal() << "Stream tree filling failed... Aborting run" << std::endl;
		throw WriteException();
	}
	if(fStreamTree->Write()<=0){
		std::cout << normal() << "Stream tree writing failed... Aborting run" << std::endl;
		throw WriteException();
	}
	fOutFile->Purge();
	fIOTimeCount.Stop();
}

void OutputHandler::WriteAnalyzerList(std::vector<Analyzer*> anList) {
	/// \MemberDescr
	/// \param anList : vector containing the list of analyzers to write in the output file
	///
	/// Write in the output file the history of analyzers coming from input file and
	/// append the list of analyzers running here.
	/// \EndMemberDescr

	for (auto an : anList)
		fStreamEvent->GetAnalysisInfo().AddAnalyzer(an->GetIdentifier());

	fStreamEvent->GetAnalysisInfo().AddStreamName(GetModuleName().substr(9,GetModuleName().size()-1));
}

void OutputHandler::AddFileName(std::string name) {
	/// \MemberDescr
	/// \param name: Name of the input file
	///
	/// Add a file name to the output
	/// \EndMemberDescr

	fIOTimeCount.Start();
	//Print fileName in the output file for future reference
	SetOutputFileAsCurrent();
	TString fileName = TString(name);
	if (!fileName.BeginsWith("/") && !fileName.BeginsWith("root://")) {
		//It is not an absolute path
		fileName = TString(fCurrentDir) + "/" + fileName;
	}
	std::cout << debug() << "Writing filename " << fileName << " into Stream" << std::endl;
	AnalysisInfo info;
	info.AddJobInfo(fileName, GetCurrentGitRevision());
	fStreamEvent->GetAnalysisInfo().MergeJobAttributes(info);
	gFile->cd();
	fIOTimeCount.Stop();
}

void OutputHandler::PrepareOutputTreesReco(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree) {
    /// \MemberDescr
    /// \param filterWord: filter result mask. Each bit indicates the result of the corresponding analyzer
    /// \param treeList: list of enabled input TTrees
    /// \param referenceTree: pointer to the reference TTree
    ///
    /// Create the output trees from the list of active input trees.
    /// Add the filterWord to the reference tree.
    /// \EndMemberDescr

    NA62Analysis::NA62Map<TString, TTree*>::type::iterator itTree;

    std::cout << extended() << "Preparing output trees (Reco)" << std::endl;
    std::cout << extended() << "List of disabled trees (Reco):" << std::endl;
    if (CanPrint()) {
        std::cout << extended();
        for (auto it : fDisabledTrees)
            std::cout << it << " ";
        std::cout << std::endl;
    }
    fIOTimeCount.Start();
    std::vector<TString> branchToEnable;

    TTree *clonedReco = nullptr, *origSlim = nullptr;

    for (auto it : treeList) {
        //Need to disable unnecessary branches before cloning or they appear in output tree
        if(it.first.EqualTo("SlimReco")) {
            origSlim = it.second;
            if (Configuration::ConfigSettings::CLI::fFilterToFull) continue; // Skip the SlimReco tree cloning in that case
        }
        std::cout << debug() << "Cloning tree " << it.first << std::endl;
        for (auto bName : fDisabledTrees) {
            if (it.second->GetBranchStatus(bName)) {
                branchToEnable.push_back(bName);
                it.second->GetBranch(bName)->SetStatus(0);
            }
        }
        TTree *clone = it.second->CloneTree(0);
        clone->SetDirectory(fOutFile->GetDirectory("/"));
        int basketSize = Configuration::ConfigSettings::global::fBasketSize;
        int autoflushVal = Configuration::ConfigSettings::global::fAutoFlushValue;

        if (basketSize != 0)
            clone->SetBasketSize("*", basketSize);
        if (autoflushVal != 0)
            clone->SetAutoFlush(autoflushVal);
        fExportTrees.insert(std::pair<const TString, TTree*>(it.first, clone));
        //Need to enable the branches we disabled before and that were supposed to be enabled. Else we read nothing anymore.
        for (auto bName : branchToEnable)
            it.second->GetBranch(bName)->SetStatus(1);

        //Setting manually branch addresses seems to solve issues like NARKD-695,696
        auto blist = clone->GetListOfBranches();
        for (int i = 0; i < blist->GetEntries(); i++) {
            TBranch *br = static_cast<TBranch*>(blist->At(i));
            std::cout << debug() << "  Resetting branch " << br->GetName() << std::endl;
            br->SetAddress(it.second->GetBranch(br->GetName())->GetAddress());
        }

        if(it.first.EqualTo("Reco")) clonedReco = clone;

    }

    //If filter to Full, need to go through all the Slim branches and create them in the Reco tree
    if (Configuration::ConfigSettings::CLI::fFilterToFull && origSlim) {
        TObjArray* branches = origSlim->GetListOfBranches();
        for (int iBranch = 0; iBranch < branches->GetEntries(); ++iBranch) {
            TBranch* branch = static_cast<TBranch*>(branches->At(iBranch));
            TString bName = branch->GetName();
            std::cout << debug() << "Adding branch " << bName << " from SlimReco tree to Reco tree" << std::endl;
            if (origSlim->GetBranchStatus(bName) && std::count(fDisabledTrees.begin(), fDisabledTrees.end(), bName) == 0) {
                auto objects = getRecoAndSlimFromName(bName, reinterpret_cast<void**>(branch->GetAddress()));

                if (objects.first == nullptr) {
                    std::cout << normal() << "Unable to find Slim and Reco classes from branch name " << bName << ". Aborting branching of " << bName << " into SlimReco tree." << std::endl;
                    continue;
                }
                fSlimObjects.push_back(std::make_pair(objects.second, objects.first));
                clonedReco->Branch(bName, fSlimObjects.back().second);
                fDisabledTrees.push_back(bName);
            }
        }

    }

    fIOTimeCount.Stop();

    if ((itTree = fExportTrees.find(referenceTree->GetName())) != fExportTrees.end())
        AddFilterBranch(itTree->second, filterWord);
}

void OutputHandler::PrepareOutputTreesSlim(Long64_t *filterWord, TChain* treeList, TChain *referenceTree) {
    /// \MemberDescr
    /// \param filterWord: filter result mask. Each bit indicates the result of the corresponding analyzer
    /// \param treeList: list of enabled input TTrees
    /// \param referenceTree: pointer to the reference TTree
    ///
    /// Create the output trees from the list of active input trees.
    /// Add the filterWord to the reference tree.
    /// \EndMemberDescr

    std::cout << extended() << "Preparing output trees (Slim)" << std::endl;
    std::cout << extended() << "List of disabled trees (Slim):" << std::endl;
    if(CanPrint()){
        std::cout << extended();
        for (auto it : fDisabledTrees)
            std::cout << it << " ";
        std::cout << std::endl;
    }
    fIOTimeCount.Start();

    TTree *slimTree = new TTree("SlimReco", "SlimReco");

    TObjArray* branches = treeList->GetListOfBranches();
    for(int iBranch=0; iBranch<branches->GetEntries(); ++iBranch){
        TBranch* branch = static_cast<TBranch*>(branches->At(iBranch));
        TString bName = branch->GetName();
        if(treeList->GetBranchStatus(bName) && std::count(fDisabledTrees.begin(), fDisabledTrees.end(), bName)==0) {
            auto objects = getSlimAndRecoFromName(bName, reinterpret_cast<void**>(branch->GetAddress()));

            if(objects.first==nullptr){
                std::cout << normal() << "Unable to find Slim and Reco classes from branch name " <<
                        bName << ". Aborting branching of " << bName << " into SlimReco tree." << std::endl;
                continue;
            }
            fSlimObjects.push_back(objects);
            slimTree->Branch(bName, fSlimObjects.back().first);
            fDisabledTrees.push_back(bName);
        }
    }

    slimTree->SetDirectory(fOutFile->GetDirectory("/"));
    int basketSize = Configuration::ConfigSettings::global::fBasketSize;
    int autoflushVal = Configuration::ConfigSettings::global::fAutoFlushValue;

    if(basketSize!=0)
        slimTree->SetBasketSize("*", basketSize);
    if(autoflushVal!=0)
        slimTree->SetAutoFlush(autoflushVal);
    fExportTrees.insert(std::pair<const TString, TTree*>("RecoSlim", slimTree));

    fIOTimeCount.Stop();

	NA62Analysis::NA62Map<TString, TTree*>::type::iterator itTree;
    if ((itTree = fExportTrees.find(referenceTree->GetName())) != fExportTrees.end())
        AddFilterBranch(itTree->second, filterWord);
}

void OutputHandler::PrepareUserTrees(std::map<TString, TTree*> treeList, bool firstCopy){
    std::cout << extended() << "Preparing user trees" << std::endl;
    fIOTimeCount.Start();
    for (auto it : treeList) {
        TTree *clone;
        if (!firstCopy)
            clone = it.second->CloneTree(0);
        else
            clone = it.second;
        clone->SetDirectory(fOutFile->GetDirectory("/"));
        fUserTrees.insert(std::pair<const TString, TTree*>(it.first, clone));
    }
    fIOTimeCount.Stop();
}

void OutputHandler::WriteEvent(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree) {
    /// \MemberDescr
    /// \param filterWord: filter result mask. Each bit indicates the result of the corresponding analyzer
    /// \param treeList: list of enabled input TTrees
    /// \param referenceTree: pointer to the reference TTree
    ///
    /// Write the event in the output tree.
    /// \EndMemberDescr

    SetOutputFileAsCurrent();

    NA62Analysis::NA62Map<TString, TTree*>::type::iterator itTree;

    std::cout << trace() << "Writing event in output" << std::endl;
    if (fExportTrees.size() == 0) {
        if (Configuration::ConfigSettings::CLI::fFilterToSlim && treeList.count("Reco")) {
            PrepareOutputTreesSlim(filterWord, treeList["Reco"], referenceTree);
        }
        PrepareOutputTreesReco(filterWord, treeList, referenceTree);
    }

    if (Configuration::ConfigSettings::CLI::fFilterToSlim && fSlimObjects.size() > 0) {
        // Need to update the Slim objects before filling
        for (auto branch : fSlimObjects)
            branch.first->FromReco(static_cast<TRecoVEvent*>(branch.second));
    }

    if (Configuration::ConfigSettings::CLI::fFilterToFull && fSlimObjects.size() > 0) {
        // Need to update the Reco objects before filling
        for (auto branch : fSlimObjects)
            branch.first->ToReco(static_cast<TRecoVEvent*>(branch.second));
    }

    fIOTimeCount.Start();
    for (itTree = fExportTrees.begin(); itTree != fExportTrees.end(); ++itTree) {
        if (itTree->first.EqualTo("SpecialTrigger"))
            continue; //Skip the special trigger tree. Events are not corresponding. This will be done in WriteSpecialEvent

        if (itTree->second->Fill() <= 0) {
            std::cout << normal() << "TTree filling failed... Aborting run" << std::endl;
            throw WriteException();
        }
    }
    fIOTimeCount.Stop();
}

void OutputHandler::WriteSpecialEvent(Long64_t *filterWord, NA62Analysis::NA62Map<TString, TChain*>::type treeList, TChain *referenceTree) {
	/// \MemberDescr
	/// \param filterWord: filter result mask. Each bit indicates the result of the corresponding analyzer
	/// \param treeList: list of enabled input TTrees
	/// \param referenceTree: pointer to the reference TTree
	///
	/// Write the special event in the output tree.
	/// \EndMemberDescr

	SetOutputFileAsCurrent();

	NA62Analysis::NA62Map<TString, TTree*>::type::iterator itTree;

	std::cout << extended() << "Writing special event in output" << std::endl;
	if (fExportTrees.size() == 0){
        if(Configuration::ConfigSettings::CLI::fFilterToSlim && treeList.count("Reco")){
            PrepareOutputTreesSlim(filterWord, treeList["Reco"], referenceTree);
        }
        PrepareOutputTreesReco(filterWord, treeList, referenceTree);
	}

	//Nothing to do here. No slim special trigger event exists

	fIOTimeCount.Start();
	itTree = fExportTrees.find("SpecialTrigger");
	if(itTree != fExportTrees.end())
		if(itTree->second->Fill()<=0){
			std::cout << normal() << "TTree filling failed... Aborting run" << std::endl;
			throw WriteException();
		}
	fIOTimeCount.Stop();
}

void OutputHandler::WriteUserEvent(std::map<TString, TTree*> treeList, bool firstCopy, TString name){
	SetOutputFileAsCurrent();

	std::cout << extended() << "Writing user event in output" << std::endl;
	if (fUserTrees.size() == 0)
		PrepareUserTrees(treeList, firstCopy);

	fIOTimeCount.Start();
	if(name.CompareTo("")==0){
		for (auto treesToFill : treeList){
			auto itTree = fUserTrees.find(treesToFill.first);
			if(itTree!=fUserTrees.end()) {
				if(itTree->second->Fill()<=0){
					std::cout << normal() << "TTree filling failed... Aborting run" << std::endl;
					throw WriteException();
				}
			}
		}
	}
	else{
		auto itTree = fUserTrees.find(name.Data());
		if(itTree!=fUserTrees.end()) {
			if(itTree->second->Fill()<=0){
				std::cout << normal() << "TTree filling failed... Aborting run" << std::endl;
				throw WriteException();
			}
		}
	}
	fIOTimeCount.Stop();

}

void OutputHandler::WriteTree() const {
	/// \MemberDescr
	/// Write the output trees in the output file
	/// \EndMemberDescr

	SetOutputFileAsCurrent();

	std::cout << extended() << "Writing output trees" << std::endl;
	for (auto itTree : fExportTrees) {
		fIOTimeCount.Start();
		if(itTree.second->Write()<=0){
			std::cout << normal() << "TTree writing failed... Aborting run" << std::endl;
			throw WriteException();
		}
		fIOTimeCount.Stop();
	}

	std::cout << extended() << "Writing user trees" << std::endl;
	for (auto itTree : fUserTrees) {
		fIOTimeCount.Start();
		if(itTree.second->Write()<=0){
			std::cout << normal() << "TTree writing failed... Aborting run" << std::endl;
			throw WriteException();
		}
		fIOTimeCount.Stop();
	}
}

void OutputHandler::AddFilterBranch(TTree* exportTree, Long64_t *filterWord) {
	/// \MemberDescr
	/// \param exportTree : TTree in which the filterword should be written
	/// \param filterWord: pointer to the filterword variable to  input in the TTree
	///
	/// Append the FilterWord branch in the TTree.
	/// \EndMemberDescr

	exportTree->Branch("FilterWord", filterWord, "fFilterWord/L");
}

void OutputHandler::AddStreamEvent(Stream* streamEvent) {
	fStreamEvent->UpdateAndMergeAttributes(*streamEvent);
}

} /* namespace Core */
} /* namespace NA62Analysis */

