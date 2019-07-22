/*
 * ROOTCompare.cpp
 *
 *  Created on: 31 Oct 2016
 *      Author: ncl
 */

#include <iostream>
#include <iomanip>
#include <TFile.h>
#include "DirComparator.hh"
#include "IgnoreList.hh"

#include "TermManip.hh"

using namespace std;

std::string GetNA62AnalysisPath() {
	char* val = std::getenv("ANALYSISFW_PATH");
	if (val == nullptr)
		return std::string(".");

	return std::string(val);
}

void usage(){
	std::cout << "ROOTCompare file1 file2 [-ai]" << std::endl;
	std::cout << "  file1,file2: ROOT files to compare" << std::endl;
	std::cout << "  -a: Verify all entries of the trees. Without this option, only the first entry is compared." << std::endl;
	std::cout << "  -i: Ignore failures and continue comparison. Without this option the comparison will stop when a difference is spotted at the current directory level." << std::endl << std::endl;
	std::cout << "  -o: Compare only object specified here" << std::endl;
	std::cout << "  -e: Tree entry to check" << std::endl << std::endl;
}

int main(int argc, char** argv){

	if(argc<3){
		std::cout << "Error: missing arguments" << std::endl;
		usage();
		exit(0);
	}
	TString objectChecked;
	TString objectChecked2;
	for(int iArg=3; iArg<argc; ++iArg){
		BaseComparator::gCheckAllEntries = (TString(argv[iArg]).CompareTo("-a")==0);
		BaseComparator::gIgnoreFailures = (TString(argv[iArg]).CompareTo("-i")==0);
		if((TString(argv[iArg]).CompareTo("-o")==0)){
			if(objectChecked.IsWhitespace())
				objectChecked = argv[++iArg];
			else
				objectChecked2 = argv[++iArg];
		}
		if((TString(argv[iArg]).CompareTo("-e")==0)){
		    BaseComparator::gFirstEntry = TString(argv[++iArg]).Atoi();
		}
	}

	NA62Analysis::manip::enableManip = true;
	//All objects whose name corresponds to the ones found in the objignore.txt file will not be compared
	IgnoreList::getInstance()->readIgnoreList(TString::Format("%s/Tools/objignore.txt", GetNA62AnalysisPath().data()));

	TFile * f1 = TFile::Open(argv[1], "READ");
	TFile * f2 = TFile::Open(argv[2], "READ");

	//Start comparison
	BaseComparator *mainComp;
	if(objectChecked.IsWhitespace()){
		mainComp = new DirComparator("", f1, f2);
		mainComp->setIsRoot(true);
	}
	else{
		TObject *obj1, *obj2;
		obj1 = f1->Get(objectChecked);
		if(objectChecked2.IsWhitespace())
			obj2 = f2->Get(objectChecked);
		else
			obj2 = f2->Get(objectChecked2);
		if(!obj1 || !obj2){
			std::cout << "Verifying " << std::setw(100) << std::left << TString::Format("/%s", objectChecked.Data()) << "... ";
			TString where;
			if(!obj1)
				where += "left,";
			if(!obj2)
				where += "right";
			BaseComparator::printBad(TString::Format("Missing from %s: %s", where.Data(), objectChecked.Data()));
			std::cout << "Comparison successful... File are different" << std::endl;
			exit(0);
		}
		mainComp = BaseComparator::getComparatorForObject("", obj1);
		mainComp->setRightObject(obj2);
	}
	mainComp->readLeftStructure();
	mainComp->readRightStructure();

	if(!mainComp->compare())
		std::cout << "Comparison successful... File are different" << std::endl;
	else
		std::cout << "Comparison successful... File are identical" << std::endl;
}
