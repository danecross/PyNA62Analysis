/*
 * TreeComparator.cpp
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#include "TTreeComparator.hh"
#include <TTree.h>
#include <TBranchElement.h>
#include <TLeaf.h>
#include <iostream>
#include <numeric>
#include "TBranchComparator.tcc"

TTreeComparator::TTreeComparator() {
}

TTreeComparator::TTreeComparator(TString parent, TObject* obj1) : TBranchComparator(parent, obj1){
}

TTreeComparator::TTreeComparator(TString parent, TObject* obj1, TObject* obj2) : TBranchComparator(parent, obj1, obj2){
}

TTreeComparator::~TTreeComparator() {
}

void TTreeComparator::readLeftStructure() {
	readStructure<TTree>(static_cast<TTree*>(f1), fParentName, fSubCompare, fNLeftObjects);
}

void TTreeComparator::readRightStructure() {
	readStructure<TTree>(static_cast<TTree*>(f2), fParentName, fSubCompare, fNRightObjects);
}

bool TTreeComparator::compare(){
	bool good = TBranchComparator::compare();

	good = compareEntry() && good;
	printFinal();
	return good;
}

bool TTreeComparator::compareEntry() {
	TTree* left = static_cast<TTree*>(f1);
	TTree* right = static_cast<TTree*>(f2);

	int nEntries = 1;
	int startEntry = 0;
	if(BaseComparator::gFirstEntry)
	    startEntry = BaseComparator::gFirstEntry;
	if(BaseComparator::gCheckAllEntries)
		nEntries = left->GetEntries();

	bool goodSub;
	for (int iEntry = startEntry; iEntry < startEntry + nEntries; ++iEntry) {
		left->GetEntry(iEntry);
		right->GetEntry(iEntry);

		goodSub = compareSub();

		if(!goodSub){
			fError = TString::Format("Entry %i is different", iEntry);
			fGood = false;
			break;
		}
	}
	return goodSub;
}

