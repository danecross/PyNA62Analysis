/*
 * DirComparator.cpp
 *
 *  Created on: 31 Oct 2016
 *      Author: ncl
 */

#include "DirComparator.hh"

#include <iostream>

DirComparator::DirComparator() {
}

DirComparator::~DirComparator() {
}

DirComparator::DirComparator(TString parent, TObject* obj1) : BaseComparator(parent, obj1){
}

DirComparator::DirComparator(TString parent, TObject* obj1, TObject* obj2) : BaseComparator(parent, obj1, obj2){
}

bool DirComparator::compare() {
	//This method returns true if the left and right directories are equal.

	//The structure of the directory has already been read.
	//Print compared objects.
	BaseComparator::compare();

	bool badEntries = true;
	//Case where we have a different number of objects at this level in both files
	if(fNLeftObjects!=fNRightObjects){
		printBad(TString::Format("Different number of entries (%i,%i)", fNLeftObjects, fNRightObjects));
		badEntries = false;
	}
	//Continue comparing entries if we have the same number of objects, or if we force it
	if(badEntries || BaseComparator::gIgnoreFailures){
		//Comparing the left and right entries. Do we have a one-to-one correspondence?
		std::vector<TString> missing;
		for(auto entry : fSubCompare){
			if(!entry.second->hasBoth())
				missing.push_back(entry.first);
			else{
				entry.second->readLeftStructure();
				entry.second->readRightStructure();
			}
		}
		if(missing.size()>0){
			printBad(TString::Format("Missing %s", join(missing, ',').c_str()));
			badEntries = false;
		}
		else
			printGood("Identical");
	}

	bool badSub = compareSub();

	return badSub && badEntries;
}

