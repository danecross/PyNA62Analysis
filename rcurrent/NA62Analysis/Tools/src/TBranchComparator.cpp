/*
 * TBranchComparator.cpp
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#include "TBranchComparator.hh"
#include "TTreeComparator.hh"
#include "IgnoreList.hh"
#include <iostream>
#include <numeric>
#include <TLeaf.h>
#include "TBranchComparator.tcc"

TBranchComparator::TBranchComparator() : fGood(true) {
}

TBranchComparator::~TBranchComparator() {
}

TBranchComparator::TBranchComparator(TString parent, TObject* obj1) : BaseComparator(parent, obj1), fGood(true){
}

TBranchComparator::TBranchComparator(TString parent, TObject* obj1, TObject* obj2) : BaseComparator(parent, obj1, obj2), fGood(true){
}

void TBranchComparator::readLeftStructure() { readStructure<TBranchElement>(static_cast<TBranchElement*>(f1), fParentName, fSubCompare, fNLeftObjects); }
void TBranchComparator::readRightStructure() { readStructure<TBranchElement>(static_cast<TBranchElement*>(f2), fParentName, fSubCompare, fNRightObjects); }

bool TBranchComparator::isKnownType(TString type) {
	if(type.CompareTo("Bool_t")==0)
		return true;
	else if(type.CompareTo("Int_t")==0)
		return true;
	else if(type.CompareTo("UInt_t")==0)
		return true;
	else if(type.CompareTo("Long_t")==0)
		return true;
	else if(type.CompareTo("ULong_t")==0)
		return true;
	else if(type.CompareTo("Long64_t")==0)
		return true;
	else if(type.CompareTo("ULong64_t")==0)
		return true;
	else if(type.CompareTo("Double_t")==0)
		return true;
	else if(type.CompareTo("Float_t")==0)
		return true;
	else if(type.CompareTo("TArrayI")==0)
		return true;
	else if(type.CompareTo("TArrayD")==0)
		return true;
	else if(type.CompareTo("TVector3")==0)
		return true;

	return false;
}

bool TBranchComparator::compareElement(TLeaf* ll, TLeaf* lr){
	if(!isKnownType(ll->GetTypeName())){
	    if(ll->IsOnTerminalBranch())
	        fWarning = TString::Format("Unknown type %s", ll->GetTypeName());
	    else
	        fWarning = TString::Format("Checking sub-branches");
	    return true;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Bool_t")==0 && (ll->GetTypedValue<Bool_t>()!=lr->GetTypedValue<Bool_t>()) ){
		fError = TString::Format("Value (%i,%i)", ll->GetTypedValue<Bool_t>(), lr->GetTypedValue<Bool_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Int_t")==0 && (ll->GetTypedValue<Int_t>()!=lr->GetTypedValue<Int_t>()) ){
	    if(isnan(ll->GetTypedValue<Int_t>()) && isnan(lr->GetTypedValue<Int_t>())) return true;
		fError = TString::Format("Value (%i,%i)", ll->GetTypedValue<Int_t>(), lr->GetTypedValue<Int_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("UInt_t")==0 && (ll->GetTypedValue<UInt_t>()!=lr->GetTypedValue<UInt_t>()) ){
	    if(isnan(ll->GetTypedValue<UInt_t>()) && isnan(lr->GetTypedValue<Int_t>())) return true;
		fError = TString::Format("Value (%i,%i)", ll->GetTypedValue<UInt_t>(), lr->GetTypedValue<UInt_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Long_t")==0 && (ll->GetTypedValue<Long_t>()!=lr->GetTypedValue<Long_t>()) ){
		fError = TString::Format("Value (%li,%li)", ll->GetTypedValue<Long_t>(), lr->GetTypedValue<Long_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("ULong_t")==0 && (ll->GetTypedValue<ULong_t>()!=lr->GetTypedValue<ULong_t>()) ){
		fError = TString::Format("Value (%lu,%lu)", ll->GetTypedValue<ULong_t>(), lr->GetTypedValue<ULong_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Long64_t")==0 && (ll->GetTypedValue<Long64_t>()!=lr->GetTypedValue<Long64_t>()) ){
	    if(isnan(ll->GetTypedValue<Long64_t>()) && isnan(lr->GetTypedValue<Long64_t>())) return true;
		fError = TString::Format("Value (%lli,%lli)", ll->GetTypedValue<Long64_t>(), lr->GetTypedValue<Long64_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("ULong64_t")==0 && (ll->GetTypedValue<ULong64_t>()!=lr->GetTypedValue<ULong64_t>()) ){
		fError = TString::Format("Value (%lli,%lli)", ll->GetTypedValue<ULong64_t>(), lr->GetTypedValue<ULong64_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Double_t")==0 && (ll->GetTypedValue<Double_t>()!=lr->GetTypedValue<Double_t>()) ){
	    if(isnan(ll->GetTypedValue<Double_t>()) && isnan(lr->GetTypedValue<Double_t>())) return true;
		fError = TString::Format("Value (%f,%f)", ll->GetTypedValue<Double_t>(), lr->GetTypedValue<Double_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("Float_t")==0 && (ll->GetTypedValue<Float_t>()!=lr->GetTypedValue<Float_t>()) ){
	    if(isnan(ll->GetTypedValue<Float_t>()) && isnan(lr->GetTypedValue<Float_t>())) return true;
		fError = TString::Format("Value (%f,%f)", ll->GetTypedValue<Float_t>(), lr->GetTypedValue<Float_t>());
		return false;
	}
	else if(TString(ll->GetTypeName()).CompareTo("TArrayI")==0){
		TArrayI *al = static_cast<TArrayI*>(ll->GetValuePointer());
		TArrayI *ar = static_cast<TArrayI*>(lr->GetValuePointer());
		if(al->GetSize()!=ar->GetSize() || al->GetSum()!=ar->GetSum()){
			fError = TString::Format("Entries[sum] (%i[%f],%i[%f])", al->GetSize(), al->GetSum(), ar->GetSize(), ar->GetSum());
			return false;
		}
	}
	else if(TString(ll->GetTypeName()).CompareTo("TArrayD")==0){
		TArrayD *al = static_cast<TArrayD*>(ll->GetValuePointer());
		TArrayD *ar = static_cast<TArrayD*>(lr->GetValuePointer());
		if(al->GetSize()!=ar->GetSize() || al->GetSum()!=ar->GetSum()){
			fError = TString::Format("Entries[sum] (%i[%f],%i[%f])", al->GetSize(), al->GetSum(), ar->GetSize(), ar->GetSum());
			return false;
		}
	}
	return true;
}

bool TBranchComparator::compare(){
	//BaseComparator::compare();

	if(fNLeftObjects!=fNRightObjects){
		fError = TString::Format("Different number of elements (%i,%i)", fNLeftObjects, fNRightObjects);
		fGood = false;
	}

	if(fGood || BaseComparator::gIgnoreFailures){
		std::vector<TString> missing;
		for (auto entry : fSubCompare) {
			if (!entry.second->hasBoth())
				missing.push_back(entry.first);
			else {
				entry.second->readLeftStructure();
				entry.second->readRightStructure();
			}
		}
		if (missing.size() > 0) {
			fError = TString::Format("Missing %s", join(missing, ',').c_str());
			fGood = false;
		}
	}

	bool goodSub = BaseComparator::compareSub();

	return fGood && goodSub;
}

bool TBranchComparator::compareEntry(){
	TBranchElement* left = static_cast<TBranchElement*>(f1);
	TBranchElement* right = static_cast<TBranchElement*>(f2);

	TObjArray *branchesL = left->GetListOfLeaves();
	TObjArray *branchesR = right->GetListOfLeaves();
	int nBrL = branchesL->GetEntries();
	int nBrR = branchesR->GetEntries();

	if(nBrL!=nBrR){
		fError = TString::Format("NLeaves (%i,%i)", nBrL, nBrR);
		fGood = false;
	}
	bool badElement = true;
	for(int i=0; i<nBrL; ++i){
		TLeaf *brL = static_cast<TLeaf*>(branchesL->At(i));
		TLeaf *brR = static_cast<TLeaf*>(branchesR->At(i));
		badElement = compareElement(brL, brR) && badElement;
	}
	fGood = fGood && badElement;

	bool goodSub = compareSub();

	return fGood && goodSub;
}

bool TBranchComparator::compareSub(){
	auto map_and = [](bool a, std::pair<TString, BaseComparator*> b) {
		if(b.second->hasBoth())
			return static_cast<TBranchComparator*>(b.second)->compareEntry() && a;
		else
			return a;
	};
	return std::accumulate(fSubCompare.begin(), fSubCompare.end(), true, map_and);
}

void TBranchComparator::printFinal() {
	BaseComparator::compare();

	if(!fGood)
		printBad(fError);
	else if(fWarning.CompareTo("")!=0)
		printWarning(fWarning);
	else
		printGood("Identical");

	for(auto entry : fSubCompare){
		if(entry.second->hasBoth()){
			static_cast<TBranchComparator*>(entry.second)->printFinal();
		}
	}
}
