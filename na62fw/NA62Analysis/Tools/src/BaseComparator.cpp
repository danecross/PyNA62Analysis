/*
 * BaseComparator.cpp
 *
 *  Created on: 31 Oct 2016
 *      Author: ncl
 */

#include "BaseComparator.hh"

#include "TermManip.hh"
#include "Verbose.hh"

#include <iostream>
#include <iomanip>
#include <numeric>

#include <TKey.h>
#include "DirComparator.hh"
#include "TGraphComparator.hh"
#include "TH1Comparator.hh"
#include "TH2Comparator.hh"
#include "TStringObjComparator.hh"
#include "TTreeComparator.hh"
#include "UnknownComparator.hh"
#include "IgnoreList.hh"

namespace nm = NA62Analysis::manip;

bool BaseComparator::gCheckAllEntries = false;
bool BaseComparator::gIgnoreFailures = false;
int  BaseComparator::gFirstEntry = 0;

BaseComparator::BaseComparator() : fIsRoot(false), fNLeftObjects(0), fNRightObjects(0), f1(nullptr), f2(nullptr) {

}

BaseComparator::~BaseComparator() {
}

BaseComparator::BaseComparator(TString parent, TObject* obj1) : fIsRoot(false), fNLeftObjects(0), fNRightObjects(0), f1(obj1), f2(nullptr), fParentName(parent) {
}

BaseComparator::BaseComparator(TString parent, TObject* obj1, TObject* obj2) : fIsRoot(false), fNLeftObjects(0), fNRightObjects(0), f1(obj1), f2(obj2), fParentName(parent){
}

bool BaseComparator::compare() {
	static int width = 100;
	if(f1)
		std::cout << "Verifying " << std::setw(width) << std::left << TString::Format("%s/%s", fParentName.Data(), f1->GetName()) << "... ";
	else if(f2)
		std::cout << "Verifying " << std::setw(width) << std::left << TString::Format("%s/%s", fParentName.Data(), f2->GetName()) << "... ";
	else
		std::cout << "No object to compare! In " << std::setw(width-15) << std::left << fParentName << "... ";
	return true;
}

bool BaseComparator::compareSub() {
	auto map_and = [](bool a, std::pair<TString, BaseComparator*> b) {
		if(b.second->hasBoth())
			return b.second->compare() && a;
		else
			return a;
	};
	return std::accumulate(fSubCompare.begin(), fSubCompare.end(), true, map_and);
}

BaseComparator::map_comp BaseComparator::readStructure(bool isRoot, TDirectory* fd, TString parentName, BaseComparator::map_comp &objects, int& nObjects) {
	TString thisName;
	if(isRoot)
		thisName = "";
	else
		thisName = TString::Format("%s/%s", parentName.Data(), fd->GetName());
	TList *keys = fd->GetListOfKeys();
	TString objName, objClass;
	TKey *key;
	BaseComparator::map_comp::iterator it;
	nObjects = keys->GetSize();
	for (int i = 0; i < nObjects; ++i) {
		key = static_cast<TKey*>(keys->At(i));
		objName = key->GetName();
		objClass = key->GetClassName();
		if(IgnoreList::getInstance()->isIgnored(objName))
			continue;
		it = objects.find(objName);
		if (it == objects.end()) {
			if (objClass.CompareTo("TDirectoryFile") == 0)
				objects.insert(BaseComparator::map_pair(TString(objName), new DirComparator(thisName, key->ReadObj())));
			else if (objClass.BeginsWith("TH1"))
				objects.insert(BaseComparator::map_pair(TString(objName), new TH1Comparator(thisName, key->ReadObj())));
			else if (objClass.BeginsWith("TH2"))
				objects.insert(BaseComparator::map_pair(TString(objName), new TH2Comparator(thisName, key->ReadObj())));
			else if (objClass.BeginsWith("TGraph"))
				objects.insert(BaseComparator::map_pair(TString(objName), new TGraphComparator(thisName, key->ReadObj())));
			else if (objClass.CompareTo("TObjString") == 0)
				objects.insert(BaseComparator::map_pair(TString(objName), new TStringObjComparator(thisName, key->ReadObj())));
			else if (objClass.CompareTo("TTree") == 0)
				objects.insert(BaseComparator::map_pair(TString(objName), new TTreeComparator(thisName, key->ReadObj())));
			else
				objects.insert(BaseComparator::map_pair(TString(objName), new UnknownComparator(thisName, key->ReadObj(), objName, objClass)));
		} else {
			objects[objName]->setRightObject(key->ReadObj());
		}
	}
	return objects;
}

BaseComparator* BaseComparator::getComparatorForObject(TString parentName, TObject *object){
	TString objName, objClass;
	objName = object->GetName();
	objClass = object->ClassName();
	if (objClass.CompareTo("TDirectoryFile") == 0)
		return new DirComparator(parentName, object);
	else if (objClass.BeginsWith("TH1"))
		return new TH1Comparator(parentName, object);
	else if (objClass.BeginsWith("TH2"))
		return new TH2Comparator(parentName, object);
	else if (objClass.BeginsWith("TGraph"))
		return new TGraphComparator(parentName, object);
	else if (objClass.CompareTo("TObjString") == 0)
		return new TStringObjComparator(parentName, object);
	else if (objClass.CompareTo("TTree") == 0)
		return new TTreeComparator(parentName, object);
	else
		return new UnknownComparator(parentName, object, objName, objClass);
}

void BaseComparator::printBad(TString s) {
	std::cout << nm::red << s << nm::reset << std::endl;
}

void BaseComparator::printGood(TString s) {
	std::cout << nm::green << s << nm::reset << std::endl;
}

void BaseComparator::printWarning(TString s) {
	std::cout << nm::brown << s << nm::reset << std::endl;
}

void BaseComparator::print() {
	std::cout << "(" << comparatorName() << ":" << PRINTVAR(fParentName) << PRINTVAR(f1) << PRINTVAR(f2) << PRINTVAR(fNLeftObjects) << PRINTVAR(fNRightObjects) << ")" << std::endl;
}
