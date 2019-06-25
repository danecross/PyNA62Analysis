
#ifndef TOOLS_INCLUDE_TBRANCHCOMPARATOR_TCC_
#define TOOLS_INCLUDE_TBRANCHCOMPARATOR_TCC_

#include "BaseComparator.hh"
#include "IgnoreList.hh"
#include "TBranchComparator.hh"

#include <TString.h>

template <class T>
void TBranchComparator::readStructure(T* fd, TString parentName, map_comp &objects, int& nObjects) {
	TString thisName;
	thisName = TString::Format("%s/%s", parentName.Data(), fd->GetName());

	TObjArray *keys = fd->GetListOfBranches();
	nObjects = keys->GetEntries();

	TString objName, objClass;
	TBranchElement *key;
	BaseComparator::map_comp::iterator it;

	for(int i=0; i<nObjects; ++i){
		key = static_cast<TBranchElement*>(keys->At(i));
		objName = key->GetName();
		objClass = keys->At(i)->ClassName();
		if(IgnoreList::getInstance()->isIgnored(objName))
			continue;
		it = objects.find(objName);
		if (it == objects.end()) {
			if(objClass.CompareTo("TBranchElement")==0)
				objects.insert(BaseComparator::map_pair(TString(objName), new TBranchComparator(thisName, key)));
			else if(objClass.CompareTo("TBranch")==0)
				objects.insert(BaseComparator::map_pair(TString(objName), new TBranchComparator(thisName, key)));
		} else {
			objects[objName]->setRightObject(key);
		}
	}
}

#endif /* TOOLS_INCLUDE_TBRANCHCOMPARATOR_TCC_ */
