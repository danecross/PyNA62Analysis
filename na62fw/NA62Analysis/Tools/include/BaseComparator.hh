/*
 * BaseComparator.h
 *
 *  Created on: 31 Oct 2016
 *      Author: ncl
 */

#ifndef TOOLS_BASECOMPARATOR_H_
#define TOOLS_BASECOMPARATOR_H_

#include <map>
#include <vector>

#include <TDirectory.h>

template<typename T>
std::string join(const std::vector<T>& v, char c) {
	std::string s;

	for (typename std::vector<T>::const_iterator p = v.begin(); p != v.end(); ++p) {
		s += *p;
		if (p != v.end() - 1)
			s += c;
	}
	return s;
}


class BaseComparator {
public:
	BaseComparator();
	BaseComparator(TString parent, TObject *obj1);
	BaseComparator(TString parent, TObject *obj1, TObject *obj2);
	virtual ~BaseComparator();

	typedef std::pair<TString, BaseComparator*> map_pair;
	typedef std::map<TString, BaseComparator*> map_comp;

	void setLeftObject(TObject *o) { f1 = o; }
	void setRightObject(TObject *o) { f2 = o; }

	virtual void readLeftStructure() = 0;
	virtual void readRightStructure() = 0;


	virtual bool compare();
	bool hasBoth() { return (f1!=nullptr && f2!=nullptr);}

	static map_comp readStructure(bool isRoot, TDirectory* fd, TString parentName, map_comp &objects, int& nObjects);
	static BaseComparator* getComparatorForObject(TString parentName, TObject* object);
	static void printBad(TString s);
	static void printGood(TString s);
	static void printWarning(TString s);

	void print();
	virtual TString comparatorName() {
		return "BaseComparator";
	}

	void setIsRoot(bool isRoot) {
		fIsRoot = isRoot;
	}

	static bool gCheckAllEntries;
	static bool gIgnoreFailures;
	static int  gFirstEntry;

protected:
	virtual bool compareSub();

	bool fIsRoot;
	int fNLeftObjects, fNRightObjects;
	TObject *f1, *f2;
	TString fParentName;
	map_comp fSubCompare;
};

#endif /* TOOLS_BASECOMPARATOR_H_ */
