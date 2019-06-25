/*
 * IgnoreList.h
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_IGNORELIST_HH_
#define TOOLS_INCLUDE_IGNORELIST_HH_

#include <set>
#include <TString.h>

class IgnoreList {
public:
	bool readIgnoreList(TString fileName);
	bool isIgnored(TString objName	) const;

	static IgnoreList *getInstance()
	{
		if (!fInstance)
			fInstance = new IgnoreList;
		return fInstance;
	}
private:
	IgnoreList();
	virtual ~IgnoreList();

	std::set<TString> fIgnored;
	static IgnoreList* fInstance;
};

#endif /* TOOLS_INCLUDE_IGNORELIST_HH_ */
