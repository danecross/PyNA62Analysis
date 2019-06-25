/*
 * IgnoreList.cpp
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#include "IgnoreList.hh"

#include <fstream>
#include <iostream>

IgnoreList *IgnoreList::fInstance = 0;

IgnoreList::IgnoreList() {
}

IgnoreList::~IgnoreList() {
}

bool IgnoreList::readIgnoreList(TString fileName) {
	std::ifstream fd(fileName);
	if(!fd.is_open()){
		std::cout << "Failed to open ignore file: " << fileName << std::endl;
		return false;
	}

	TString line;
	while(!fd.eof()){
		fd >> line;
		fIgnored.insert(line);
	}
	return true;
}

bool IgnoreList::isIgnored(TString objName) const{
	return fIgnored.count(objName)>0;
}
