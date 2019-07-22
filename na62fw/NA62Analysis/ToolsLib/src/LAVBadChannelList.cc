#include <iostream>
#include <fstream>
using namespace std;

#include <Rtypes.h>

#include "LAVBadChannelList.hh"


LAVBadChannelList* LAVBadChannelList::fInstance = 0;


LAVBadChannelList::LAVBadChannelList(const char listfile[]) {
  
  fstream map(listfile, ios_base::in);
  
  cout << "Reading bad channel list from " << listfile << endl;
  
  for (Int_t i = 0; i < MAX_BADLIST; i++) fBadList[i] = kFALSE;
  
  Int_t i;
  while (map >> i) {
    if (i >= 0 && i < MAX_BADLIST) fBadList[i] = kTRUE;
    fnBadList++;
  }

}


LAVBadChannelList* LAVBadChannelList::GetInstance() {

  // if (fInstance == 0) {fInstance = new LAVBadChannelList();}
  return fInstance;

}


Bool_t LAVBadChannelList::IsBad(Int_t id) {
  return fBadList[id];
}
