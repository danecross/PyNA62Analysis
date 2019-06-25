#ifndef LAVBadChannelList_H
#define LAVBadChannelList_H

#include <Rtypes.h>

#define MAX_BADLIST 130000


class LAVBadChannelList {

public:

  explicit LAVBadChannelList(const char file[]);
  static LAVBadChannelList* GetInstance(); 
  Bool_t IsBad(Int_t id);


private:

  static LAVBadChannelList* fInstance;

  Int_t fnBadList;
  Bool_t fBadList[MAX_BADLIST];
  
};

#endif
