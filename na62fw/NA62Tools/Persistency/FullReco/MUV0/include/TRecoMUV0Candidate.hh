#ifndef TRecoMUV0Candidate_H
#define TRecoMUV0Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV0Candidate : public TRecoVCandidate {

public:
  TRecoMUV0Candidate();
  ~TRecoMUV0Candidate(){};

  void Clear(Option_t* = "");

private:

  ClassDef(TRecoMUV0Candidate,1);
};
#endif
