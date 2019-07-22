#ifndef MCADD4CPP_H
#define MCADD4CPP_H

#include "TVector3.h"
#include "TLorentzVector.h"

int  mcadd4cpp(int ipid, const TLorentzVector &momentum);
int  mcadd4pol3cpp(int ipid, const TLorentzVector &momentum, const TVector3 &polar);
int  mcadd4gencpp(int ipid, const TLorentzVector &momentum, int iflag=0);
void SetKeepFlag(int partIndex, int flag);
void dboost(const TLorentzVector& parent, const double massParent, TLorentzVector& daughter);

void pi0decay_manager_cpp(int, TLorentzVector, int);

#endif /* MCADD4CPP_H */
