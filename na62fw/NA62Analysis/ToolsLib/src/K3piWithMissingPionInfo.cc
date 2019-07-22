#include "K3piWithMissingPionInfo.hh"

K3piWithMissingPionInfo::K3piWithMissingPionInfo() :
  fDownstreamTrackId(std::make_pair(-1, -1)),
  fChodId(std::make_pair(-1, -1)),
  fKtagId(-1),
  fGtkId(-1),
  fPairTime(-9999.),
  fKaonTime(-9999.),
  fTrackFourMomenta(std::make_pair(TLorentzVector(-9999., -9999., -9999., -9999.),
                                   TLorentzVector(-9999., -9999., -9999., -9999.)))
{
  fKaonMomentum.SetXYZM(-9999., -9999., -9999., -9999.); 
  fMissingPionMomentum.SetXYZM(-9999., -9999., -9999., -9999.); 
  fPairKaonVertex.SetXYZ(-9999., -9999., -9999.); 
}

K3piWithMissingPionInfo::~K3piWithMissingPionInfo(){
}

