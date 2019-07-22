#ifndef K3PIWITHMISSINGPIONINFO_HH
#define K3PIWITHMISSINGPIONINFO_HH

#include <vector>
#include "TVector3.h"
#include "TLorentzVector.h"

class K3piWithMissingPionInfo {
public:
    K3piWithMissingPionInfo();
    virtual ~K3piWithMissingPionInfo();

    std::pair<Int_t, Int_t> GetDownstreamTrackId() const {return fDownstreamTrackId;}
    std::pair<Int_t, Int_t> GetChodId() const {return fChodId;}
    Int_t GetKtagId() const {return fKtagId;}
    Int_t GetGtkId() const {return fGtkId;}
    Double_t GetPairTime() const {return fPairTime;}
    Double_t GetKaonTime() const {return fKaonTime;}
    std::pair<TLorentzVector, TLorentzVector> GetTrackFourMomenta() const {return fTrackFourMomenta;}
    TLorentzVector GetKaonMomentum() const {return fKaonMomentum;}
    TLorentzVector GetMissingPionMomentum() const {return fMissingPionMomentum;}
    TVector3 GetPairKaonVertex() const {return fPairKaonVertex;}
    
    void SetDownstreamTrackId(const std::pair<Int_t, Int_t> &pair){fDownstreamTrackId = pair;}
    void SetChodId(const std::pair<Int_t, Int_t> &pair){fChodId = pair;}
    void SetKtagId(const Int_t &id){fKtagId = id;}
    void SetGtkId(const Int_t &id){fGtkId = id;}
    void SetPairTime(const Double_t &time){fPairTime = time;}
    void SetKaonTime(const Double_t &time){fKaonTime = time;}
    void SetTrackFourMomenta(const std::pair<TLorentzVector, TLorentzVector> &pair){fTrackFourMomenta = pair;}
    void SetKaonMomentum(const TLorentzVector &mom){fKaonMomentum = mom;}
    void SetMissingPionMomentum(const TLorentzVector &mom){fMissingPionMomentum = mom;}
    void SetPairKaonVertex(const TVector3 &mom){fPairKaonVertex = mom;}
    
private:
    
    std::pair<Int_t, Int_t> fDownstreamTrackId; ///< IDs of the two detected pions as DownstreamTrack objects 
    std::pair<Int_t, Int_t> fChodId; ///< IDs of the CHODAssociationRecords for the two DownstreamTrack objects
    Int_t fKtagId; ///< ID of the TRecoCedarCandidate in time with the pair
    Int_t fGtkId; ///< ID of the TRecoGigaTrackerCandidate matched with the pair

    Double_t fPairTime; ///< Average of the times of CHOD candidates matched with each track
    Double_t fKaonTime; ///< Time of the TRecoCedarCandidate matched with the pair
    std::pair<TLorentzVector, TLorentzVector> fTrackFourMomenta; ///< four momenta of the two tracks in the Pi+ Pi- hypotesys
    TLorentzVector fKaonMomentum; ///< Matched GTK candidate four momentum
    TLorentzVector fMissingPionMomentum; ///< Difference between kaon 4-momentum and tracks 4-momenta
    TVector3 fPairKaonVertex; ///< Position of the common vertex of the kaon and the two tracks. Starting point for the missing pion
};

#endif /* K3PIWITHMISSINGPIONINFO_HH */

