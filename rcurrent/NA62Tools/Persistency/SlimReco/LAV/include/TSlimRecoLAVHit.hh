// --------------------------------------------------------------
// History:
//
// 2019-04-12 T. Spadaro (tommaso.spadaro@lnf.infn.it) and S. Martellotti (silvia.martellotti@lnf.infn.it)
//
// --------------------------------------------------------------
#ifndef TSLIMRecoLAVHit_H
#define TSLIMRecoLAVHit_H 1

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TVector3.h"
#include "TSlimRecoVHit.hh"

class TRecoLAVHit;

class TSlimRecoLAVHit : public TSlimRecoVHit {

public:
  TSlimRecoLAVHit();
  explicit TSlimRecoLAVHit(TRecoLAVHit *hitReco);
  virtual ~TSlimRecoLAVHit() = default;

  void SetChannelID(Int_t channelID)          { fChannelID = channelID;                       }
  void SetTime(Float_t time)                  { fTime = time;                                 }
  void SetEdgeMask(Short_t edgeMask)          { fEdgeMask = edgeMask;                         }
  void SetLeadingEdgeLow(Double_t edgeTime)   { fLeadingEdgeLow   = edgeTime; fEdgeMask |= 1; }
  void SetLeadingEdgeHigh(Double_t edgeTime)  { fLeadingEdgeHigh  = edgeTime; fEdgeMask |= 2; }
  void SetTrailingEdgeHigh(Double_t edgeTime) { fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4; }
  void SetTrailingEdgeLow(Double_t edgeTime)  { fTrailingEdgeLow  = edgeTime; fEdgeMask |= 8; }

  Int_t GetChannelID()                            const { return fChannelID;                                              }
  Float_t GetTime()                               const { return fTime;                                                   }
  Short_t GetEdgeMask()                           const { return fEdgeMask;                                               }
  Float_t GetLeadingEdgeLow()                     const { if (fEdgeMask & 1) {return fLeadingEdgeLow;}   else {return 0;} }
  Float_t GetLeadingEdgeHigh()                    const { if (fEdgeMask & 2) {return fLeadingEdgeHigh;}  else {return 0;} }
  Float_t GetTrailingEdgeHigh()                   const { if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;} }
  Float_t GetTrailingEdgeLow()                    const { if (fEdgeMask & 8) {return fTrailingEdgeLow;}  else {return 0;} }
  TVector3 GetBlockPosition()                     const; ///< Returns block position, expressed in mm
  Int_t GetBlockIDFromPhi(Double_t, Int_t, Int_t) const;
  Double_t GetBlockPhiSpan(Int_t)                 const;
  Int_t GetLAVID()                                const;
  Int_t GetLayerID()                              const;
  Int_t GetBananaID()                             const;
  Int_t GetBlockID()                              const;

  Int_t EncodeChannelID();
  Int_t GetPackedChannelID();

  // Methods not implemented at the moment
  //  void TDetectorVHit::AddEnergy(Double_t value )
  //  void TRecoLAVHit::Clear(Option_t * option = "" )
  //  Int_t TVHit::Compare(const TObject * obj ) const [inline]
  //  TVDigi* TRecoVHit::GetDigi( ) [inline]
  //  Bool_t TRecoVHit::GetDigiOwner( ) [inline]
  //  Bool_t TVHit::GetDirectInteraction( ) [inline]
  //  Int_t TVHit::GetMCTrackID( ) [inline]
  //  Bool_t TVHit::IsSortable( ) const [inline]
  //  void TVHit::Print(Option_t * option = "" ) const
  //  void TRecoVHit::SetDigi(TVDigi * value ) [inline]
  //  void TRecoVHit::SetDigiOwner(Bool_t value ) [inline]
  //  void TVHit::SetDirectInteraction(Bool_t value ) [inline, inherited]
  //  void TDetectorVHit::SetEnergy(Double_t value ) [inline, inherited]
  //  void TVHit::SetMCTrackID(Int_t value ) [inline, inherited]
  //  void TDetectorVHit::SetPosition(TVector3 value ) [inline, inherited]
  //  void TVHit::ShiftMCTrackID(Int_t value ) [inline, inherited]
  //  virtual void TDetectorVHit::UpdateReferenceTime(Double_t value ) [inline, virtual, inherited]
  //

  virtual void FromReco(TRecoVHit*);
  virtual void ToReco(TRecoVHit*);
private:

  Int_t   fChannelID;
  Float_t fTime;
  Short_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

  Float_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only
  Float_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only
  Float_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only
  Float_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only

  // Private variables not included that cannot be obtained from the above private vars.
  // Double_t fEnergy
  // TVector3 fPosition
  // Bool_t fDirectInteraction
  // Int_t fMCTrackID

  ClassDef(TSlimRecoLAVHit, 1)
};
#endif /* TSLIMRecoLAVHit_H */
