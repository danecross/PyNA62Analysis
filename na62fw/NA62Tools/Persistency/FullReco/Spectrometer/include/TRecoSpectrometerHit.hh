// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerHit_H
#define TRecoSpectrometerHit_H

#include "TRecoVHit.hh"

class TRecoSpectrometerHit : public TRecoVHit {

    public:

        TRecoSpectrometerHit();
        ~TRecoSpectrometerHit(){};
        TRecoSpectrometerHit(const TRecoSpectrometerHit&);
        void Clear(Option_t* = "");
        void                 AddUsedDigi()                                      { fNUsedDigis++;                        };
        Double_t             GetDriftTime()                                     { return GetTime();                     };
        void                 SetDriftTime(Double_t value)                       { SetTime(value);                       };

    public:

        Int_t                GetHalfViewID()                                    { return fHalfViewID;                   };
        void                 SetHalfViewID(Int_t value)                         { fHalfViewID = value;                  };
        Int_t                GetViewID()                                        { return fViewID;                       };
        void                 SetViewID(Int_t value)                             { fViewID = value;                      };
        Int_t                GetChamberID()                                     { return fChamberID;                    };
        void                 SetChamberID(Int_t value)                          { fChamberID = value;                   };
        Double_t             GetWireAverageDistance()                           { return fWireAverageDistance;          };
        void                 SetWireAverageDistance(Double_t value)             { fWireAverageDistance = value;         };
        TVector3             GetDirection()                                     { return fDirection;                    };
        void                 SetDirection(TVector3 value)                       { fDirection = value;                   };

        Double_t             GetTimeWidth()                                     { return fTimeWidth;                    };
        void                 SetTimeWidth(Double_t value)                       { fTimeWidth = value;                   };
        Double_t             GetWireDistance()                                  { return fWireDistance;                 };
        void                 SetWireDistance(Double_t value)                    { fWireDistance = value;                };
        Int_t                GetNUsedDigis()                                    { return fNUsedDigis;                   };
        void                 SetNUsedDigis(Int_t value)                         { fNUsedDigis = value;                  };

        Int_t GetTDCID() { return fTDCID; };
        void SetTDCID(Int_t val) { fTDCID = val; };

        TVector3 GetLocalPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fLocalPosition.
            ///
            /// The x coordinate is by default the measured one: indeed only one coordinate is measured per tube-hit.
            /// The not measured coordinates are -9999. (e.g. LR algorithm failed).
            /// \EndMemberDescr

            return fLocalPosition;
        };
        TVector3 GetPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fPosition.
            /// \EndMemberDescr

            return fPosition;
        };
        TVector3 GetOriginalPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fOriginalPosition.
            /// \EndMemberDescr

            return fOriginalPosition;
        };
        Double_t GetTime()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fTime.
            /// \EndMemberDescr

            return fTime;
        };
        Double_t GetEnergy()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fEnergy.
            /// \EndMemberDescr

            return fEnergy;
        };
        Int_t GetStrawID() 
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fStrawID.
            ///
            /// The id of the straw is a number nx, set in HalfView::CreateGeometry(), which runs over the straws in the x view-plane.
            /// The numeration is, according to the plane:
            /// -# plane 0: n0.
            /// -# plane 1: 1000+n1.
            /// -# plane 2: 10000+n2.
            /// -# plane 3: 11000+n3.
            /// \EndMemberDescr

            return fStrawID;
        };
        Int_t GetPlaneID() 
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fPlaneID.
            ///
            /// The view-planes are 0123.
            /// \EndMemberDescr

            return fPlaneID;
        };
        Double_t GetRadius()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fRadius.
            /// \EndMemberDescr

            return fRadius;
        };
        Int_t GetID()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fID.
            /// \EndMemberDescr

            return fID;
        };
        Int_t GetRecoID()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fRecoID.
            /// \EndMemberDescr

            return fRecoID;
        };
        Int_t GetMCID()
        {
            return fMCID;
        };
        Int_t GetSingle()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fSingle.
            /// \EndMemberDescr

            return fSingle;
        }   
        Int_t GetEdgeStatus()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fEdgeStatus.
            /// \EndMemberDescr

          return fEdgeStatus;
        }

        void SetLocalPosition(TVector3 val){fLocalPosition=val;};
        void SetPosition(TVector3 val){fPosition=val;};
        void SetOriginalPosition(TVector3 val){fOriginalPosition=val;};
        void SetTime(Double_t val){fTime=val;};
        void SetEnergy(Double_t val){fEnergy=val;};
        void SetStrawID(Int_t val){fStrawID=val;};
        void SetPlaneID(Int_t val){fPlaneID=val;};
        void SetID(Int_t val){fID=val;};
        void SetRecoID(Int_t val){fRecoID=val;};
        void SetRadius(Double_t val){fRadius=val;};
        void SetMCID(Int_t val){fMCID=val;};
        void SetSingle(Int_t val){fSingle=val;};
        void SetEdgeStatus(Int_t val){fEdgeStatus=val;};
        // overriding methods of TVChannelID, needed due to fChannelID redefinition below
        Int_t GetChannelID()            { return fChannelID;         }
        Int_t SetChannelID(Int_t value) { return fChannelID = TVChannelID::SetChannelID(value); }

    private:
        TVector3 fLocalPosition; ///< Reconstructed tube-hit position in the chamber reference frame.
        TVector3 fPosition; ///< Reconstructed tube-hit position in the laboratory reference frame.
        TVector3 fOriginalPosition; ///< Original hit position as given by NA62MC, but after the delta-ray merging algorithm (see SpectrometerReconstruction)
        Double_t fTime; ///< Time of the hit as given by NA62MC.
        Double_t fEnergy; ///< Energy deposition as given by NA62MC, but after the delta-ray merging algorithm.
        Int_t fStrawID; ///< ID of the hitted straw tube.
        Int_t fPlaneID; ///< ID of the view-plane of the hitted straw tube.
        Double_t fRadius; ///< Measured radius, after digitization.
        Int_t fRecoID; ///< ID of the tube-hit according to the TRecoSpectrometerHit sequence.
        Int_t fID; ///< ID of the tube-hit according to the TSpectrometerHit sequence
        Int_t fTDCID;
        Int_t fMCID;
        Int_t fChannelID;
        Int_t fSingle; ///< Flag the hit as single (no LR ambiguity solution) or not.
        Int_t fEdgeStatus; ///< Status of the edge: 0 only leading, 1 leading and trailing. 

        TVector3   fDirection;
        Int_t      fHalfViewID;
        Int_t      fViewID;
        Int_t      fChamberID;
        Double_t   fWireAverageDistance;
        Double_t fTimeWidth;
        Double_t fWireDistance;

        Int_t fNUsedDigis;

        ClassDef(TRecoSpectrometerHit,1);
};
#endif
