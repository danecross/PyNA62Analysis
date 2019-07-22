//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// Modified by Sergey Podolsky (siarhei.padolski@cern.ch) 2012-09-14 
// Modified by Sergey Podolsky (siarhei.padolski@cern.ch) 2012-11-29
// --------------------------------------------------------------------

#ifndef LKrHit_h
#define LKrHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"
#include "TClonesArray.h"
#include "TLKrMicroCellHit.hh"

#include "boost/multi_index_container.hpp"
#include "boost/multi_index/member.hpp"
#include "boost/multi_index/ordered_index.hpp"
#include "boost/multi_index/hashed_index.hpp"
#include "boost/call_traits.hpp"
#include "boost/multi_index_container.hpp"
#include "boost/multi_index/composite_key.hpp"
#include "boost/multi_index/member.hpp"
#include "boost/multi_index/ordered_index.hpp"
#include "boost/next_prior.hpp"
#include "boost/tokenizer.hpp"

class LKrHit : public G4VHit
{
    public:

        LKrHit();
        explicit LKrHit(G4LogicalVolume* logVol);
        ~LKrHit();
        LKrHit(const LKrHit &right);
        LKrHit& operator=(const LKrHit &right);
        G4int operator==(const LKrHit &right) const;

        inline void *operator new(size_t);
        inline void operator delete(void *aHit);
        inline void AddEnergy(G4double, G4int, G4int, G4int);

        void Draw();
        void Print();

    public:

        inline G4int         GetChannelID()                                     { return fChannelID;                    };
        inline void          SetChannelID(G4int value)                          { fChannelID = value;                   };
        inline G4int         GetTrackID()                                       { return fTrackID;                      };
        inline void          SetTrackID(G4int value)                            { fTrackID = value;                     };

        inline G4double      GetTime()                                          { return fTime;                         };
        inline void          SetTime(G4double value)                            { fTime = value;                        };
        inline G4double      GetEnergy()                                        { return fEnergy;                       };
        inline void          SetEnergy(G4double value)                          { fEnergy = value;                      };

        TClonesArray *       GetMicroCellData()                                 { return fMicroCellData;                };
        void                 SetMicroCellData(TClonesArray * value)             { fMicroCellData = value;               };

        inline G4double      GetCurrent() { return fCurrent; };
        inline void          SetCurrent(G4double value) { fCurrent = value; };

        inline G4double      GetMaxEnergyDeposit()                              { return fMaxEnergyDeposit;             };
        inline void          SetMaxEnergyDeposit(G4double value)                { fMaxEnergyDeposit = value;            };

        inline G4ThreeVector GetPosition()                                      { return fPosition;                     };
        inline void          SetPosition(G4ThreeVector value)                   { fPosition = value;                    };

    private:

        G4int fChannelID;
        G4int fTrackID;

        G4double fTime;
        G4double fEnergy;
        TClonesArray * fMicroCellData;
 
        G4double fCurrent;
 
        G4double fMaxEnergyDeposit;

        G4ThreeVector fPosition;
	
	struct CellCoordTypr
	{
	    int x_;
	    int y_;
	    int z_;
	    int N_;
	    CellCoordTypr(int x, int y, int z,  int N):x_(x),y_(y),z_(z), N_(N) {}
	};

	typedef boost::multi_index_container<
	    CellCoordTypr,
	    boost::multi_index::indexed_by<
	    boost::multi_index::ordered_unique< 
	    boost::multi_index::composite_key<CellCoordTypr,
	    boost::multi_index::member<CellCoordTypr, int, &CellCoordTypr::x_>, 
	    boost::multi_index::member<CellCoordTypr, int, &CellCoordTypr::y_>, 
	    boost::multi_index::member<CellCoordTypr, int, &CellCoordTypr::z_>
	    >
	   >
	  >
	> CellCoord_set;
	CellCoord_set CellCoord_set_;
};

typedef G4THitsCollection<LKrHit> LKrHitsCollection;

extern G4Allocator<LKrHit> LKrHitAllocator;

inline void* LKrHit::operator new(size_t)
{
    void *aHit;
    aHit = static_cast<void *>(LKrHitAllocator.MallocSingle());
    return aHit;
}

inline void LKrHit::operator delete(void *aHit) {
    LKrHitAllocator.FreeSingle(static_cast<LKrHit*>(aHit));
}

inline void LKrHit::AddEnergy(G4double value, G4int idx, G4int idy, G4int idz){
    fEnergy += value;
    TClonesArray &MicroCellData = *fMicroCellData;
    G4int NEntries = fMicroCellData->GetEntries(); 
    //int iEntry = NEntries;

    CellCoord_set::iterator it=CellCoord_set_.find(boost::tuples::make_tuple(idx,idy,idz));
    if (it != CellCoord_set_.end() )
    {
      Int_t iEntry = it->N_;
      TLKrMicroCellHit * Hit = static_cast<TLKrMicroCellHit*>(MicroCellData[iEntry]);
      Hit->SetEnergyFraction(value + Hit->GetEnergyFraction());
    }
    else
    {
      CellCoord_set_.insert(CellCoordTypr(idx, idy, idz, NEntries));
      TLKrMicroCellHit * Hit = new(MicroCellData[NEntries]) TLKrMicroCellHit;
      Hit->SetXIndex(idx);
      Hit->SetYIndex(idy);
      Hit->SetZIndex(idz);
      Hit->SetEnergyFraction(value);
    }
}
#endif
