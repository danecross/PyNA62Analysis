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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch)
// Modified (MUV -> MUV2) Rainer Wanke              2010-11-26
//
// --------------------------------------------------------------------
#ifndef MUV2Hit_h
#define MUV2Hit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"


class MUV2Hit : public G4VHit
{
    
public:
    
    MUV2Hit();
    explicit MUV2Hit( G4LogicalVolume* logVol );
    ~MUV2Hit();
    MUV2Hit( const MUV2Hit &right );
    MUV2Hit& operator=(const MUV2Hit &right);
    G4int operator==(const MUV2Hit &right) const;
    
    inline void *operator new(size_t);
    inline void operator delete(void *aHit);
    
    void Draw();
    void Print();
    G4double  SumUp(G4double summenPuffer);
    G4double ChannelSum(G4double fChannel);
    
public:
    
    inline G4int         GetChannelID()                     { return fChannelID;	};
    inline void          SetChannelID(G4int value)          { fChannelID = value;	};
    inline G4int         GetTrackID()                       { return fTrackID;   	};
    inline void          SetTrackID(G4int value)            { fTrackID = value;  	};
    inline G4int         GetScintillatorID()                     { return fScintillatorID;	};
    inline void          SetScintillatorID(G4int value)          { fScintillatorID = value;	};
    
    inline G4double      GetTime()                          { return fTime;      	};
    inline void          SetTime(G4double value)            { fTime = value;     	};
    inline G4double      GetEnergy()                        { return fEnergy;    	};
    inline void          SetEnergy(G4double value)          { fEnergy  = value;  	};
    inline void          AddEnergy(G4double value)          { fEnergy += value;   };
    
    void AddHit (G4ThreeVector HitPosition, G4double Energy,
                 G4double Time, G4double PositionOfHitInScintillatorFrame, G4int TrackID);
    
    inline G4double GetStepLength()                      { return fStepLength;  	};
    inline void          SetStepLength(G4double value)   { fStepLength = value; 	};
    
    inline G4ThreeVector GetPosition()                      { return fPosition;  	};
    inline void          SetPosition(G4ThreeVector value)   { fPosition = value; 	};
    
    inline G4String		GetVolumeName()					  {return fVolumeName;   };
    inline void		    SetVolumeName(G4String value)     {fVolumeName = value;   };
    
    //new for optical photon processes
    inline G4String      GetCreatorProcessName()               { return fCreatorProcessName;  };
    inline void          SetCreatorProcessName(G4String value) { fCreatorProcessName = value; };
    
    inline G4double GetPositionInScintillator()                      { return fPositionInScintillator;  	};
    inline void          SetPositionInScintillator(G4double value)   { fPositionInScintillator = value; 	};
    
private:
    
    G4int fChannelID; // Channel id
    G4int fTrackID;   // Id of track generating hit
    G4int fScintillatorID;
    G4double fTime;   // Global time of hit
    G4double fEnergy; // Energy of hit
    
    G4String fVolumeName;
    
    G4ThreeVector fPosition;      // Global position of hit (used for visualization)
    
    G4String fCreatorProcessName; // new for optical photon processes
    G4double fStepLength;
    G4double fPositionInScintillator;
};

typedef G4THitsCollection<MUV2Hit> MUV2HitsCollection;

extern G4Allocator<MUV2Hit> MUV2HitAllocator;

inline void* MUV2Hit::operator new(size_t)
{
    void *aHit;
    aHit = static_cast<void *>(MUV2HitAllocator.MallocSingle());
    return aHit;
}

inline void MUV2Hit::operator delete(void *aHit)
{
    MUV2HitAllocator.FreeSingle(static_cast<MUV2Hit*>(aHit));
}

#endif


