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
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch) 
//
// --------------------------------------------------------------
#ifndef LKrDetector_H
#define LKrDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class LKrElectrodeParameterisation;
class LKrElectrodes;
class LKrBeamPipe;
class LKrLKrVolume;
class LKrCryostat;
class LKrFrontBackPlate;
class LKrColdWindow;
class LKrWarmWindow;

class LKrDetectorMessenger;

class LKrDetector : public NA62VComponent, public NA62VNamed
{

    public:

        ~LKrDetector();
        LKrDetector(G4Material*, G4LogicalVolume*);
        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:

        G4double             GetXLength()                                       { return fXLength;                      };
        void                 SetXLength(G4double value)                         { fXLength = value;                     };
        G4double             GetYLength()                                       { return fYLength;                      };
        void                 SetYLength(G4double value)                         { fYLength = value;                     };
        G4double             GetZLength()                                       { return fZLength;                      };
        void                 SetZLength(G4double value)                         { fZLength = value;                     };

        G4double             GetZPosition()                                     { return fZPosition;                    };
        void                 SetZPosition(G4double value)                       { fZPosition = value;                   };
        void                 SetPathToLKrShowersDb(G4String value)		{ fPathToLKrShowersDb = value;		};


    private:
     LKrDetectorMessenger* fLKrMessenger;

        LKrCryostat * fCryostat;
        LKrBeamPipe * fBeamPipe;
        LKrColdWindow * fLKrColdWindow;
        LKrWarmWindow * fLKrWarmWindow;
	
	G4String fPathToLKrShowersDb;
        G4double fXLength;
        G4double fYLength;
        G4double fZLength;

        G4double fZPosition;

};

#endif
