#include "FastBeam.hh"
#include <iostream>
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"
#include "TString.h"
#include "CLHEP/Units/PhysicalConstants.h"

using namespace std;
using namespace CLHEP;

#define turtleinit turtleinit_
#define turtleloop turtleloop_
#define turtleclose turtleclose_
#define type_of_call

extern "C"
{
  void type_of_call turtleinit(); 
  void type_of_call turtleloop(); 
  void type_of_call turtleclose(); 

  // Common
  void* turtle_common_address(const char*);
}

/// \class FastBeam
/// \Brief
/// Steering class for Turtle beam simulation
/// \EndBrief
/// \Detailed
/// Based on Turtle MC: beam particles are generated according to the Turtle input files
/// found in the NA62Tools/Conditions/MC/beam directory.
/// \EndDetailed

FastBeam::FastBeam() : npart(0) {
/// \MemberDescr
/// \param datacardname  Name of the datacard.
/// Initialization for beam simulation. This method is called by
/// PrimaryGeneratorAction::GeneratePrimaries.
/// The main steps of the initialization are:
/// - datacardname passed to turtle.f;<br>
/// - beam type selected according to the datacard name;<br>
/// - initialization of turtle;<br>
/// - initialization of the common blocks to communicate with turtle.
/// \EndMemberDescr

  // Datacard name
  TString FileName = NA62ConditionsService::GetInstance()->GetFullPath("turtle.dat");
  if (FileName.Length()>=100) {
    cout << "[FastBeam] Error: beam datacard name is too long (>=100 symbols)" << endl;
    exit(kGenericError);
  }
  fTURTLEchar = static_cast<TURTLEchar_t*>(turtle_common_address("turtle_char"));
  for (int j=0; j<FileName.Length(); j++) fTURTLEchar->FILENAME[j] = FileName[j];

  for (int j=0; j<10; j++) {
    type[j]  = 0;
    pmomx[j] = 0.;
    pmomy[j] = 0.;
    pmomz[j] = 0.;
    posx[j]  = 0.;
    posy[j]  = 0.;
    posz[j]  = 0.;
    ttime[j] = 0.;
  }

  // Initialization
  turtleinit();

  // Initialize common blocks
  fTURTLEint    = static_cast<TURTLEint_t*>   (turtle_common_address("turtle_int"));
  fTURTLEdouble = static_cast<TURTLEdouble_t*>(turtle_common_address("turtle_double"));
}

FastBeam::~FastBeam() {
  turtleclose();
}

void FastBeam::Generate() {
/// \MemberDescr
/// Steering routine for beam generation. It is called by
/// PrimaryGeneratorAction::GeneratePrimaries. 
/// \EndMemberDescr

  Reset_OutputVar();
  turtleloop();
  Fill_OutputVar();
}

void FastBeam::Fill_OutputVar() {
/// \MemberDescr
/// Fill the variables of the beam using the output variables
/// of Turtle. 
/// \EndMemberDescr
  npart = fTURTLEint->NPART;
  if (npart>=10) {
    cout << "[Beam] Warning: too many beam particles generated, skipping Fill_OutputVar" << endl;
    return;
  }
  for (int j=0; j<npart; j++) {
    pmomx[j]  = fTURTLEdouble->XPMOM;
    pmomy[j]  = fTURTLEdouble->YPMOM;
    pmomz[j]  = fTURTLEdouble->ZPMOM;
    posx[j]   = fTURTLEdouble->XPOS;
    posy[j]   = fTURTLEdouble->YPOS;
    posz[j]   = fTURTLEdouble->ZPOS;
    ttime[j]  = posz[j]*1000/c_light;
    type[j]   = -1;
  }
}

void FastBeam::Reset_OutputVar() {
/// \MemberDescr
/// Reset the beam variables.
/// \EndMemberDescr

  for (int j=0; j<npart; j++) {
    pmomx[j]  = -999999.;
    pmomy[j]  = -999999.;
    pmomz[j]  = -999999.;
    posx[j]   = -999999.;
    posy[j]   = -999999.;
    posz[j]   = -999999.;
    ttime[j]  = -999999.;
    type[j]   = -1;
  }
  npart = 0;
}
