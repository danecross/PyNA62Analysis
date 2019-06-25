// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------
/// \class KinFitConstraint
/// \Brief
/// Abstract class for constraints in NA62KinFit 
/// \EndBrief
/// \Detailed
/// This abstract class is used in NA62KinFit by every constraint derived class.
/// Do not try to use it directly, it will not work.
///
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitConstraint.hh"

using namespace std;

KinFitConstraint::KinFitConstraint() :
  Derivative(nullptr),
  dVector(nullptr),
  NEq(0)
{
}

KinFitConstraint::~KinFitConstraint(){
}

