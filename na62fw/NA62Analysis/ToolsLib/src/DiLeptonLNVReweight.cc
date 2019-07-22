// ---------------------------------------------------------------
// History:
//
// Created by Nicolas Lurkin (nicolas.lurkin@cern.ch) 2018-07-25
// ---------------------------------------------------------------

/// \class DiLeptonLNVReweight
/// \Brief
/// Compute weights to change the phase space of K+->pi-l+l+ (l=e,mu) events
/// \EndBrief
/// \Detailed
/// This tool computes the weights to be applied to K+->pi-l+l+ (l=e,mu) events
/// generated with flat phase space, corresponding to specific models.
/// The two models implemented are:
///   - LNV carried by PASCOLI MODEL
///     http://inspirehep.net/record/811669
///     Assumption: immediately subsequent N decay
///         mediator particle has mass [GeV] and decay width [GeV] that can be defined
///   - LNV carried by VECTOR MEDIATOR
///     https://arxiv.org/abs/1011.4817
///     Assumption: A = 1, B = 0 (form factors), no loop contribution<br>
/// The code has been ported from FORTRAN code that was implemented in the
/// NA48 decay Generator.<br>
/// All units in the code should be in GeV and as standard units in NA62 are
/// MeV, the input values are assumed to be in MeV and are automatically
/// transformed in GeV. If the vectors used in input are already in GeV,
/// the 'inMeV' flag should be set to false (default is true if the flag is
/// omitted).<br>
/// The code can also provide the global scaling to apply to recover an integral
/// equal to the number of generated events.
/// Example usage:
/// \code
/// #include "DiLeptonLNVReweight.hh"
/// ...
/// DiLeptonLNVReweight* dlw = DiLeptonLNVReweight::GetInstance();
/// dlw->Pascoli.SetMediatorMass(300); // [MeV]
/// dlw->Pascoli.SetMediatorWidth(1);  // [MeV]
/// double weight = dlw->GetWeight_PascoliAmplitude(GetMCEvent());
/// ...
/// // or
/// dlw->Vector.SetFormFactorA(-0.587);
/// dlw->Vector.SetFormFactorB(-0.655);
/// double weight = dlw->GetWeight_VectorAmplitude(GetMCEvent());
/// ...
/// void EndofJobUser(){
///    std::cout << "Global scaling: " << DiLeptonLNVReweight::GetInstance()->GetGlobalNormalization() << std::endl;
/// }
/// \endcode
/// \EndDetailed

#include "DiLeptonLNVReweight.hh"
#include "NA62Constants.hh"
#include "CoulombCorrection.hh"
using namespace NA62Constants;

DiLeptonLNVReweight* DiLeptonLNVReweight::fInstance = nullptr;
const double DiLeptonLNVReweight::fSqRatio_el = SQMEL/SQMKCH;
const double DiLeptonLNVReweight::fSqRatio_mu = SQMMU/SQMKCH;
const double DiLeptonLNVReweight::fSqRatio_pi = SQMPI/SQMKCH;
const double DiLeptonLNVReweight::fGFERMI     = 1.16639e-5;
const double DiLeptonLNVReweight::fALPHA      = 0.007297352568;

// Redefine all masses locally in GeV (same formula as in generator)
const double DiLeptonLNVReweight::fMKCH   = MKCH  *1e-3;
const double DiLeptonLNVReweight::fSQMKCH = SQMKCH*1e-6;
const double DiLeptonLNVReweight::fSQMPI  = SQMPI *1e-6;

DiLeptonLNVReweight* DiLeptonLNVReweight::GetInstance(){
  if (fInstance==nullptr) fInstance = new DiLeptonLNVReweight();
  return fInstance;
}

// cppcheck-suppress uninitMemberVarPrivate
DiLeptonLNVReweight::DiLeptonLNVReweight(){
  Pascoli.fMediatorMass  = 0.3;  //In GeV
  Pascoli.fMediatorGamma = 1e-3; //In GeV
  Vector.fFormFactorA = 1.0;
  Vector.fFormFactorB = 0.0;
  Vector.fLoopFactor  = 0.0;
  fMode = kEEMODE;
  fSumEvents = 0.;
  fSumWeights = 0.;
}

DiLeptonLNVReweight::~DiLeptonLNVReweight() {}

double DiLeptonLNVReweight::GetWeight_VectorAmplitude(Event* mcevt) {
  if (mcevt->GetNGeneParts()<3) return 0.0;

  if      (mcevt->GetGenePart(0)->GetPDGcode()==-211 &&
	   mcevt->GetGenePart(1)->GetPDGcode()== -11 &&
	   mcevt->GetGenePart(2)->GetPDGcode()== -11) fMode = kEEMODE;
  else if (mcevt->GetGenePart(0)->GetPDGcode()==-211 &&
	   mcevt->GetGenePart(1)->GetPDGcode()== -13 &&
	   mcevt->GetGenePart(2)->GetPDGcode()== -13) fMode = kMUMUMODE;
  else return 0.0;

  return GetWeight_VectorAmplitude
    (mcevt->GetGenePart(0)->GetInitial4Momentum(),
     mcevt->GetGenePart(1)->GetInitial4Momentum(),
     mcevt->GetGenePart(2)->GetInitial4Momentum());
}

double DiLeptonLNVReweight::GetWeight_VectorAmplitude
(TLorentzVector pi, TLorentzVector l1, TLorentzVector l2, bool inMeV) {
  if (inMeV) { // Needs to be transformed in GeV
    pi = pi*1e-3;
    l1 = l1*1e-3;
    l2 = l2*1e-3;
  }
  return GetWeight_VectorAmplitude
    ((pi+l1).Mag2()/DiLeptonLNVReweight::fSQMKCH, (l1+l2).Mag2()/DiLeptonLNVReweight::fSQMKCH);
}

/*==========================================================*/
/* Generator of decays K+- -> PI-+ L+- L+- (L=e,mu)         */
/* LNV carried by a VECTOR MEDIATOR                         */
/* Assumption: A = 1, B = 0, no loop contribution           */
/*==========================================================*/
double DiLeptonLNVReweight::GetWeight_VectorAmplitude(double m2_pi_l1, double m2_l1_l2){
  //preparing parameters
  double sqratio_lept = -1;
  if      (fMode==kEEMODE)   sqratio_lept = fSqRatio_el;
  else if (fMode==kMUMUMODE) sqratio_lept = fSqRatio_mu;
  else    return 0.0;

  // ChPT parameters
  double aa=Vector.fFormFactorA;
  double bb=Vector.fFormFactorB;

  // Kinematic variables
  double xx = m2_pi_l1;///SQMKCH;
  double zz = m2_l1_l2;///SQMKCH;

  // Compute squared form-factor
  double zr   = zz/fSqRatio_pi;
  double sq   = sqrt(fabs(1.-4./zr));
  double wpol = 1e6*fGFERMI*fSQMKCH*(aa + bb*zz);
  double gre, gim;
  if(zr <= 4.0){
    gre = sq*asin(0.5*sqrt(zr));
    gim = 0.0;
  }
  else {
    gre = -0.5*sq*log((1.-sq)/(1.+sq));
    gim = -0.5*sq*3.14159265359;
  }
  double xre     = 4./9.-4.*fSqRatio_pi/3./zz-(1.-4./zr)*gre/3.;
  double xim     = -(1.-4./zr)*gim/3.;
  double factor  = Vector.fLoopFactor;
  double wloopre = 1e6*factor*xre;
  double wloopim = 1e6*factor*xim;
  double wabs2   = pow(wloopre+wpol, 2)+wloopim*wloopim;

  // Squared amplitude
  double a2 = (2*xx+zz-2*fSqRatio_pi-2*sqratio_lept)*(2+2*sqratio_lept-2*xx-zz)+zz*(zz-2-2*fSqRatio_pi);
  a2 = a2*wabs2;

  fSumWeights += a2;
  fSumEvents += 1.;
  return a2;
}

double DiLeptonLNVReweight::GetWeight_PascoliAmplitude(Event* mcevt){
  if (mcevt->GetNGeneParts()<3) return 0.0;

  if      (mcevt->GetGenePart(0)->GetPDGcode()==-211 &&
	   mcevt->GetGenePart(1)->GetPDGcode()== -11 &&
	   mcevt->GetGenePart(2)->GetPDGcode()== -11) fMode = kEEMODE;
  else if (mcevt->GetGenePart(0)->GetPDGcode()==-211 &&
	   mcevt->GetGenePart(1)->GetPDGcode()== -13 &&
	   mcevt->GetGenePart(2)->GetPDGcode()== -13) fMode = kMUMUMODE;
  else return 0.0;

  return GetWeight_PascoliAmplitude
    (mcevt->GetGenePart(0)->GetInitial4Momentum(),
     mcevt->GetGenePart(1)->GetInitial4Momentum(),
     mcevt->GetGenePart(2)->GetInitial4Momentum());
}

/*==========================================================*/
/* Generator of decays K+- -> PI-+ L+- L+- (L=e,mu)         */
/* LNV carried by a PASCOLI MODEL                           */
/* Assumption: immediately subsequent N decay               */
/*             m_N = NEWMASS, gamma = NEWGAMMA from DATACARD*/
/*==========================================================*/
double DiLeptonLNVReweight::GetWeight_PascoliAmplitude
(TLorentzVector pi, TLorentzVector l1, TLorentzVector l2, bool inMeV) {
  if(inMeV){
    l1 = l1*1e-3;
    l2 = l2*1e-3;
    pi = pi*1e-3;
  }

  TLorentzVector q1(0.0, 0.0, 0.0, DiLeptonLNVReweight::fMKCH);
  // Prepare parameters
  double sqmed_mass2   = Pascoli.fMediatorMass*Pascoli.fMediatorMass;
  double bx            = Pascoli.fMediatorMass*Pascoli.fMediatorGamma;

  //Kinematic variables
  double l1l2 = l1*l2;
  double l1pi = l1*pi;
  double l2pi = l2*pi;
  double a1   = fSQMKCH*(1+fSqRatio_el) - 2*fMKCH*l1.E() - sqmed_mass2;
  double a2   = fSQMKCH*(1+fSqRatio_el) - 2*fMKCH*l2.E() - sqmed_mass2;
  double a12  = a1*a1;
  double a22  = a2*a2;
  double bx2  = bx*bx;
  double AP = 8*(l1.E()*fMKCH)*l2pi*(fMKCH*pi.E())
    - 4*fSQMKCH*l1pi*l2pi
    - 4*fSQMPI*(l1.E()*fMKCH)*(l2.E()*fMKCH)
    + 2*fSQMKCH*fSQMPI*l1l2;
  double BP = 8*(l2.E()*fMKCH)*l1pi*(fMKCH*pi.E())
    - 4*fSQMKCH*l2pi*l1pi
    - 4*fSQMPI*(l2.E()*fMKCH)*(l1.E()*fMKCH)
    + 2*fSQMKCH*fSQMPI*l1l2;
  double CP = 4*l1l2*pow(fMKCH*pi.E(), 2) - AP;
  double DP = 4*l1l2*pow(fMKCH*pi.E(), 2) - BP;

  //Calculate the amplitude terms
  double F11 = ((a22+bx2)*AP+(a1*a2+bx2)*CP)/((a22+bx2)*AP+(a12+bx2)*BP);
  double F12 = ((a12+bx2)*BP+(a2*a1+bx2)*DP)/((a12+bx2)*BP+(a22+bx2)*AP);
  double F1 = (AP/(a12+bx2))*(F11+F12);

  //squared amplitude
  double amp2 = bx2*F1*1.E3;

  fSumWeights += amp2;
  fSumEvents += 1.;
  return amp2;
}
