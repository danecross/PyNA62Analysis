////////////////////////////////////////////////////////////////////////////
// pi0 -> e+ e- (gamma) decay generator
// NA62 implementation: Michal Koval (michal.koval@cern.ch) 01/12/2016
//
// Routine generate_pi0ee is called from pi0decays.F,
// decay options (macro setting /decay/pizeroDecay):
//
// 21: pi0-->e+e-    (Two body decay, no radiative corrections).
// 22: pi0-->e+e-(g) (Generator with an extra radiative photon).
//     - default value for the generator cutoff: x0 = 0.98,
//       radiative photons are generated only for x=(Mee/Mpi0)^2 < x0,
//       while events with x>x0 are approximated by two body decays (x=1).
//     - Average CPU time needed for the default option ~ 1.5s / event
// any other value will mean that pi0 will be passed to Geant4
//
/////////////////////////////////////////////////////////////////////////////

#include "pi0ee.hh"
#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "../include/CMC.hh"

#include <iostream>
using namespace std;

// ROOT
#include "TLorentzVector.h"
#include "TGenPhaseSpace.h"
#include "TMath.h"

void generate_pi0ee_(double ppi0[4], int *pzmode)
{
    if (CMC::GetInstance()->GetVerbose() >= 1)
        cout << "Generate pi0->ee decay selected mode = " << *pzmode << endl;

    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

    TLorentzVector pi0mom(ppi0[0], ppi0[1], ppi0[2], ppi0[3]);

    TLorentzVector epmom, emmom, gamom;
    bool radphoton = false;
    if (*pzmode == 21) {
        generate_pi0ee_2body(epmom, emmom);
    } else if (*pzmode == 22) {
        // probability of a 3-body (radiative) decay computed only once per NA62MC run
        static double prob_3body = get_3body_event_probability(xcutPi0ee);
        radphoton = (RandomDecay->Uniform() < prob_3body);
        if (radphoton) {
            generate_pi0ee_3body(epmom, emmom, gamom);
        } else {
            generate_pi0ee_2body(epmom, emmom);
        }
    }
    dboost(pi0mom, MP0, epmom);
    dboost(pi0mom, MP0, emmom);
    if (radphoton) dboost(pi0mom, MP0, gamom);

    mcadd4cpp(PDG_ID_elp, epmom);
    mcadd4cpp(PDG_ID_elm, emmom);
    if (radphoton) mcadd4cpp(PDG_ID_gam, gamom);
    return;
}

// The following values for decay widths were provided by Tomas Husek
double get_3body_event_probability(double xcutoff)
{
    if (CMC::GetInstance()->GetVerbose() >= 1)
        cout << "Function get_3body_event_probability, selected x cut-off = " << xcutoff << endl;

    double gamma2body = 0, gamma3body = 0;
    if (xcutoff == 0.95) {
        gamma2body = 2.520;
        gamma3body = 0.4488;
    } else if (xcutoff == 0.98) {
        gamma2body = 2.400;
        gamma3body = 0.5614;
    } else if (xcutoff == 0.99) {
        gamma2body = 2.311;
        gamma3body = 0.6475;
    } else if (xcutoff == 0.995) {
        gamma2body = 2.224;
        gamma3body = 0.7340;
    } else {
        cout << "WARNING: unknown value for the x cut-off; disable radiative pi0->eeg decay" << endl;
        return 0;
    }
    return gamma3body / (gamma3body + gamma2body);
}

void generate_pi0ee_2body(TLorentzVector &epmom, TLorentzVector &emmom)
{
    if (CMC::GetInstance()->GetVerbose() >= 1)
        cout << "Generate 2 body pi0->ee decay" << endl;
    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
    double E = 0.5*MP0;
    double P = sqrt(E*E - MEL*MEL);
    double px = 0, py = 0, pz = 0;
    RandomDecay->Sphere(px, py, pz, P);
    epmom.SetPxPyPzE(px, py, pz, E);
    emmom.SetPxPyPzE(-px, -py, -pz, E);
    if (CMC::GetInstance()->GetVerbose() >= 2) {
        cout << "Momenta in pi0 rest frame: " << endl;
        cout << " e+ "; epmom.Print();
        cout << " e- "; emmom.Print();
    }
    mcadd4gencpp(PDG_ID_elp, epmom, 1);
    mcadd4gencpp(PDG_ID_elm, emmom, 1);
}

void generate_pi0ee_3body(TLorentzVector &epmom, TLorentzVector &emmom, TLorentzVector &gamom)
{
    if (CMC::GetInstance()->GetVerbose() >= 1)
        cout << "Generate radiative 3 body pi0->eeg decay" << endl;

    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

    // following variables are defined static, as they need to be initialized only once
    static TLorentzVector ppion(0.0, 0.0, 0.0, MP0); // pi0 at rest
    static double masses[3] = {MEL, MEL, 0}; // e+, e-, g
    static TGenPhaseSpace pi0ee3bodyEvent;
    static int gen_phase_space_calls = 0;

    if (!gen_phase_space_calls) {
        pi0ee3bodyEvent.SetDecay(ppion, 3, masses);
        gen_phase_space_calls++;
    }

    TLorentzVector *pgm, *pep, *pem;
    double weight;              // pi0ee3bodyEvent weight
    double M32 = 0;
    static double M32max = get_maximum_M2_value_3body(xcutPi0ee);
    do {
        // generate momenta
        weight = pi0ee3bodyEvent.Generate();
        pep = pi0ee3bodyEvent.GetDecay(0);
        pem = pi0ee3bodyEvent.GetDecay(1);
        pgm = pi0ee3bodyEvent.GetDecay(2);

        double x = (*pep + *pem).M2() / SQMP0;
        double y = TMath::Abs(2 * ppion * (*pep - *pem) / (SQMP0 * (1-x)));
        if (x > xcutPi0ee || TMath::IsNaN(x) != 0 || TMath::IsNaN(y) != 0) {
            weight = 0;
            continue;
        }
        M32 = M2_piee_brems(x, y);
        if (M32 > M32max) {
            if (CMC::GetInstance()->GetVerbose() >= 1)
                cout << "WARNING: M32 > M32max in pi0ee.cc; M32 =  " << M32 << endl;
        }
        if (weight > wMaxPi0eeg) {
            if (CMC::GetInstance()->GetVerbose() >= 1)
                cout << "WARNING: weight > wMaxPi0eeg in pi0ee.cc; M32 =  " << weight << endl;
        }
        if (fabs(y) > sqrt(1 - v2 / x)) {
            if (CMC::GetInstance()->GetVerbose() >= 1)
                cout << "WARNING: kinematic bounds not satisfied in pi0ee.cc;"
                     << "x = " << x << " y = " << y << endl;
            weight = 0;
            continue;
        }

    } while ((M32max * wMaxPi0eeg * RandomDecay->Uniform()) > (M32 * weight)); // accept-reject

    epmom = *pep;
    emmom = *pem;
    gamom = *pgm;
    if (CMC::GetInstance()->GetVerbose() >= 2) {
        cout << "Momenta in pi0 rest frame " << endl;
        cout << " e+ "; epmom.Print();
        cout << " e- "; emmom.Print();
        cout << " g  "; gamom.Print();
    }
    mcadd4gencpp(PDG_ID_elp, epmom, 3);
    mcadd4gencpp(PDG_ID_elm, emmom, 3);
    mcadd4gencpp(PDG_ID_gam, gamom, 3);
}

double M2_piee_brems(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV
    static double normalization_constant =
        pow(AlphaQED,5) / pow(Fpi0,2)* pow(mpi0,4) * (16 * TMath::Pi()) / 8 / pow(16 * pow(TMath::Pi(),2),2);
    double M2 = pow((1-x),2) * (pow(mpi0,2) * (x * (1 - pow(y,2)) - v2) * (
                                    x * pow(mpi0,2) * (pow(ImP(x, y),2) + pow(ReP(x, y),2))
                                    + 2 * sqrt(v2) * mpi0 * (ImP(x, y)*(ImA(x, -y) + ImA(x, y)) + ReP(x, y)*(ReA(x, -y) + ReA(x, y)))
                                    - 4 * (ImP(x, y) * ImT(x, y) + ReP(x, y) * ReT(x, y)))
                                + 2 * pow(mpi0,2) * (x - v2) * (pow(1 + y,2) * (pow(ImA(x, -y),2) + pow(ReA(x, -y),2))
                                                              + pow(1 - y,2) * (pow(ImA(x,  y),2) + pow(ReA(x,  y),2)))
                                - 8 * sqrt(v2) * mpi0 * y * (-((1 + y)*(ImT(x, y) * ImA(x, -y) + ReT(x, y) * ReA(x, -y)))
                                                             + (1 - y)*(ImT(x, y) * ImA(x,  y) + ReT(x, y) * ReA(x,  y)))
                                - 4 * v2 * pow(mpi0,2) * pow(y,2) * (ImA(x, -y) * ImA(x, y) + ReA(x, -y) * ReA(x, y))
                                + 8 * (1 - pow(y,2)) * (pow(ImT(x, y),2) + pow(ReT(x, y),2)));
    if (CMC::GetInstance()->GetVerbose() >= 3) {
        cout << " pi0->eeg: "<< "x = " << x << " y = " << y << endl
             << " Cnorm = " << normalization_constant << " |M(x,y)|^2 = " << M2 << endl;
    }
    return M2; // the normalization constant is not important for the shape sampling
}

// The following maximum of the matrix elements were obtained experimentally by M. Koval
double get_maximum_M2_value_3body(double xcutoff)
{
    if (CMC::GetInstance()->GetVerbose() >= 3)
        cout << "Function get_maximum_M2_value_3body x cut-off = " << xcutoff << endl;

    if (xcutoff == 0.95) {
        return 1593.;
    } else if (xcutoff == 0.98) {
        return 5946.;
    } else if (xcutoff == 0.99) {
        return 37498.;
    } else if (xcutoff == 0.995) {
        return 95079.;
    } else {
        cout << "WARNING: unknown x cut-off value; set M32max to 999999" << endl;
        return 999999;
    }
}

// redefine dilog macro from T.Husek to use ROOT DiLog function
inline double dilog(double z)
{
    return TMath::DiLog(z);
}

// redefine sign macro from T.Husek as a function
inline int sign(double z)
{
    return ((z > 0) ? 1 : ((z < 0) ? -1 : 0));
}

double ReP(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return (48*sqrt(v2)*(1 - (2*chi)/3.))/(pow(mpi0,3)*pow(-1 + x,2)*(-1 + pow(y,2))) +
   (16*sqrt(v2)*(2 - 3*log((pow(mpi0,2)*v2)/(4.*pow(mu,2)))))/(pow(mpi0,3)*pow(-1 + x,2)*(-1 + pow(y,2))) +
   (16*x*(-1 + y)*(pow(TMath::Pi(),2)/6. - pow(log((2 + 2*x*(-1 + y) - 2*y + v2)/v2),2)/2. -
        dilog(v2/(2 + 2*x*(-1 + y) - 2*y + v2))))/
    (pow(mpi0,3)*(-1 + x)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) -
   (16*x*(1 + y)*(pow(TMath::Pi(),2)/6. - pow(log((2 + 2*y - 2*x*(1 + y) + v2)/v2),2)/2. - dilog(v2/(2 + 2*y - 2*x*(1 + y) + v2))))/
    (pow(mpi0,3)*(-1 + x)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   (8*(x + pow(x,2)*pow(-1 + y,2) - x*pow(y,2) - 2*v2)*
      (-2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
            (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
              (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))))/
    (pow(mpi0,3)*pow(-1 + x,2)*(-1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) +
   ((-16*x*(1 + y)*(1 + x + (-1 + x)*y) + 32*v2)*(-2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
            (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
              (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))))/
    (2.*pow(mpi0,3)*pow(-1 + x,2)*(1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) +
   (2*(32*(-1 + x)*x*(-1 + pow(y,2)) - 64*v2)*(log((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
             (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))/4.)*
         log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
               sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                 (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) +
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) -
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) -
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) +
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.)))/
    (pow(mpi0,3)*(-1 + x)*pow(sqrt(v2),3)*(x*(-1 + pow(y,2)) + v2)*
      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))));
}

double ImP(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return TMath::Pi()*((16*x*(-1 + y)*log((2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
    (pow(mpi0,3)*(-1 + x)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) -
   (16*x*(1 + y)*log((2 + 2*y - 2*x*(1 + y) + v2)/v2))/
    (pow(mpi0,3)*(-1 + x)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   (16*(x + pow(x,2)*pow(-1 + y,2) - x*pow(y,2) - 2*v2)*
      log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))/
    (pow(mpi0,3)*pow(-1 + x,2)*(-1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) +
   ((-16*x*(1 + y)*(1 + x + (-1 + x)*y) + 32*v2)*log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))/
    (pow(mpi0,3)*pow(-1 + x,2)*(1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) -
   (2*(32*(-1 + x)*x*(-1 + pow(y,2)) - 64*v2)*log((v2*
          (8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
            sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
              (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.))/
    (pow(mpi0,3)*(-1 + x)*pow(sqrt(v2),3)*(x*(-1 + pow(y,2)) + v2)*
      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))));
}

double ReA(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return (4*(-1 + x + y - x*y + v2))/(pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)) +
   (4*v2*(3*(-1 + x)*(-1 + y) + v2))/(pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)*(2*(-1 + x)*(-1 + y) + v2)) +
   ((-12*(-1 + x)*(-1 + y)*v2 - 4*pow(v2,2))*(2 +
        (-1 + v2/(2 + 2*x*(-1 + y) - 2*y + v2))*log(fabs(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))))/
    (pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)*(2*(-1 + x)*(-1 + y) + v2)) -
   ((4*pow(-1 + x,2)*pow(-1 + y,3)*(1 + y) - 8*(-1 + y)*(-1 + 2*x + y)*v2 - 8*pow(v2,2))*
      (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*x*(-1 + y) - 2*y + v2)/v2),2)/2. - dilog(v2/(2 + 2*x*(-1 + y) - 2*y + v2))))/
    (pow(mpi0,2)*(-1 + x)*(-1 + y)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) +
   (4*(1 + y)*((-1 + x)*(-1 + pow(y,2)) + 2*v2)*(pow(TMath::Pi(),2)/6. - pow(log((2 + 2*y - 2*x*(1 + y) + v2)/v2),2)/2. -
        dilog(v2/(2 + 2*y - 2*x*(1 + y) + v2))))/
    (pow(mpi0,2)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   ((-4*(1 + y) + 4*(1 + y)*pow(x + y - x*y,2) - 16*y*v2)*
      (-2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
            (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
              (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))))/
    (2.*pow(mpi0,2)*(-1 + x)*v2*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) +
   ((-4*(-1 + x)*(1 + x + (-1 + x)*y)*(-1 + pow(y,2)) + 16*y*v2)*
      (-2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
            (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
              (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))))/
    (2.*pow(mpi0,2)*(-1 + x)*v2*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) +
   (16*(-1 + pow(y,2))*(pow(-1 + x,2)*(-1 + pow(y,2)) - 4*v2)*
      (log((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))/4.)*
         log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
               sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                 (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) +
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) -
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) -
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) +
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.)))/
    (pow(mpi0,2)*pow(v2,2)*(x*(-1 + pow(y,2)) + v2)*sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
        (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*(16/v2 +
                                                (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))));
}

double ImA(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return TMath::Pi()*(-(((4*pow(-1 + x,2)*pow(-1 + y,3)*(1 + y) - 8*(-1 + y)*(-1 + 2*x + y)*v2 - 8*pow(v2,2))*log((2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
      (pow(mpi0,2)*(-1 + x)*(-1 + y)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))) +
   (4*(1 + y)*((-1 + x)*(-1 + pow(y,2)) + 2*v2)*log((2 + 2*y - 2*x*(1 + y) + v2)/v2))/
    (pow(mpi0,2)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   ((-4*(1 + y) + 4*(1 + y)*pow(x + y - x*y,2) - 16*y*v2)*
      log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))/
    (pow(mpi0,2)*(-1 + x)*v2*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) +
   ((-4*(-1 + x)*(1 + x + (-1 + x)*y)*(-1 + pow(y,2)) + 16*y*v2)*
      log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))/
    (pow(mpi0,2)*(-1 + x)*v2*(x*(-1 + pow(y,2)) + v2)*
      sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) -
   (16*(-1 + pow(y,2))*(pow(-1 + x,2)*(-1 + pow(y,2)) - 4*v2)*
      log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
            sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
              (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.))/
    (pow(mpi0,2)*pow(v2,2)*(x*(-1 + pow(y,2)) + v2)*sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
        (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*(16/v2 +
          (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))) +
   ((-12*(-1 + x)*(-1 + y)*v2 - 4*pow(v2,2))*(1 - v2/(2 + 2*x*(-1 + y) - 2*y + v2))*
    TMath::Max(0,sign(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)))/(pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)*(2*(-1 + x)*(-1 + y) + v2)));
}

double ReT(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return (2*sqrt(v2)*(6/((-1 + x)*(-1 + pow(y,2))) + 1/(2*(-1 + x)*(1 + y) - v2) - 1/(2*(-1 + x)*(-1 + y) + v2))*(1 - (2*chi)/3.))/mpi0 +
   (4*sqrt(v2)*(2 - 3*log((pow(mpi0,2)*v2)/(4.*pow(mu,2)))))/(mpi0*(-1 + x)*(-1 + pow(y,2))) +
   (2*sqrt(v2)*(2 - (2*chi)/3. + (-1 + v2/(2 + 2*x*(-1 + y) - 2*y + v2))*log(fabs(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))))/
    (mpi0*(2*(-1 + x)*(-1 + y) + v2)) + (2*sqrt(v2)*(2 - (2*chi)/3. +
        (-1 + v2/(2 + 2*y - 2*x*(1 + y) + v2))*log(fabs(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))))/
    (mpi0*(-2*(-1 + x)*(1 + y) + v2)) - (4*(-1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*
      (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*x*(-1 + y) - 2*y + v2)/v2),2)/2. - dilog(v2/(2 + 2*x*(-1 + y) - 2*y + v2))))/
    (mpi0*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) +
   (4*(1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*(pow(TMath::Pi(),2)/6. - pow(log((2 + 2*y - 2*x*(1 + y) + v2)/v2),2)/2. -
        dilog(v2/(2 + 2*y - 2*x*(1 + y) + v2))))/
    (mpi0*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   ((-8*v2 + 4*(-1 + y)*(pow(x,2)*(-1 + y)*pow(y,2) - 2*x*(1 - y + pow(y,3)) + y*(y + pow(y,2) - 4*v2)))*
      (-2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
            (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
              (1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))))/
    (2.*mpi0*(-1 + x)*(-1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*sqrt(-16/v2 +
        pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) +
   ((-4*(1 + y)*(2*x + 2*x*y + (-1 + pow(x,2))*pow(y,2) + pow(-1 + x,2)*pow(y,3)) + 8*(1 + 2*y*(1 + y))*v2)*
      (-2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
         log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
            (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
              sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))*
         (2*log(4/v2) - 2*log(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
           log(-1 + (2*(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
              (1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
                sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))))) -
        2*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) +
        4*dilog((2*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))) -
        2*dilog((4*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
             sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
           ((-1 - 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))*
             (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
               sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))))/
    (2.*mpi0*(-1 + x)*(1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))
     + (2*(8*(-1 + pow(y,2))*(2*x + pow(-1 + x,2)*pow(y,2)) + 16*(1 - 2*pow(y,2))*v2)*
      (log((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))/4.)*
         log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
               sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                 (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) +
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) -
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) -
        dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
              ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v2,2) +
              sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.)))/
    (mpi0*pow(sqrt(v2),3)*(x*(-1 + pow(y,2)) + v2)*sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
        (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*(16/v2 +
                                                (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))));
}

double ImT(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return TMath::Pi()*((-4*(-1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*log((2 + 2*x*(-1 + y) - 2*y + v2)/v2))/
    (mpi0*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) +
   (4*(1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*log((2 + 2*y - 2*x*(1 + y) + v2)/v2))/
    (mpi0*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) +
   ((-8*v2 + 4*(-1 + y)*(pow(x,2)*(-1 + y)*pow(y,2) - 2*x*(1 - y + pow(y,3)) + y*(y + pow(y,2) - 4*v2)))*
      log((-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))))/
    (mpi0*(-1 + x)*(-1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2)))\
    + ((-4*(1 + y)*(2*x + 2*x*y + (-1 + pow(x,2))*pow(y,2) + pow(-1 + x,2)*pow(y,3)) + 8*(1 + 2*y*(1 + y))*v2)*
      log((-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))/
        (-1 + 4/v2 + (2 + 2*y - 2*x*(1 + y) + v2)/v2 +
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2)))))/
    (mpi0*(-1 + x)*(1 + y)*sqrt(v2)*(x*(-1 + pow(y,2)) + v2)*sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) -
   (2*(8*(-1 + pow(y,2))*(2*x + pow(-1 + x,2)*pow(y,2)) + 16*(1 - 2*pow(y,2))*v2)*
      log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
            sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
              (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.))/
    (mpi0*pow(sqrt(v2),3)*(x*(-1 + pow(y,2)) + v2)*sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
        (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*(16/v2 +
          (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))) +
   (2*sqrt(v2)*(1 - v2/(2 + 2*x*(-1 + y) - 2*y + v2))*TMath::Max(0,sign(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)))/
    (mpi0*(2*(-1 + x)*(-1 + y) + v2)) + (2*sqrt(v2)*(1 - v2/(2 + 2*y - 2*x*(1 + y) + v2))*
                                         TMath::Max(0,sign(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))/(mpi0*(-2*(-1 + x)*(1 + y) + v2)));
}
