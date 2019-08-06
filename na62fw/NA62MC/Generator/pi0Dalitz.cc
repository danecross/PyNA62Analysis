////////////////////////////////////////////////////////////////////////////
// Pi0 Dalitz decay generator: pi0 -> e+ e- gamma
// NA62 version: Michal Koval (michal.koval@cern.ch) 02/10/2015
// Updated version: Michal Koval 05/11/2015
//
// Routine generate_pi0dal_ is called from pi0decays.F,
// decay options (macro setting /decay/pizeroDecay):
//
// 11: pi0-->ge+e-    (Dalitz decay, no radiative corrections);
// 12: pi0-->ge+e-    (Dalitz decay, Mikaelian-Smith + one photon irreducible corrections);
// 13: pi0-->ge+e-(g) (Dalitz radiative decay, full radiative corrections);
// 14: pi0-->ge+e-g   (radiative tail of "4-body" Dalitz decay with radiative photon),
// 15: pi0-->ge+e-    (Dalitz decay with x>0.5, Mikaelian-Smith + one photon irreducible corrections);
// 16: pi0-->ge+e-(g) (Dalitz radiative decay with x>0.5, full radiative corrections);
// see http://mkoval.web.cern.ch/mkoval/na62/pi0/pdf/report.pdf
//
// Any other value will mean that pi0 will be passed to Geant4.
//
// Modes 13,14 use a minimal cut-off on the invariant mass of the photon pair
// One can modify the variable zgammaCut in pi0Dalitz.hh in order to change the cut-off value
// When the cut-off is lowered, one should increase the value of the M42max (max 4-body event weight)
// Default values of the cut-off and M42max have been optimized in order to generate correct
// spectrum of Dalitz x variable above x>0.01.
//

// LXPLUS SLC6 average CPU time consumption per event:
// 11: ~0.0005 s/evt
// 12: ~0.04   s/evt
// 13: ~0.3    s/evt
// 14: ~6      s/evt
//
// Radiative corrections by Tomas Husek, see license in pi0Dalitz.hh
// T.Husek et al., Phys Rev D92 (2015) 054027,
// DOI: 10.1103/PhysRevD.92.054027, arXiv:1504.06178
/////////////////////////////////////////////////////////////////////////////

#include "pi0Dalitz.hh"
#include "rndmcpp.hh"
#include "mcadd4cpp.hh"

#include <iostream>
using namespace std;

// package for numerical integration
#include "GaussLegendre.hh"

// ROOT
#include "TLorentzVector.h"
#include "TGenPhaseSpace.h"
#include "TMath.h"

// redefine dilog function from T.Husek to use ROOT one
inline double dilog(double z) {
  return TMath::DiLog(z);
}

// this routine is called from pi0decays.F
void generate_pi0dal_(double ppi0 [], int * pzmode)
{
    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
    TLorentzVector pi0mom(ppi0[0], ppi0[1], ppi0[2], ppi0[3]);

    double xmincutoff = v2;
    if (*pzmode == 15 || *pzmode == 16) { // special modes, lower cutoff on the x variable
        xmincutoff = 0.5;
    }

    if (*pzmode == 11 || *pzmode == 12 || *pzmode == 15) { // 3 body decays
        bool radcor = (*pzmode == 12 || *pzmode == 15);
        double zcut = 1.0;      // no extra photons
        TLorentzVector gamom, epmom, emmom;
        generate_dalitz_3body(gamom, epmom, emmom, radcor, zcut, xmincutoff);

        dboost(pi0mom, MP0, gamom);
        dboost(pi0mom, MP0, epmom);
        dboost(pi0mom, MP0, emmom);

        mcadd4cpp(PDG_ID_gam, gamom);
        mcadd4cpp(PDG_ID_elp, epmom);
        mcadd4cpp(PDG_ID_elm, emmom);

    } else if (*pzmode == 13 || *pzmode == 14 || *pzmode == 16) { // 3-4 body decays

        TLorentzVector  gamom1,  gamom2, epmom, emmom;
        bool radphoton = false;

        if (*pzmode == 14) {    // 14 = special mode: only decays with rad photon
            radphoton = true;
        } else if (*pzmode == 13 || *pzmode == 16) { // 13 = standard Dalitz decay with full rad. corr., 16 = low cutoff
            double r  = ratio_34body(zgammaCut);
            double w4 = 1 / (r+1);
            radphoton = (RandomDecay->Uniform() < w4);
        } else {
            return;
        }

        if (radphoton) {
            generate_dalitz_4body(gamom1, gamom2, epmom, emmom, zgammaCut, xmincutoff);
        } else {
            bool radcor = true;
            generate_dalitz_3body(gamom1, epmom, emmom, radcor, zgammaCut, xmincutoff);
        }

        dboost(pi0mom, MP0, gamom1);
        dboost(pi0mom, MP0, epmom);
        dboost(pi0mom, MP0, emmom);

        if (radphoton) dboost(pi0mom, MP0, gamom2);

	mcadd4cpp(PDG_ID_gam, gamom1);
	mcadd4cpp(PDG_ID_elp, epmom);
	mcadd4cpp(PDG_ID_elm, emmom);
	if (radphoton) mcadd4cpp(PDG_ID_gam, gamom2);
    }
    return;
}

// generate 4-momenta of the gamma, e+, e-
void generate_dalitz_3body(TLorentzVector &gamom,
                           TLorentzVector &epmom,
                           TLorentzVector &emmom,
                           bool radcor, double zcut, double xmincutoff)
{
    double xmin = v2;
    double xmax = 1.;
    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

    TLorentzVector pee;          // e+e- 4 momentum in Pi0 rest frame
    double y, fy;
    bool accept = false;
    do {
        double t = log(xmincutoff) + RandomDecay->Uniform()*(log(xmax) - log(xmincutoff));
        double x = exp(t);
        double fx = pow((1.0 - x), 3)*(1.0 + 0.5*xmin/x) * sqrt(1.0 - xmin/x);
        fx *= pow((1.0 + aslopeVMD*x), 2);     // multiply by form factor
        if (RandomDecay->Uniform() > fx) continue; // accept/reject x-spectrum

        double mee = sqrt(x)*MP0;
        double eg = (SQMP0 - mee*mee) / (2*MP0); // gamma energy
        double gx, gy, gz;
        RandomDecay->Sphere(gx, gy, gz, eg); // random photon direction in pi0 rest frame
        gamom.SetPxPyPzE(gx, gy, gz, eg);

        double ex = MP0 - eg;
        pee.SetPxPyPzE(-gx, -gy, -gz, ex);
        do {
            double eE = mee/2;
            double pE = sqrt(eE*eE - MEL*MEL);
            double px, py, pz;
            RandomDecay->Sphere(px, py, pz, pE);
            epmom.SetPxPyPzE( px,  py,  pz, eE);
            emmom.SetPxPyPzE(-px, -py, -pz, eE);

            // boost e+- to pi0 rest frame
            dboost(pee, mee, epmom);
            dboost(pee, mee, emmom);

            y = 2.0*abs((epmom.E() - emmom.E()) / (1.0 - x)) / MP0;
            fy = (1. + y*y + xmin / x)/3.;
        } while (RandomDecay->Uniform() > fy);

        if (radcor) {
            double rc_weight = (1 + radcorr_full(x, y, zcut)) / maxRadDelta;
            if (RandomDecay->Uniform() < rc_weight) accept = true;
        } else {
            accept = true;
        }
    } while (accept == false);

    mcadd4gencpp(PDG_ID_gam, gamom, 1);
    mcadd4gencpp(PDG_ID_elp, epmom, 1);
    mcadd4gencpp(PDG_ID_elm, emmom, 1);
}

// generate 4-momenta of the gamma, gamma, e+, e-
void generate_dalitz_4body(TLorentzVector &gamom1,
                           TLorentzVector &gamom2,
                           TLorentzVector &epmom,
                           TLorentzVector &emmom,
                           double zcut, double xmincutoff) {
    double xmin = xmincutoff;
    double m42max = M42max;     // default constant
    if (xmin == 0.5) m42max = 1.5e8; // smaller cutoff for lowered xmin
    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
    // following variables are defined static, as they need to be initialized only once
    static TLorentzVector ppion(0.0, 0.0, 0.0, MP0); // pi0 at rest
    static TGenPhaseSpace pi0D4bodyEvent;
    static int gen_phase_space_calls = 0;

    if (!gen_phase_space_calls) {
      static double masses[4] = {0.0, 0.0, MEL, MEL}; // g, g, e+, e-
      pi0D4bodyEvent.SetDecay(ppion, 4, masses);
      gen_phase_space_calls++;
    }

    double weight = 0;          // pi0D4bodyEvent weight
    double M42 = 0;             // matrix element
    TLorentzVector *pg1 = nullptr, *pg2 = nullptr, *pep = nullptr, *pem = nullptr;
    do {
        // kinematic variables
        double x = 0, z = 0, a = 0, b = 0, c = 0, d = 0, e = 0, f = 0;
        // generate momenta
        weight = pi0D4bodyEvent.Generate();
        pg1 = pi0D4bodyEvent.GetDecay(0);
        pg2 = pi0D4bodyEvent.GetDecay(1);
        pep = pi0D4bodyEvent.GetDecay(2);
        pem = pi0D4bodyEvent.GetDecay(3);

        // compute kinematic variables
        x = (*pep + *pem).M2() / SQMP0;
        if (x < xmin) {
            weight = 0;         // reject event
            continue;
        }
        z = (*pg1 + *pg2).M2() / SQMP0;
        // M42 is divergent for z->0 => cut-off
        if (z < zcut) {
            weight = 0;         // reject event
            continue;
        }
        // the following check ensures that all 4-momenta are valid
        if ((TMath::IsNaN(x) != 0) || (TMath::IsNaN(z) != 0)) {
            weight = 0;         // reject event
            continue;
        }
        a = (*pep) * (*pg2) / SQMP0;
        b = (*pem) * (*pg2) / SQMP0;
        c = (*pep) * (*pg1) / SQMP0;
        d = (*pem) * (*pg1) / SQMP0;
        e = (*pep + *pem + *pg2).M2() / SQMP0;
        f = (*pep + *pem + *pg1).M2() / SQMP0;

        // find matrix element squared
        M42 = pi0dal_4body_M42(x,z,a,b,c,d,e,f);
        M42*= pow((1 + aslopeVMD*x), 2.); // multiply by form factor
    } while ((m42max * weightMax * RandomDecay->Uniform()) > (M42 * weight)); // accept-reject

    // copy momenta from TGenPhaseSpace internal structure
    gamom1 = *pg1;
    gamom2 = *pg2;
    epmom  = *pep;
    emmom  = *pem;

    mcadd4gencpp(PDG_ID_gam, gamom1, 3);
    mcadd4gencpp(PDG_ID_gam, gamom2, 3);
    mcadd4gencpp(PDG_ID_elp, epmom, 3);
    mcadd4gencpp(PDG_ID_elm, emmom, 3);
    return;
}

double ratio_34body(double zcut)
{
    if (zcut == 0.01) // precise numeric integration result provided by T. Husek for default zcut= 0.01
        return 17.397;
    else
        return (                // approximate formula for all other values
            (5.3690764 - 0.832133 * zcut + 2.966765 * pow(zcut, 2) + 0.107166*log(zcut)) /
            (-0.201556 - 0.263745 * pow(zcut, (1./3)) + 0.517361*sqrt(zcut) - 0.052051*zcut - 0.105731*log(zcut))
            );
}

double radcorr_full(double x, double y, double zcut)
{
    double ms  = radcorr_MS(x, y, zcut);
    double gir = radcorr_1gIR(x, y, zcut);
    return (ms + gir);
}

double radcorr_MS(double x, double y, double zcut)
{
    double zmax = 1 + x - sqrt(4 * x + (pow(1 - x, 2)*pow(y, 2)) / (1 - v2 / x));
    double IJTr = NAN;
    int i = ord;
    double z_upper = TMath::Min(zcut,zmax); // upper bound of integration
    xGlobal = x;            // used in f_JTr
    yGlobal = y;            // used in f_JTr
    while (std::isnan(IJTr) || std::isinf(IJTr)) {
        IJTr = gauss_legendre(i, f_JTr, 0, z_upper); // numerically integrate f_JTr
        i = i/2;
        if (i < 2){ IJTr = 0; break; }
    }

    double dBrems = (AlphaQED * x * (IJTr + IJTrDiv(x, y, z_upper))) / (16 * TMath::Pi() * pow(1 - x, 2) * (1 + pow(y, 2) + v2 / x));
    return (dBrems + AlphaQED / TMath::Pi() * dVirt(x, y));
}

// in the 1gIR correction computation masses in MeV are assumed
double radcorr_1gIR(double x, double y, double /*zcut*/)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV
    return (
        -2 * AlphaQED / TMath::Pi() * mpi0 / (1 + pow(y, 2) + v2 / x)
                  * (4 * v * T(x, y)
                     + mpi0 * (x * pow(1 - y, 2) - v2) * A(x, y)
                     + mpi0 * (x * pow(1 + y, 2) - v2) * A(x, -y))
        );
}

/* the function to be numerically integrated must be of only one variable */
double f_JTr(double z)
{
    return JTr(xGlobal, yGlobal, z);
}

double JTr(double x, double y, double z)
{
    return (
        (JTrv0(x, y, z) + JTrv0(x, -y, z)) +
        (JTrv2(x, y, z) + JTrv2(x, -y, z)) * v2 +
        (JTrv4(x, y, z) + JTrv4(x, -y, z)) * v2 * v2
        );
}

double JTrv0(double x, double y, double z)
{
    return (
        (16*(-1 + x))/x - (16*(-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) -
                               z*(-2 + 4*v2 + z) + x*(2 - 2*pow(y,2) + 6*z)))/
        (-4*v2*z + pow(-1 + x + (-1 + x)*y + z,2)) +
        (32*(1 + y + pow(x,2)*(1 + y) + z*(-2 + y + z) -
             x*(2*(1 + y) + (2 + y)*z)))/
        (-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2)) +
        (16*z*log(-1 - 2/(-1 + sqrt(1 - v2/x))))/
        sqrt(x*(-v2 + x)) -
        (32*log(-1 - 2/(-1 + sqrt(1 - (4*x)/pow(1 + x - z,2)))))/
        ((1 + x - z)*sqrt(pow(x,2) + pow(-1 + z,2) - 2*x*(1 + z))) +
        (16*((-3 + y)*pow(1 + y,2) - pow(x,3)*(-3 + y)*pow(1 + y,2) +
             (9 + 8*v2 + 10*y - 4*v2*y + pow(y,2))*z -
             (9 + 8*v2 + 5*y)*pow(z,2) + 3*pow(z,3) +
             pow(x,2)*(1 + y)*(3*(-3 - 2*y + pow(y,2)) + (5 + y)*z) +
             x*(-3*(-3 + y)*pow(1 + y,2) -
                2*(7 + 4*v2 + 8*y - 2*v2*y + pow(y,2))*z +
                5*(1 + y)*pow(z,2)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*z)/pow(-1 + x - y + x*y + z,2)))))/
        pow(pow(1 + y,2) + pow(x,2)*pow(1 + y,2) -
            2*(1 + 2*v2 + y)*z + pow(z,2) + 2*x*(1 + y)*(-1 - y + z),1.5) \
        - (16*x*(pow(x,7)*pow(1 + y,3)*(1 + pow(y,2)) -
                 pow(x,6)*pow(1 + y,2)*
                 (5*(1 + y + pow(y,2) + pow(y,3)) +
                  (5 + 2*y + 3*pow(y,2))*z) +
                 4*v2*(-1 + z)*z*
                 (-1 + y + 2*pow(y,2) - 3*y*z + pow(z,2)) +
                 2*pow(x,5)*(1 + y)*(pow(1 + y,2)*(5 + y + 5*pow(y,2)) +
                                     (9 + 15*y + 11*pow(y,2) + 5*pow(y,3))*z +
                                     (5 + 4*y + 2*pow(y,2))*pow(z,2)) -
                 2*pow(x,4)*(pow(1 + y,3)*(4 + 4*y + 5*pow(y,2)) +
                             (1 + y)*(2*(6 + v2) + 24*y +
                                      (17 + 2*v2)*pow(y,2) + 5*pow(y,3))*z +
                             (11 + 27*y + 22*pow(y,2) + 6*pow(y,3))*pow(z,2) +
                             (5 + 6*y + 2*pow(y,2))*pow(z,3)) +
                 pow(x,3)*(pow(1 + y,3)*(-1 + 12*y + 5*pow(y,2)) +
                           4*(1 + y)*(2*(2 + v2) + 9*y +
                                      (5 + 3*v2)*pow(y,2))*z +
                           4*(5 + 3*v2 + 2*(7 + v2)*y +
                              (12 + v2)*pow(y,2) + 3*pow(y,3))*pow(z,2) +
                           4*(2 + 6*y + 3*pow(y,2))*pow(z,3) + (5 + 3*y)*pow(z,4)) -
                 2*x*(-((-1 + y)*pow(1 + y,3)) +
                      (1 + y)*(-1 - (1 + 8*v2)*y +
                               (1 - 2*v2)*pow(y,2) + pow(y,3))*z +
                      2*(-3*v2 + y + (1 + 3*v2)*pow(y,2))*
                      pow(z,2) - 2*y*(1 + 4*v2 + y)*pow(z,3) -
                      (1 + 2*v2)*pow(z,4) + pow(z,5)) -
                 pow(x,2)*(pow(1 + y,3)*(-5 + 8*y + pow(y,2)) -
                           (1 + y)*(-7 - 8*v2 - (13 + 8*v2)*y -
                                    (1 + 12*v2)*pow(y,2) + 5*pow(y,3))*z +
                           4*(2 + 3*v2 + (4 + 6*v2)*y + 3*pow(y,2) +
                              pow(y,3))*pow(z,2) +
                           4*(2 + 3*v2 + (4 + v2)*y + 3*pow(y,2))*
                           pow(z,3) + 3*(-1 + y)*pow(z,4) + pow(z,5)))*
           log(-1 - 2/
               (-1 + sqrt(1 - (4*v2*z)/
                          (x*pow(1 + y - x*(1 + y) + z,2))))))/
        ((1 + x - z)*(-1 + x - y + x*y + z)*
         pow(x*(pow(x,3)*pow(1 + y,2) - 4*v2*z -
                2*pow(x,2)*(1 + y)*(1 + y + z) + x*pow(1 + y + z,2)),1.5)) -
        (16*(pow(x,3)*(1 + pow(y,2))*(-1 - y + (-1 + y)*z) +
             pow(x,2)*(1 + 3*y + 5*pow(y,2) + 3*pow(y,3) +
                       y*(3 - 2*y - 3*pow(y,2))*z +
                       (3 - 2*y + pow(y,2))*pow(z,2)) +
             (-1 + z)*(-pow(1 + y,3) + pow(1 + y,2)*z -
                       (1 + y)*pow(z,2) + pow(z,3)) +
             x*(-(pow(1 + y,2)*(1 + 3*y)) +
                (-3 + y + 7*pow(y,2) + 3*pow(y,3))*z -
                (-3 + y + 2*pow(y,2))*pow(z,2) + (-3 + y)*pow(z,3)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*x*z)/
                        pow((-1 + z)*(-1 - y + z) + x*(-1 - y + (-1 + y)*z),2)))))/
        ((1 + x - z)*(-1 + x - y + x*y + z)*
         sqrt(pow(1 + y - z,2)*pow(-1 + z,2) +
              pow(x,2)*pow(1 + y + z - y*z,2) +
              2*x*(-pow(1 + y,2) + (1 - 2*v2 + 3*y + 2*pow(y,2))*z -
                   (-1 + 2*y + pow(y,2))*pow(z,2) + (-1 + y)*pow(z,3)))) -
        (16*(-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) + 2*z - pow(z,2) +
             2*x*(1 - pow(y,2) + z))*
         log(fabs(1 + 2/
                  (-1 + sqrt(1 - (v2*z)/
                             (x + (pow(-1 + x,2)*pow(y,2))/4. - pow(1 + x - z,2)/4.))\
                      ))))/sqrt((-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) + 2*z -
                                 pow(z,2) + 2*x*(1 - pow(y,2) + z))*
                                (-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) +
                                 (2 - 4*v2)*z - pow(z,2) + 2*x*(1 - pow(y,2) + z)))
        );
}

double JTrv2(double x, double y, double z)
{
    return (
        (-16*z)/(v2*x) + (64*v2*(-1 + x + z) -
                          64*x*(-1 + x + (-1 + x)*y + z))/
        (4*pow(v2,2)*z - v2*pow(-1 + x + (-1 + x)*y + z,2)) -
        (32*(1 + pow(x + (-1 + x)*y - z,2))*
         (pow(x,2)*(1 + y) + v2*(1 + z) - x*(1 + v2 + y + z))\
            )/(v2*x*(-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2))) +
        (32*(1 + y + pow(x,2)*(1 + y) + z*(-2 + y + z) -
             x*(2*(1 + y) + (2 + y)*z)))/
        (x*(-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2))) +
        (8*z*log(-1 - 2/(-1 + sqrt(1 - v2/x))))/
        (x*sqrt(x*(-v2 + x))) -
        (32*(pow(x,3)*pow(1 + y,3) - pow(1 + y,2)*(2 + y) +
             (5 + 4*v2 + 4*(2 + v2)*y + 3*pow(y,2))*z -
             (4 + 3*y)*pow(z,2) + pow(z,3) -
             pow(x,2)*(1 + y)*(4 + 7*y + 3*pow(y,2) + z - 3*y*z) +
             x*(pow(1 + y,2)*(5 + 3*y) -
                2*(2 + 5*y + 2*v2*y + 3*pow(y,2))*z +
                (-1 + 3*y)*pow(z,2)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*z)/pow(-1 + x - y + x*y + z,2)))))/
        ((-1 + x - y + x*y + z)*pow(pow(1 + y,2) +
                                    pow(x,2)*pow(1 + y,2) - 2*(1 + 2*v2 + y)*z +
                                    pow(z,2) + 2*x*(1 + y)*(-1 - y + z),1.5)) +
        (16*sqrt(x*(-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2)))*
         (1 + y + pow(y,2) + pow(y,3) + pow(x,4)*pow(1 + y,3) +
          (-2 + 3*y + pow(y,3))*z + (2 - 3*y + 3*pow(y,2))*pow(z,2) +
          (-2 + 3*y)*pow(z,3) + pow(z,4) -
          pow(x,3)*pow(1 + y,2)*(2 + 4*y + (4 + y)*z) +
          pow(x,2)*(1 + y)*(2 + 6*y + 6*pow(y,2) +
                            (2 + 9*y + 3*pow(y,2))*z + 3*(2 + y)*pow(z,2)) -
          x*(2 + 4*y + 6*pow(y,2) + 4*pow(y,3) +
             (4 + 5*y + 6*pow(y,2) + 3*pow(y,3))*z +
             (-2 + 6*y + 6*pow(y,2))*pow(z,2) + (4 + 3*y)*pow(z,3)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*z)/
                        (x*pow(1 + y - x*(1 + y) + z,2))))))/
        (x*pow(pow(x,3)*pow(1 + y,2) - 4*v2*z -
               2*pow(x,2)*(1 + y)*(1 + y + z) + x*pow(1 + y + z,2),2)) +
        (16*(pow(x,8)*(-1 + y)*pow(1 + y,5) -
             pow(x,7)*pow(1 + y,4)*
             (-7 - 2*y + 5*pow(y,2) + (-5 + 2*y + pow(y,2))*z) +
             pow(x,6)*pow(1 + y,2)*
             (-13 - 34*y - 24*pow(y,2) + 10*pow(y,3) + 9*pow(y,4) +
              2*(-12 - 21*y - pow(y,2) + 9*pow(y,3) + 3*pow(y,4))*z +
              (-9 - 12*y - pow(y,2) + 2*pow(y,3))*pow(z,2)) +
             pow(x,5)*(1 + y)*(1 + 21*y + 52*pow(y,2) + 32*pow(y,3) -
                               5*pow(y,4) - 5*pow(y,5) +
                               (35 + 4*v2 + (105 + 4*v2)*y +
                                (90 - 4*v2)*pow(y,2) -
                                2*(9 + 2*v2)*pow(y,3) - 45*pow(y,4) -
                                15*pow(y,5))*z +
                               (27 + 67*y + 35*pow(y,2) - 7*pow(y,3) - 10*pow(y,4))*
                               pow(z,2) + (5 + 15*y + 11*pow(y,2) + pow(y,3))*
                               pow(z,3)) + 4*v2*(-1 + z)*z*
             (2 - 3*pow(y,2) + pow(y,4) + (-5 + 3*pow(y,2))*z +
              (5 - 2*pow(y,2))*pow(z,2) - 3*pow(z,3) + pow(z,4)) -
             pow(x,4)*(pow(1 + y,2)*
                       (-13 - 14*y + 8*pow(y,2) + 10*pow(y,3) + 5*pow(y,4)) -
                       4*(1 + y)*(-3 - 4*v2 - 2*(8 + 3*v2)*y +
                                  (-22 + v2)*pow(y,2) +
                                  (-2 + 3*v2)*pow(y,3) + 10*pow(y,4) +
                                  5*pow(y,5))*z +
                       2*(15 + 6*v2 + 12*(4 + v2)*y +
                          (50 + 4*v2)*pow(y,2) +
                          (6 - 4*v2)*pow(y,3) -
                          (19 + 2*v2)*pow(y,4) - 10*pow(y,5))*pow(z,2) \
                       + 4*(2 + 10*y + 14*pow(y,2) + 8*pow(y,3) + pow(y,4))*pow(z,3) +
                       (-5 + 9*pow(y,2) + 4*pow(y,3))*pow(z,4)) +
             pow(x,3)*(pow(1 + y,3)*
                       (-3 - 17*y - pow(y,2) + 9*pow(y,3)) +
                       (-9 + 8*v2 + 6*(-1 + 4*v2)*y +
                        (47 + 40*v2)*pow(y,2) + 76*pow(y,3) +
                        (17 - 8*v2)*pow(y,4) - 30*pow(y,5) -
                        15*pow(y,6))*z -
                       2*(-7 - 8*v2 - 4*(6 + 5*v2)*y -
                          22*pow(y,2) + 6*(1 + 2*v2)*pow(y,3) +
                          (21 + 8*v2)*pow(y,4) + 10*pow(y,5))*pow(z,2) \
                       + 2*(1 + y)*(5 + 4*v2 + (3 + 8*v2)*y + 9*pow(y,2) +
                                    3*pow(y,3))*pow(z,3) +
                       3*pow(1 + y,2)*(-1 + 4*y)*pow(z,4) +
                       (-9 - 6*y + pow(y,2))*pow(z,5)) +
             pow(x,2)*(-(pow(1 + y,3)*
                         (7 - 13*y - 3*pow(y,2) + 5*pow(y,3))) +
                       2*(1 + y)*(6 + 12*v2 + 9*y -
                                  2*(3 + 2*v2)*pow(y,2) -
                                  4*(3 + v2)*pow(y,3) + 3*pow(y,5))*z +
                       (-9 - 32*v2 - 10*(3 + 4*v2)*y -
                        26*pow(y,2) + 8*(1 + 3*v2)*pow(y,3) +
                        (23 + 24*v2)*pow(y,4) + 10*pow(y,5))*pow(z,2) \
                       - 4*(-1 + 2*(-1 + v2)*y + (-4 + 3*v2)*pow(y,2) +
                            pow(y,4))*pow(z,3) -
                       (5 - 8*v2 + (4 + 8*v2)*y +
                        (15 + 8*v2)*pow(y,2) + 12*pow(y,3))*pow(z,4) \
                       - 2*(-1 + y)*y*pow(z,5) + (5 + 2*y)*pow(z,6)) +
             x*(pow(1 + y,3)*(3 - 3*y - pow(y,2) + pow(y,3)) -
                (1 + y)*(7 - 4*v2 + (9 + 20*v2)*y +
                         (-2 + 4*v2)*pow(y,2) -
                         6*(1 + 2*v2)*pow(y,3) - pow(y,4) + pow(y,5))*z \
                - (1 + y)*(-3 - (11 + 24*v2)*y -
                           (3 + 8*v2)*pow(y,2) +
                           (3 + 16*v2)*pow(y,3) + 2*pow(y,4))*pow(z,2) +
                (5 - 8*v2 - 4*(1 + 4*v2)*y -
                 2*(5 + 12*v2)*pow(y,2) - 4*pow(y,3) +
                 pow(y,4))*pow(z,3) +
                (-7 + 16*v2 + (-2 + 8*v2)*y +
                 (3 + 16*v2)*pow(y,2) + 4*pow(y,3))*pow(z,4) +
                (3 - 12*v2 + 4*y + pow(y,2))*pow(z,5) +
                (1 - 2*y)*pow(z,6) - pow(z,7)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*z)/
                        (x*pow(1 + y - x*(1 + y) + z,2))))))/
        ((1 + x - z)*(1 + x - y + x*y - z)*(-1 + x - y + x*y + z)*
         pow(x*(pow(x,3)*pow(1 + y,2) - 4*v2*z -
                2*pow(x,2)*(1 + y)*(1 + y + z) + x*pow(1 + y + z,2)),1.5)) +
        (32*(-1 + (-1 + x)*y)*(x + z)*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*x*z)/
                        pow((-1 + z)*(-1 - y + z) + x*(-1 - y + (-1 + y)*z),2)))))/
        ((1 + x - z)*(-1 + x - y + x*y + z)*
         sqrt(pow(1 + y - z,2)*pow(-1 + z,2) +
              pow(x,2)*pow(1 + y + z - y*z,2) +
              2*x*(-pow(1 + y,2) + (1 - 2*v2 + 3*y + 2*pow(y,2))*z -
                   (-1 + 2*y + pow(y,2))*pow(z,2) + (-1 + y)*pow(z,3)))) -
        (32*pow(x - z,2)*log(-1 - 2/
                             (-1 + sqrt(1 - (4*v2*x*z)/
                                        pow((-1 + z)*(-1 + y + z) - x*(1 - y + z + y*z),2)))))/
        ((1 + x - z)*(1 + x - y + x*y - z)*
         sqrt(pow(-1 + z,2)*pow(-1 + y + z,2) +
              pow(x,2)*pow(1 - y + z + y*z,2) -
              2*x*(pow(-1 + y,2) + (-1 + 2*v2 + 3*y - 2*pow(y,2))*z +
                   (-1 - 2*y + pow(y,2))*pow(z,2) + (1 + y)*pow(z,3)))) -
        (32*(pow(x,3)*(-1 + pow(y,2)) +
             pow(x,2)*(2 - 2*pow(y,2) + (5 - 3*pow(y,2))*z) +
             x*(-1 + pow(y,2) + 6*pow(y,2)*z - 7*pow(z,2)) +
             z*(3 - 3*pow(y,2) - 2*z + 3*pow(z,2)))*
         log(fabs(1 + 2/
                  (-1 + sqrt(1 - (v2*z)/
                             (x + (pow(-1 + x,2)*pow(y,2))/4. - pow(1 + x - z,2)/4.))\
                      ))))/((-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) + 2*z - pow(z,2) +
                             2*x*(-1 - pow(y,2) + z))*
                            sqrt((-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) + 2*z -
                                  pow(z,2) + 2*x*(1 - pow(y,2) + z))*
                                 (-1 + pow(y,2) + pow(x,2)*(-1 + pow(y,2)) +
                                  (2 - 4*v2)*z - pow(z,2) + 2*x*(1 - pow(y,2) + z))))
        );
}

double JTrv4(double x, double y, double z)
{
    return (
        (-192*z*pow(pow(x,2)*(1 + y) + v2*(1 + z) -
                    x*(1 + v2 + y + z),2))/
        (v2*pow(x,2)*pow(-4*v2*z +
                         x*pow(1 + y - x*(1 + y) + z,2),2)) +
        (64*x*(1 + y + x*(-2 + x + (-2 + x)*y - z) + 3*z) -
         16*v2*(5*pow(-1 + x,2) + 2*(5 - 3*x)*z + pow(z,2)))/
        (v2*pow(x,2)*(-4*v2*z +
                      x*pow(1 + y - x*(1 + y) + z,2))) +
        (-16*(-1 + x)*y*(-1 + x - z) -
         16*(pow(x,2) + pow(-1 + z,2) - 2*x*(1 + z)))/
        (pow(x,2)*(-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2))) -
        (32*(pow(x,4)*pow(1 + y,2) - v2*(-3 + z)*z -
             pow(x,3)*(1 + y)*(3 + 3*y + 2*z) -
             x*(pow(1 + y,2) + (1 + 3*v2 + y)*z) +
             pow(x,2)*(3*pow(1 + y,2) + 3*(1 + y)*z + pow(z,2)))*
         log(-1 - 2/
             (-1 + sqrt(1 - (4*v2*z)/
                        (x*pow(1 + y - x*(1 + y) + z,2))))))/
        (x*pow(x*(pow(x,3)*pow(1 + y,2) - 4*v2*z -
                  2*pow(x,2)*(1 + y)*(1 + y + z) + x*pow(1 + y + z,2)),1.5)) -
        (32*sqrt(x*(-4*v2*z + x*pow(1 + y - x*(1 + y) + z,2)))*
         (pow(x,6)*pow(1 + y,3) -
          pow(x,5)*pow(1 + y,2)*(5*(1 + y) + (4 + y)*z) -
          v2*(-1 + z)*z*(1 + y + (-4 + 3*y)*z + 3*pow(z,2)) +
          pow(x,4)*(1 + y)*(10*pow(1 + y,2) +
                            (9 + 14*y + 5*pow(y,2))*z + 3*(2 + y)*pow(z,2)) -
          pow(x,3)*(10*pow(1 + y,3) +
                    (1 + y)*(3 + v2 + 12*y + 9*pow(y,2))*z +
                    (1 + 9*y + 8*pow(y,2))*pow(z,2) + (4 + 3*y)*pow(z,3)) +
          pow(x,2)*(5*pow(1 + y,3) +
                    (1 + y)*(-5 + 3*v2 + 2*y + 7*pow(y,2))*z +
                    (-4 - v2 + (3 - 2*v2)*y + 7*pow(y,2))*
                    pow(z,2) + (-5 + y)*pow(z,3) + pow(z,4)) +
          x*(-pow(1 + y,3) - (1 + y)*
             (-3 + 3*v2 - y + 2*pow(y,2))*z -
             (1 - 6*v2 + 3*y + 2*pow(y,2))*pow(z,2) +
             (-3 + 5*v2 + (2 + 3*v2)*y)*pow(z,3) +
             2*pow(z,4)))*log(-1 -
                              2/(-1 + sqrt(1 - (4*v2*z)/
                                           (x*pow(1 + y - x*(1 + y) + z,2))))))/
        (pow(x,2)*pow(pow(x,3)*pow(1 + y,2) - 4*v2*z -
                      2*pow(x,2)*(1 + y)*(1 + y + z) + x*pow(1 + y + z,2),3))
        );
}

double IJTrDiv(double x, double y, double z_upper)
{
    return (
        (-32 * pow(1 - x, 2)*(1 + v2 / x + pow(y, 2))*(-log(1 - pow(y, 2)) / 2. +
    (1 + ((2 - v2/x)*log((1 - sqrt(1 - v2/x)) / (1 + sqrt(1 - v2/x)))) /
    (2.*sqrt(1 - v2/x)))*log(
        (2 * z_upper) / (1 - x)) -
    ((2 - v2/x)*(log((1 - sqrt(1 - v2/x)) / (1 + sqrt(1 - v2/x)))*
    (log(v2 / x) + 2 * log((sqrt(1 - v2/x) + y) / (2.*sqrt(1 - v2/x)))) -
    dilog(((1 - sqrt(1 - v2/x))*(-sqrt(1 - v2/x) + y)) /
    ((1 + sqrt(1 - v2/x))*(sqrt(1 - v2/x) + y))) +
    dilog(((1 + sqrt(1 - v2/x))*(-sqrt(1 - v2/x) + y)) /
    ((1 - sqrt(1 - v2/x))*(sqrt(1 - v2/x) + y))))) /
    (4.*sqrt(1 - v2/x)))) / x
    );
}


double dVirt(double x, double y)
{
    return (
        (-34.0/9 + (pow(TMath::Pi(),2)*(2 - v2/x))/
         (2.*sqrt(1 - v2/x)) - (2*(-1 + v2/x))/3.) +
        (2*pow(v,6) - 13*pow(x,3)*(1 + pow(y,2)) +
         2*pow(v,4)*x*(5 + pow(y,2)) +
         v2*pow(x,2)*(1 + 8*pow(y,2)))*
        log(-1 + 2/(1 + sqrt(1 - v2/x)))/
        (6.*sqrt(1 - v2/x)*pow(x,2)*(v2 + x + x*pow(y,2))) \
        + (v2 - 2*x)*pow(log(-1 + 2/(1 + sqrt(1 - v2/x))),2)/
        (4.*sqrt(1 - v2/x)*x) +
        (v2 - 2*x)*dilog(2 - 2/(1 + sqrt(1 - v2/x)))/
        (sqrt(1 - v2/x)*x)
        );
}

double A(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV

    return (
        ((-4*(-1 + x + y - x*y + v2))/(pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)) -
         (4*v2*(3*(-1 + x)*(-1 + y) + v2)*(1 - (2*chi)/3.))/
         (pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)*(2*(-1 + x)*(-1 + y) + v2)) -
         ((-12*(-1 + x)*(-1 + y)*v2 - 4*pow(v,4))*
          (2 - (2*chi)/3. + (-1 + v2/(2 + 2*x*(-1 + y) - 2*y + v2))*
           log(abs(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))))/
         (pow(mpi0,2)*pow(-1 + x,2)*pow(-1 + y,2)*(2*(-1 + x)*(-1 + y) + v2)) +
         ((4*pow(-1 + x,2)*pow(-1 + y,3)*(1 + y) - 8*(-1 + y)*(-1 + 2*x + y)*v2 - 8*pow(v,4))*
          (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*x*(-1 + y) - 2*y + v2)/v2),2)/2. -
           dilog(v2/(2 + 2*x*(-1 + y) - 2*y + v2))))/
         (pow(mpi0,2)*(-1 + x)*(-1 + y)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))
         - (4*(1 + y)*((-1 + x)*(-1 + pow(y,2)) + 2*v2)*
            (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*y - 2*x*(1 + y) + v2)/v2),2)/2. -
             dilog(v2/(2 + 2*y - 2*x*(1 + y) + v2))))/
         (pow(mpi0,2)*v2*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) -
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
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) -
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
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) -
         (16*(-1 + pow(y,2))*(pow(-1 + x,2)*(-1 + pow(y,2)) - 4*v2)*
          (log((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
               4.)*log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
                            sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                                 (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                                  (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) +
           dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                      (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
                      ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v,4) -
                      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) -
           dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                      (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
                      ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v,4) +
                      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.)))/
         (pow(mpi0,2)*pow(v,4)*(x*(-1 + pow(y,2)) + v2)*
          sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
               (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))
              )))/16.
        );
}

double T(double x, double y)
{
    double mpi0 = MP0 * 1e3;    // the formula assumes mass in MeV
    return (
        ((-2*v*(6/((-1 + x)*(-1 + pow(y,2))) + 1/(2*(-1 + x)*(1 + y) - v2) - 1/(2*(-1 + x)*(-1 + y) + v2))*(1 - (2*chi)/3.))/mpi0 -
         (4*v*(2 - 3*log((pow(mpi0,2)*v2)/(4.*pow(mu,2)))))/(mpi0*(-1 + x)*(-1 + pow(y,2))) -
         (2*v*(2 - (2*chi)/3. + (-1 + v2/(2 + 2*x*(-1 + y) - 2*y + v2))*
               log(abs(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2))))/(mpi0*(2*(-1 + x)*(-1 + y) + v2)) -
         (2*v*(2 - (2*chi)/3. + (-1 + v2/(2 + 2*y - 2*x*(1 + y) + v2))*
               log(abs(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))))/(mpi0*(-2*(-1 + x)*(1 + y) + v2)) +
         (4*(-1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*
          (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*x*(-1 + y) - 2*y + v2)/v2),2)/2. -
           dilog(v2/(2 + 2*x*(-1 + y) - 2*y + v2))))/
         (mpi0*v*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)) -
         (4*(1 + y)*(-pow(y,2) + x*(-2 + pow(y,2)) + 2*v2)*
          (pow(TMath::Pi(),2)/6. - pow(log((2 + 2*y - 2*x*(1 + y) + v2)/v2),2)/2. -
           dilog(v2/(2 + 2*y - 2*x*(1 + y) + v2))))/
         (mpi0*v*(x*(-1 + pow(y,2)) + v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)) -
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
         (2.*mpi0*(-1 + x)*(-1 + y)*v*(x*(-1 + pow(y,2)) + v2)*
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*x*(-1 + y) - 2*y + v2)/v2,2))) -
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
         (2.*mpi0*(-1 + x)*(1 + y)*v*(x*(-1 + pow(y,2)) + v2)*
          sqrt(-16/v2 + pow(1 + 4/v2 - (2 + 2*y - 2*x*(1 + y) + v2)/v2,2))) -
         (2*(8*(-1 + pow(y,2))*(2*x + pow(-1 + x,2)*pow(y,2)) + 16*(1 - 2*pow(y,2))*v2)*
          (log((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))/
               4.)*log((v2*(8/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2) +
                            sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                                 (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                                  (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) +
           dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                      (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
                      ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v,4) -
                      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.) -
           dilog((v2*(-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2 +
                      (2 + 2*y - 2*x*(1 + y) + v2)/v2 -
                      ((2 + 2*x*(-1 + y) - 2*y + v2)*(2 + 2*y - 2*x*(1 + y) + v2))/pow(v,4) +
                      sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                           (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                            (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)))))/8.)))/
         (mpi0*pow(v,3)*(x*(-1 + pow(y,2)) + v2)*sqrt((-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*
                                                   (-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2)*
                                                   (16/v2 + (-1 + (2 + 2*x*(-1 + y) - 2*y + v2)/v2)*(-1 + (2 + 2*y - 2*x*(1 + y) + v2)/v2))
             )))/16.
        );
}

double pi0dal_4body_M42(double x, double z, double a, double b, double c, double d, double e, double f)
{
    double term1 = M42v0(x,z,a,b,c,d,e,f) + v2*M42v2(x,z,a,b,c,d,e,f) + v2*v2*M42v4(x,z,a,b,c,d,e,f);
    double term2 = M42v0(x,z,c,d,a,b,f,e) + v2*M42v2(x,z,c,d,a,b,f,e) + v2*v2*M42v4(x,z,c,d,a,b,f,e); // photon permutation

    return term1 + term2;
}

double M42v0(double x, double z, double a, double b, double c, double d, double e, double f)
{
    return (8*pow(b,3)*pow(c,2)*e*(2*(c + d) + x) + 4*c*d*f*pow(x,2)*(4*(pow(c,2) + pow(d,2)) + 2*(c + d)*z + pow(z,2)) +
     4*pow(a,2)*d*(-32*pow(b,2)*c*e + 8*b*pow(c,2)*e + 8*pow(c,3)*f + 2*pow(d,2)*(pow(e,2) + 12*c*f) + 2*c*e*pow(x,2) + 8*c*d*f*z -
        4*b*e*(2*c + x)*z - e*x*(2*c + x)*z + 2*c*(e + f)*pow(z,2)) +
     4*pow(b,2)*c*(8*d*(pow(d,2)*f + pow(c,2)*(2*e + 3*f)) + 4*c*(c + 3*d)*e*x + 2*(c + d)*e*pow(x,2) + (8*c*d*(e + f) - 2*(c + d)*e*x - e*pow(x,2))*z +
        2*d*(e + f)*pow(z,2)) + a*(64*pow(b,3)*pow(c,2)*e + 8*b*c*d*(8*pow(c - d,2)*f + e*pow(x,2) - (4*(c + d)*f + 9*e*x)*z) +
        x*(32*c*d*(pow(c,2) - c*d + 2*pow(d,2))*f + (16*c*pow(d,2)*f - 12*c*d*e*x - (c + 2*d)*e*pow(x,2))*z + 8*c*d*(-e + f)*pow(z,2) - c*e*pow(z,3)) +
        16*pow(b,2)*c*e*(2*pow(c,2) + 2*pow(d,2) - (2*d + x)*z + c*(6*d + 3*x + 2*z))) +
     b*d*x*(64*pow(c,3)*f + 16*pow(c,2)*f*(-2*d + z) - e*z*(pow(x,2) + pow(z,2)) + 4*c*(8*pow(d,2)*f + z*(-3*e*x - 2*e*z + 2*f*z))))/(a*b*c*d*pow(e,2)*f);
}

double M42v2(double x, double z, double a, double b, double c, double d, double e, double f)
{
    return (-2*pow(b,2)*c*d*f*(2*b + x)*(4*pow(d,2) + pow(2*c + z,2)) +
     pow(a,2)*(-16*pow(b,2)*c*d*e*(x - 3*z) + 16*pow(b,3)*e*(-2*c*(c + 2*d) + (c + d)*z) +
        b*(-48*c*d*(pow(c,2) + pow(d,2))*f + 2*(8*c*d*(c*e - (c + 2*d)*f) + 12*c*d*e*x + (c + 2*d)*e*pow(x,2))*z +
           (-2*d*e*x + c*(16*d*e - 4*d*f + 3*e*x))*pow(z,2) + (c + 2*d)*e*pow(z,3)) - 2*c*d*f*x*(4*pow(c,2) + pow(2*d + z,2))) +
     4*pow(a,3)*(4*pow(b,2)*d*e*(-4*c + z) - c*d*f*(4*pow(c,2) + pow(2*d + z,2)) +
        b*e*(d*z*(x + z) - pow(c,2)*(8*d + x + z) + c*(-4*d*x + 8*d*z + x*z + pow(z,2)))) +
     a*b*(4*c*d*f*x*(8*c*d + 2*(c + d)*z + pow(z,2)) + 4*pow(b,2)*e*
         (-2*pow(c,2)*(4*d + x - 3*z) - d*(d - z)*(x + z) + c*(-4*d*(2*d + x) + (8*d + x)*z + pow(z,2))) +
        b*d*(-48*pow(c,3)*f - 32*pow(c,2)*f*z + e*z*(x + z)*(2*x + z) - 4*c*(12*pow(d,2)*f + 4*d*f*z + z*(-6*e*x - 4*e*z + f*z)))))/
        (2.*pow(a,2)*pow(b,2)*c*d*pow(e,2)*f);
}

double M42v4(double x, double /*z*/, double a, double b, double /*c*/, double /*d*/, double e, double /*f*/)
{
    return -(pow(1 - e,2)*pow(e - x,2))/(8.*pow(a,2)*pow(b,2)*pow(e,2));
}
