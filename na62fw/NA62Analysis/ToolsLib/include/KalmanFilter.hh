#ifndef KalmanFilter_H
#define KalmanFilter_H

#include "TROOT.h"
#include "TObject.h"
#include "TLorentzVector.h"
#include <vector>
#include <list>
#include <deque>
#include <Math/SMatrix.h>
using namespace std;

/*
 *       KalmanFilter Filter Class Definition.
 *       Matrix Dimension must be:
 *       F: n x n
 *       H: m x n
 *       Q: n x n
 *       V: m x n
 *       I: n x n
 *       X: m x 1
 *       C: n x n
 *       K: m x n
*
*/

class KalmanFilter {

public:

  static const int n = 5; //State vector dimension
  static const int m = 2; //Measurement vector dimension

  ROOT::Math::SMatrix<double,n,n>  Q; //Process Noise Covariance matrix
  ROOT::Math::SMatrix<double,n,n>  I; //Identity matrix

  std::vector<ROOT::Math::SVector<double,n >> X; //(Current) State vector
  std::vector<ROOT::Math::SVector<double,n >> XPred; //(Predicted) State vector
  std::vector<ROOT::Math::SVector<double,m >> mV;     // Measurements
  std::deque<ROOT::Math::SVector<double,n >> XSmooth; //(Predicted) State vector

  std::vector<ROOT::Math::SMatrix<double,m,m>> V;       // Measurement Covariance matrix
  std::vector<ROOT::Math::SMatrix<double,m,n>> H;       // Mesaurement Adaptation matrix
  std::vector<ROOT::Math::SMatrix<double,n,n>> F;       // System dynamics matrix
  std::vector<ROOT::Math::SMatrix<double,n,n>> CPred;   // (Predicted) State Covariance
  std::vector<ROOT::Math::SMatrix<double,n,n>> CFilt;   // (Filtered)  State Covariance
  std::deque<ROOT::Math::SMatrix<double,n,n>> CSmooth;   // (Smoothed)  State Covariance
  std::vector<ROOT::Math::SMatrix<double,m,m>> R;       // Residual    Covariance matrix
  std::vector<Double_t> chi2filt; // chi2 from filtered state vector
  std::deque<Double_t> chi2Smooth; //chi2 from smoothed state vector

  ROOT::Math::SMatrix<double,n,n> A; //Smoother gain matrix
  ROOT::Math::SMatrix<double,n,m> K; //Kalman Gain matrix
  //ROOT::Math::SMatrix<double,n,n> R; //Predicted result Covariance matrix              //Del
  ROOT::Math::SVector<double,m> rFilt; //residuals
  ROOT::Math::SVector<double,m> rSmooth; //residuals

  /* Inizial Value */
  //  ROOT::Math::SVector<double,n > X0;  //Initial State vector
  ROOT::Math::SMatrix<double,n,n> C0;
  bool verbose;

  /*
   * Constructor
   * _n: state vector dimension
   * _m: measurement vector dimension
   */

  explicit KalmanFilter(bool _verbose);

  /* Set Matrices (NO INPUT) */
  void setMatrices ( ROOT::Math::SMatrix<double,n,n> _F, ROOT::Math::SMatrix<double,m,n> _H, ROOT::Math::SMatrix<double,n,n> _Q, ROOT::Math::SMatrix<double,m,m> _V );

  /* Set Matrix H  */
  void setH( ROOT::Math::SMatrix<double,m,n> _H);

  /* Set Matrix V  */
  void setV( ROOT::Math::SMatrix<double,m,m> _V);


  /* Set Matrix F  */
  void setF( ROOT::Math::SMatrix<double,n,n> _F);

  /* Set Matrix Q  */
  void setQ( ROOT::Math::SMatrix<double,n,n> _Q);

  /* Set Initial Value */
  void setInitial(ROOT::Math::SVector<double,n > _X0, ROOT::Math::SMatrix<double,n,n> _C0 );

  /* Do prediction (NO INPUT) */
  void predict ( void );

  /* print results */
  void print();

  void setVerbose(bool isVerbose);
  /* Do prediction (INPUT) */
  void predict (ROOT::Math::SVector<double,n > U );

  /* Do correction */
  void filter (ROOT::Math::SVector<double,m > Z );

  /* Do smoothing */

  void smoothing ();

};


#endif

