#ifndef Beam_h
#define Beam_h 1
#include <string>
using namespace std;

struct TURTLEint_t {
  int NPART;
};

struct TURTLEdouble_t {
  double XPOS, YPOS, ZPOS, XPMOM, YPMOM, ZPMOM;
};

struct TURTLEchar_t {
  char FILENAME[100];
};

class FastBeam {
public:
  explicit FastBeam();
  ~FastBeam();

  void Generate();   
  int GetN() {
    /// \MemberDescr
    /// \return Number of generated beam particles per event. 
    /// \EndMemberDescr
    return npart;
  }
  double GetPx(int j) {
    /// \MemberDescr
    /// \return Px of the generated particles. 
    /// \EndMemberDescr
    return pmomx[j];
  }
  double GetPy(int j) {
    /// \MemberDescr
    /// \return Py of the generated particles. 
    /// \EndMemberDescr
    return pmomy[j];
  }
  double GetPz(int j) {
    /// \MemberDescr
    /// \return Pz of the generated particles. 
    /// \EndMemberDescr
    return pmomz[j];
  }
  double GetX(int j) {
    /// \MemberDescr
    /// \return X of the impact point position fo the generated particles on the last beam-line element. 
    /// \EndMemberDescr
    return posx[j];
  }
  double GetY(int j) {
    /// \MemberDescr
    /// \return Y of the impact point position fo the generated particles on the last beam-line element. 
    /// \EndMemberDescr
    return posy[j];
  }
  double GetZ(int j) {
    /// \MemberDescr
    /// \return Z of the impact point position fo the generated particles on the last beam-line element. 
    /// \EndMemberDescr
    return posz[j];
  }
  double GetTime(int j) {
    /// \MemberDescr
    /// \return Time of the generated particles at the last beam-line element. 
    /// \EndMemberDescr
    return ttime[j];
  }
  int GetId(int j) {
    /// \MemberDescr
    /// \return Id of the generated particles. 
    /// \EndMemberDescr
    return type[j];
  }
  void SetN(int n) { npart=n; }
 
private:
  void Reset_OutputVar();
  void Fill_OutputVar();
  int npart,type[10];
  double pmomx[10],pmomy[10],pmomz[10],posx[10],posy[10],posz[10],ttime[10];

  TURTLEint_t*    fTURTLEint;
  TURTLEdouble_t* fTURTLEdouble;
  TURTLEchar_t*   fTURTLEchar;
};

#endif
