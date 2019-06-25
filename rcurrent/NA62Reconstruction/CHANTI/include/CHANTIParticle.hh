//
// Vito Palladino 16/7/2009
//
#ifndef CHANTIParticle_H
#define CHANTIParticle_H 1

class Particle{

public:

  Particle(int pID) { fEnergy = 0; 
                               Particle::SetID(pID);  }

  void SetID(int value)     { fID = value;    }

  void SetEnergy(double value)   { fEnergy = value;  }
  void AddEnergy(double value)   { fEnergy += value; }
  
  double GetEnergy()        { return fEnergy;   } 

  int GetID()               { return fID;       }
  
  // Operators
  bool operator==(Particle B)
  {
    if( fID==B.GetID() )
      return 1;
    else 
      return 0;
  }
  /*
  Particle operator+(Particle B)
  {
    this.Add( B.GetEnergy() );
    return ;
  }
  */

private:

  int fID;
  double fEnergy;

};

#endif
