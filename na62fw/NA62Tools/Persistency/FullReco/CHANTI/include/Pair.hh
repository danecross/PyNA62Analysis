//
// Created by Vito Palladino 19.3.2009
//

#ifndef Pair_H
#define Pair_H 1


#include <iostream>
#include <vector>


class Pair{

public:

  Pair();
  Pair(int ValueX, int ValueY);
  //bool &operator==(const Pair);
  
  void Clear(Option_t* = "");

  void SetXY(int ValueX, int ValueY);

  int GetX()   { return X; };
  int GetY()   { return Y; };

private:

  int X, Y; 

};

#endif
