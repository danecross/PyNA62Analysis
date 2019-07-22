#include "rndmcpp.hh"

void rndmcpp_(double *number) {
  *number = RandomGenerator::GetInstance()->GetRndmDecay();
}

void ranluxcpp_(int *n, double *numbers) {
  RandomGenerator::GetInstance()->GetArrayDecay(*n,numbers);
}
