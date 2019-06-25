#ifndef SACGeometry_H
#define SACGeometry_H 1

class SACGeometry
{

public:

  SACGeometry();
  static SACGeometry* GetInstance();

private:

  static SACGeometry* fInstance;

private:

  void CreateGeometry();

public:


private:

};
#endif
