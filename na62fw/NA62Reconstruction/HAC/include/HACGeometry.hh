#ifndef HACGeometry_H
#define HACGeometry_H 1

class HACGeometry
{

public:

  HACGeometry();
  static HACGeometry* GetInstance();

private:

  static HACGeometry* fInstance;

private:

  void CreateGeometry();

public:


private:

};
#endif
