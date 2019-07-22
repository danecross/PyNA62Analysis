#ifndef IRCGeometry_H
#define IRCGeometry_H 1

class IRCGeometry
{

public:

  IRCGeometry();
  static IRCGeometry* GetInstance();

private:

  static IRCGeometry* fInstance;

private:

  void CreateGeometry();

public:


private:

};
#endif
