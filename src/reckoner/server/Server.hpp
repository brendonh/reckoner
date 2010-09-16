#ifndef __RECKONER_SERVER
#define __RECKONER_SERVER

#include <string>

#include "reckoner/common/Reckoner.hpp"


namespace Reckoner {
  namespace Server {
    typedef unsigned long ClientID;
    typedef std::string UserID;

    void startShutdown();

    int run();
  }
}

#endif
