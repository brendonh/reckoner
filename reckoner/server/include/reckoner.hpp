#ifndef __RECKONER_RECKONER
#define __RECKONER_RECKONER

namespace Reckoner {
  typedef unsigned long ClientID;

  inline ClientID makeClientID(ENetAddress addr) {
    ClientID id = (ClientID)addr.host;
    id <<= 32;
    id |= addr.port;
    return id;
  }

}

#endif
