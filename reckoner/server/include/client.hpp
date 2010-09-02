#ifndef __RECKONER_CLIENT
#define __RECKONER_CLIENT

#include <enet/enet.h>
#include <unistd.h>
#include <stdio.h>
#include <string>

#include "reckoner.hpp"

namespace Reckoner {

  class Client {
  public:

    unsigned long clientID;
    ENetPeer* peer;

    Client(ClientID id, ENetPeer* p);
    ~Client();

    void handle(const ENetEvent* event);

    void log(std::string msg);

  };

}

#endif
