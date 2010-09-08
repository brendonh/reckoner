#ifndef __RECKONER_CLIENT
#define __RECKONER_CLIENT

#include <string>

#include "Reckoner.hpp"
#include "Server.hpp"

#include "ENetEndpoint.hpp"

namespace Reckoner {

  class Client : public ENetEndpoint {
  public:

    ClientID mClientID;
    UserID mUserID;

    bool mReady;

    Client(ClientID id, ENetPeer* p);
    ~Client();

    void handle(const ENetEvent* event);
    void handleLogin(const ENetEvent* event);

  };

}

#endif
