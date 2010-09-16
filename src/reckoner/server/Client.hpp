#ifndef __RECKONER_CLIENT
#define __RECKONER_CLIENT

#include <string>

#include "reckoner/common/Reckoner.hpp"
#include "reckoner/common/ENetEndpoint.hpp"

#include "Server.hpp"

namespace Reckoner {
  namespace Server {
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
}

#endif
