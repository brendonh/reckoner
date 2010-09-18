#ifndef __RECKONER_CLIENT
#define __RECKONER_CLIENT

#include <string>

#include "reckoner/common/ReckonerTypes.hpp"
#include "reckoner/common/ENetEndpoint.hpp"

namespace Reckoner {
  namespace Server {
    class Client : public Reckoner::Network::ENetEndpoint {
    public:

      ClientID mClientID;
      UserID mUserID;

      bool mReady;

      Client(ClientID id, ENetPeer* p);
      ~Client();

      //void handle(const ENetEvent& event);
      //void handleLogin(const ENetEvent* event);

    };
  }
}

#endif
