#pragma once
#ifndef __RECKONER_CLIENT_CLIENT
#define __RECKONER_CLIENT_CLIENT

#include <string>

#include "Reckoner.hpp"
#include "ENetEndpoint.hpp"


namespace Reckoner {
  namespace Client {
    
    class Connection : public ENetEndpoint {
    public:

      Connection();
      ~Connection();

      bool startConnect(std::string h, int p);
      void service(int timeout);

      void connected();

      void startDisconnect();
      void disconnected();

      std::string mIdentifier;
      std::string mHost;
      int mPort;

      bool mDisconnecting;
      bool mDisconnected;

      ENetHost* mClient;
      ENetEvent mEvent;


    };

  }
}

#endif
