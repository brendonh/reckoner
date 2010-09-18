#pragma once
#ifndef __RECKONER_CLIENT_CLIENT
#define __RECKONER_CLIENT_CLIENT

#include <string>

#include "reckoner/common/Reckoner.hpp"
#include "reckoner/common/ENetEndpoint.hpp"


namespace Reckoner {
  namespace Client {
    
    class Connection : public Reckoner::Network::ENetEndpoint {
    public:

      Connection(std::string host, int port);
      ~Connection();

      bool startConnect();
      bool service(int timeout);

      void connected();

      virtual void disconnected();

      inline bool isDisconnected() { return mDisconnected; }

      void handle(const ENetEvent* event);

      std::string mHost;
      int mPort;

      bool mReady;

      ENetHost* mClient;
      ENetEvent mEvent;


    };

  }
}

#endif
