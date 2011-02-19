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

      Connection(std::string hostname, int port, ENetHost& client, ENetPeer& peer);
      ~Connection();

      bool service(int timeout);

      void connected();

      virtual void disconnected();

      inline bool isDisconnected() { return mDisconnected; }

      std::string mHost;
      int mPort;

      bool mReady;

      ENetHost& mClient;
      ENetEvent mEvent;

      static Connection* connect(std::string hostname, int port);


    };

  }
}

#endif
