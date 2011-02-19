#include <iostream>
#include <sstream>

#include "Client.hpp"
#include "ClientList.hpp"

namespace Reckoner{
  namespace Server {

    extern ClientList* clientList;

    Client::Client(ClientID id, ENetPeer& peer) 
      : ENetEndpoint(peer),
        mClientID(id),
        mUserID("NOUSER"),
        mReady(false) {

      char ip[20];
      enet_address_get_host_ip(&mPeer.address, ip, 20);
      std::ostringstream os;
      os << "<" << ip << ":" << mPeer.address.port << ">";
      mIdentifier = os.str();

      LOG("Connected");
    }

    Client::~Client() {
      LOG("Disconnected");
    }
  }
}
