#ifndef __RECKONER__CLIENT_LIST
#define __RECKONER__CLIENT_LIST

#include <enet/enet.h>
#include <unistd.h>
#include <stdio.h>
#include <string>
#include <deque>
#include <map>
#include <limits>

#include "reckoner.hpp"
#include "client.hpp"


namespace Reckoner {

  typedef std::map<ClientID, Client*> ClientMap;

  class ClientList {
  public:

    ClientMap clients;

    ClientList();
    ~ClientList();

    Client* createClient(ENetPeer* p);
    void removeClient(Client* client);

  };

}

#endif
