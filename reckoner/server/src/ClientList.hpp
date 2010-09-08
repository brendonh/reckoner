#ifndef __RECKONER__CLIENT_LIST
#define __RECKONER__CLIENT_LIST

#include <enet/enet.h>
#include <deque>
#include <map>
#include <limits>

#include "Server.hpp"
#include "Client.hpp"


namespace Reckoner {

  typedef std::map<ClientID, Client*> ClientMap;
  typedef std::map<UserID, Client*> UserMap;

  class ClientList {
  public:

    ClientMap mClients;
    UserMap mUsers;

    ClientList();
    ~ClientList();

    Client* createClient(ENetPeer* p);
    void removeClient(Client* client);

    Client* clientByUser(UserID id);

  };

}

#endif
