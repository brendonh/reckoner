#include "clientlist.hpp"

namespace Reckoner {

  ClientList::ClientList() {}

  ClientList::~ClientList() {}

  Client* ClientList::createClient(ENetPeer* p) {
    ClientID id = makeClientID(p->address);
    if (0 == id) return NULL;
    Client* client = new Client(id, p);
    clients[id] = client;
    return client;
  }
  
  void ClientList::removeClient(Client* client) {
    ClientID id = client->clientID;
    clients.erase(id);
    delete client;
  }

}
