#include "ClientList.hpp"

using namespace Reckoner::Server;

inline ClientID makeClientID(ENetAddress addr) {
  ClientID id = (ClientID)addr.host;
  id <<= 32;
  id |= addr.port;
  return id;
}


ClientList::ClientList() {}

ClientList::~ClientList() {}

Client* ClientList::createClient(ENetPeer& p) {
  ClientID id = makeClientID(p.address);
  if (0 == id) return NULL;
  Client* client = new Client(id, p);
  mClients[id] = client;
  return client;
}
  
void ClientList::removeClient(Client* client) {
  ClientID id = client->mClientID;
  if (client->mUserID != "") mUsers.erase(client->mUserID);
  mClients.erase(id);
  delete client;
}

Client* ClientList::clientByUser(UserID uid) {
  UserMap::const_iterator it = mUsers.find(uid);
  if (it == mUsers.end()) return NULL;
  return it->second;
}

Client* ClientList::clientByID(ClientID uid) {
  ClientMap::const_iterator it = mClients.find(uid);
  if (it == mClients.end()) return NULL;
  return it->second;
}
