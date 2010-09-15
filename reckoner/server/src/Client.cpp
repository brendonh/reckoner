#include <iostream>
#include <sstream>

#include "Client.hpp"
#include "ClientList.hpp"

using namespace Reckoner;

extern ClientList* clientList;

Client::Client(ClientID id, ENetPeer* peer) 
  : ENetEndpoint(peer),
    mClientID(id),
    mUserID("NOUSER"),
    mReady(false) {

  char ip[20];
  enet_address_get_host_ip(&mPeer->address, ip, 20);
  std::ostringstream os;
  os << "<" << ip << ":" << mPeer->address.port << ">";
  mIdentifier = os.str();

  LOG("Connected");
}

Client::~Client() {
  LOG("Disconnected");
}


void Client::handle(const ENetEvent* event) {
  LOG("Received " << event->packet->dataLength << " bytes");

  if (event->packet->dataLength < 2) {
    LOG("Invalid message length " << event->packet->dataLength);
    return;
  }

  short messageType = (short)(*event->packet->data);

  if (!mReady) {
    if (messageType == MTYPE_LOGIN) {
      LOG("Login message received!");
      handleLogin(event);
    } else {
      LOG("Ignoring pre-login message type " << messageType);
      return;
    }
  }
}


void Client::handleLogin(const ENetEvent* event) {
  ProtoBufs::Login login;
  if (!extractMessage(login, event)) {
    LOG("INVALID LOGIN MESSAGE");
    enet_peer_disconnect(event->peer, NULL);
    return;
  }
  std::string name = login.name();
  if (NULL != clientList->clientByUser(name)) {
    LOG("User exists: " << name);
    enet_peer_disconnect(event->peer, NULL);
    return;
  }
  
  mUserID = name;
  clientList->mUsers[name] = this;

  LOG("Login: " << name);
  
  ProtoBufs::LoggedIn loggedIn;
  send(MTYPE_LOGGEDIN, &loggedIn, ENET_PACKET_FLAG_RELIABLE);

}

