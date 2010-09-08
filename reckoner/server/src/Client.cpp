#include <iostream>
#include <sstream>

#include "Client.hpp"
#include "ClientList.hpp"

using namespace Reckoner;

extern ClientList* clientList;

Client::Client(ClientID id, ENetPeer* p) {
  mClientID = id;
  mPeer = p;

  mUserID = "";
  mReady = false;
  
  mMessageBufferSize = 1024;
  mMessageBuffer = (char*)malloc(mMessageBufferSize);

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
    LOG("Invalid message length");
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

  // std::string msg = std::string((char*)event->packet->data);
  // std::string response = std::string("Got: ") + msg;

  // ENetPacket *packet = enet_packet_create(response.c_str(), 
  //                                         response.length() + 1, 
  //                                         ENET_PACKET_FLAG_RELIABLE);

  // enet_peer_send(peer, 0, packet);
}


void Client::handleLogin(const ENetEvent* event) {
  ProtoBufs::Login login;
  if (!extractWireBuf(&login, event)) {
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
  send(MTYPE_LOGIN, &loggedIn, ENET_PACKET_FLAG_RELIABLE);

}

