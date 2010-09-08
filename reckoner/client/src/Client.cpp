#include <iostream>
#include <sstream>

#include "./Client.hpp"

using namespace Reckoner::Client;

extern bool _shutdown;

Connection::Connection() {
  mDisconnecting = false;
  mDisconnected = false;
  mMessageBufferSize = 1024;
  mMessageBuffer = (char*)malloc(mMessageBufferSize);
}


Connection::~Connection() {}


bool Connection::startConnect(std::string h, int p) {
  mHost = h;
  mPort = p;

  std::ostringstream os;
  os << "[" << mHost << ":" << mPort << "]";
  mIdentifier = os.str();

  mClient = enet_host_create(NULL, 1, 2, 0, 0);

  if (mClient == NULL) {
    LOG("Couldn't create ENet client host.");
    return false;
  }

  ENetAddress address;
  enet_address_set_host(&address, mHost.c_str());
  address.port = 8101;

  mPeer = enet_host_connect(mClient, &address, 2, 0);

  if (mPeer == NULL) {
    LOG("Couldn't connect to server");
    return false;
  }

  return true;
}


void Connection::service(int timeout) {
  int rv = enet_host_service(mClient, &mEvent, timeout);

  if (rv == 0) return;
  
  if (rv < 0) {
    if (_shutdown) return;
    LOG("Error listening for events.");
    return;
  }

  switch (mEvent.type) {
  case ENET_EVENT_TYPE_CONNECT:
    connected();
    break;

  case ENET_EVENT_TYPE_RECEIVE:
    // printf ("A packet of length %u containing %s was received from %s on channel %u.\n",
    //         (int)event.packet->dataLength,
    //         (char*)event.packet->data,
    //         (char*)event.peer->data,
    //         event.channelID);

    enet_packet_destroy(mEvent.packet);
            
    break;
           
  case ENET_EVENT_TYPE_DISCONNECT:
    mEvent.peer -> data = NULL;
    disconnected();
    break;

  default: break;
  }
}

void Connection::connected() {
  LOG("Connected");

  ProtoBufs::Login login;
  login.set_name("Brend");
  send(MTYPE_LOGIN, &login, ENET_PACKET_FLAG_RELIABLE);
}


void Connection::startDisconnect() {
  LOG("Disconnecting...");
  mDisconnecting = true;
  enet_peer_disconnect(mPeer, NULL);
}


void Connection::disconnected() {
  LOG("Disconnected.");
  mDisconnected = true;
  enet_host_destroy(mClient);
}

