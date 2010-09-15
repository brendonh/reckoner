#include <iostream>
#include <sstream>

#include "./Connection.hpp"

using namespace Reckoner::Client;

extern bool _shutdown;

Connection::Connection(std::string host, int port) 
  : ENetEndpoint(NULL),
    mHost(host),
    mPort(port),
    mReady(false),
    mClient(NULL) {

  std::ostringstream os;
  os << "[" << mHost << ":" << mPort << "]";
  mIdentifier = os.str();
}


Connection::~Connection() {}


bool Connection::startConnect() {
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


bool Connection::service(int timeout) {
  int rv = enet_host_service(mClient, &mEvent, timeout);

  if (rv == 0) return true;
  
  if (rv < 0) {
    if (!_shutdown) LOG("Error listening for events.");
    return false;
  }

  switch (mEvent.type) {
  case ENET_EVENT_TYPE_CONNECT:
    connected();
    break;

  case ENET_EVENT_TYPE_RECEIVE:
    handle(&mEvent);
    enet_packet_destroy(mEvent.packet);
    break;
           
  case ENET_EVENT_TYPE_DISCONNECT:
    mEvent.peer -> data = NULL;
    disconnected();
    return false;

  default: break;
  }

  return true;
}

void Connection::connected() {
  LOG("Connected");

  
  ProtoBufs::Login login;
  login.set_name("Brend");
  send(MTYPE_LOGIN, &login, ENET_PACKET_FLAG_RELIABLE);
}


void Connection::disconnected() {
  ENetEndpoint::disconnected();
  LOG("Destroying client");
  enet_host_destroy(mClient);
}


void Connection::handle(const ENetEvent* event) {
  if (event->packet->dataLength < 2) {
    LOG("Invalid message length " << event->packet->dataLength);
    return;
  }

  short messageType = (short)(*event->packet->data);

  if (!mReady) {
    if (messageType == MTYPE_LOGGEDIN) {
      LOG("Logged in!");
      //handleLogin(event);
    } else {
      LOG("Ignoring pre-login message type " << messageType);
      return;
    }
  }
}
