#include <iostream>
#include <sstream>

#include "./Connection.hpp"

using namespace Reckoner::Client;

extern bool _shutdown;

Connection::Connection(std::string hostname, int port, 
                       ENetHost& client, ENetPeer& peer) 
  : ENetEndpoint(peer),
    mHost(hostname),
    mPort(port),
    mReady(false),
    mClient(client) {

  std::ostringstream os;
  os << "[" << mHost << ":" << mPort << "]";
  mIdentifier = os.str();
}


Connection::~Connection() {}


Connection* Connection::connect(std::string hostname, int port) {
  ENetHost* client = enet_host_create(NULL, 1, 2, 0, 0);

  if (NULL == client) {
    std::cout << "[CONNECT] Couldn't create ENet client host." << std::endl;
    return NULL;
  }

  ENetAddress address;
  enet_address_set_host(&address, hostname.c_str());
  address.port = 8101;

  ENetPeer* peer = enet_host_connect(client, &address, 2, 0);

  if (NULL == peer) {
    std::cout << "CONNECT] Couldn't connect to server" << std::endl;
    return NULL;
  }

  return new Connection(hostname, port, *client, *peer);

}


bool Connection::service(int timeout) {
  int rv = enet_host_service(&mClient, &mEvent, timeout);

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
    handle(mEvent);
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
  login.set_name("Brendon!!!");
  send(&login, ENET_PACKET_FLAG_RELIABLE);

  ProtoBufs::ControlObject controlObj;
  controlObj.set_objectid(0);
  send(&controlObj, ENET_PACKET_FLAG_RELIABLE);

}


void Connection::disconnected() {
  ENetEndpoint::disconnected();
  LOG("Destroying client");
  enet_host_destroy(&mClient);
}
