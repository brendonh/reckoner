#include "client.hpp"

Reckoner::Client::Client(ClientID id, ENetPeer* p) : clientID(id), peer(p) {
  log("Connected");
}

Reckoner::Client::~Client() {
  log("Disconnected");
}

void Reckoner::Client::handle(const ENetEvent* event) {
  log(std::string("Received: ") + std::string((char*)event->packet->data));

  std::string msg = std::string((char*)event->packet->data);
  std::string response = std::string("Got: ") + msg;

  ENetPacket *packet = enet_packet_create(response.c_str(), 
                                          response.length() + 1, 
                                          ENET_PACKET_FLAG_RELIABLE);

  enet_peer_send(peer, 0, packet);
}

void Reckoner::Client::log(std::string msg) {
  printf("<%lx (%x:%u)> %s\n", clientID, peer->address.host, peer->address.port, msg.c_str());
}
