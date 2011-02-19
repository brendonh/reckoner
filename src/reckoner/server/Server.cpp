#include <stdio.h>
#include <string>

#include <iostream>

#include "Server.hpp"

#include "UpdateBuilder.hpp"

namespace Reckoner {
  namespace Server {

    bool initialize() {
      if (enet_initialize () != 0) {
        fprintf (stderr, "An error occurred while initializing ENet.\n");
        return false;
      }

      Reckoner::Network::MessageMap::initialize();

      return true;
    }

    Server::Server()
      : _shutdown(false),
        mRegion(),
        mClientList(),
        mPacketQueue() {
      ENetAddress address;

      address.host = ENET_HOST_ANY;
      address.port = 8101;

      mHost = enet_host_create(&address, 32, 2, 0, 0);
    }


    Server::~Server() {
      enet_host_destroy(mHost);
      //enet_deinitialize();
    }


    int Server::run() {

      GOOGLE_PROTOBUF_VERIFY_VERSION;

      if (mHost == NULL) {
        std::cout << "Couldn't start server" << std::endl;
        return EXIT_FAILURE;
      }

      while (!_shutdown) {
        if (!service(1000)) break;
        mRegion.tick();
        flushQueue();
      }

      disconnectAllClients();
      enet_host_flush(mHost);
  
      return 0;
    }

    bool Server::service(int timeout) {
      Client *client;
      ENetEvent event;    

      int rv = enet_host_service(mHost, &event, timeout);

      if (rv == 0) return true;

      if (rv < 0) {
        if (!_shutdown) std::cout << "Error listening for events" << std::endl;
        return false;
      }

      switch (event.type) {
      case ENET_EVENT_TYPE_CONNECT:
        client = mClientList.createClient(*event.peer);
        if (client == NULL) {
          std::cout << "Couldn't create client (out of IDs?)\n" << std::endl;
          enet_peer_disconnect(event.peer, 0);
        }
        // Yes, even if it failed.
        event.peer->data = client;
        break;

      case ENET_EVENT_TYPE_RECEIVE:
        client = (Client*)event.peer->data;
        client->handle(event);
        enet_packet_destroy(event.packet);            
        break;
           
      case ENET_EVENT_TYPE_DISCONNECT:
        client = (Client*)event.peer->data;
        if (client == NULL) {
          std::cout << "Uninitiated client disconnected" << std::endl;
        } else {
          mClientList.removeClient(client);
        }
        event.peer -> data = NULL;
        break;

      default: break;
      }

      return true;
    }

    void Server::flushQueue() {
      //mRegion.queuePackets(mPacketQueue);
      mPacketQueue.sendTo(mClientList);
    }

    void Server::addObject(Reckoner::Framework::WorldObject& obj) {
      mRegion.addObject(obj);
    }

    void Server::disconnectAllClients() {
      ClientMap clients = mClientList.mClients;
      ClientMap::iterator it;

      for(it = clients.begin(); it != clients.end(); ++it) {
        Client* client = it->second;
        client->startDisconnect();
        mClientList.removeClient(client);
      }
    }

  }
}
