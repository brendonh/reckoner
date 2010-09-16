#include <stdio.h>
#include <string>
#include <signal.h>

#include <iostream>

#include "Server.hpp"

#include "reckoner/common/Region.hpp"
#include "reckoner/proto/login.pb.h"

#include "ClientList.hpp"

namespace Reckoner {
  namespace Server {
    bool _shutdown = false;
    ClientList* clientList;

    void startShutdown() {
      std::cout << "\rShutting down..." << std::endl;
      _shutdown = true;
    }


    ENetHost* startServer() {
      if (enet_initialize () != 0) {
        fprintf (stderr, "An error occurred while initializing ENet.\n");
        return NULL;
      }

      ENetAddress address;
      ENetHost * server;

      address.host = ENET_HOST_ANY;
      address.port = 8101;

      server = enet_host_create(&address, 32, 2, 0, 0);

      return server;
    }

    int run() {

      GOOGLE_PROTOBUF_VERIFY_VERSION;

      //signal (SIGINT, startShutdown);

      ENetHost* server = startServer();

      if (server == NULL) {
        std::cout << "Couldn't start server" << std::endl;
        return EXIT_FAILURE;
      }

      clientList = new ClientList();

      ENetEvent event;    
      Client *client;

      Reckoner::Region region;

      while (!_shutdown) {

        region.tick();

        int rv = enet_host_service(server, &event, 100);

        if (rv == 0) continue;

        if (rv < 0) {
          if (!_shutdown) std::cout << "Error listening for events" << std::endl;
          break;
        }

        switch (event.type) {
        case ENET_EVENT_TYPE_CONNECT:
          client = clientList->createClient(event.peer);
          if (client == NULL) {
            std::cout << "Couldn't create client (out of IDs?)\n" << std::endl;
            enet_peer_disconnect(event.peer, NULL);
          }
          // Yes, even if it failed.
          event.peer->data = client;
          break;

        case ENET_EVENT_TYPE_RECEIVE:
          client = (Client*)event.peer->data;
          client->handle(&event);
          enet_packet_destroy(event.packet);            
          break;
           
        case ENET_EVENT_TYPE_DISCONNECT:
          client = (Client*)event.peer->data;
          if (client == NULL) {
            std::cout << "Uninitiated client disconnected" << std::endl;
          } else {
            clientList->removeClient(client);
          }
          event.peer -> data = NULL;
          break;

        default: break;
        }

      }

      ClientMap clients = clientList->mClients;
      ClientMap::iterator it;

      for(it = clients.begin(); it != clients.end(); ++it) {
        client = it->second;
        client->startDisconnect();
        clientList->removeClient(client);
      }

      enet_host_flush(server);
  
      enet_host_destroy(server);
      atexit(enet_deinitialize);

      return 0;
    }

  }
}
