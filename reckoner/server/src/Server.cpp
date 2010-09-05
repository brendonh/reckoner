#include <enet/enet.h>
#include <stdio.h>
#include <string>
#include <signal.h>

#include "Reckoner.hpp"
#include "Server.hpp"

#include "ClientList.hpp"

using namespace Reckoner;

bool _shutdown = false;

void startShutdown(int UNUSED(param)) {
  printf("\rShutting down...\n");
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

int main () {
  signal (SIGINT, startShutdown);

  ENetHost* server = startServer();

  if (server == NULL) {
    printf("Couldn't start server\n");
    return EXIT_FAILURE;
  }

  Reckoner::ClientList *clientList = new ClientList();

  ENetEvent event;    
  Reckoner::Client *client;

  while (!_shutdown) {

    int rv = enet_host_service(server, &event, -1);

    if (rv == 0) continue;

    if (rv < 0) {
      if (!_shutdown) printf("Error listening for events\n");
      break;
    }

    switch (event.type) {
    case ENET_EVENT_TYPE_CONNECT:
      client = clientList->createClient(event.peer);
      if (client == NULL) {
        printf("Couldn't create client (out of IDs?)\n");
        enet_peer_disconnect(event.peer, NULL);
      }
      // Yes, even if it failed.
      event.peer->data = client;
      break;

    case ENET_EVENT_TYPE_RECEIVE:
      client = (Reckoner::Client*)event.peer->data;
      client->handle(&event);
      enet_packet_destroy(event.packet);            
      break;
           
    case ENET_EVENT_TYPE_DISCONNECT:
      client = (Reckoner::Client*)event.peer->data;
      if (client == NULL) {
        printf("Uninitiated client disconnected\n");
      } else {
        clientList->removeClient(client);
      }
      event.peer -> data = NULL;
      break;

    default: break;
    }

  }

  ClientMap clients = clientList->clients;
  ClientMap::iterator it;

  for(it = clients.begin(); it != clients.end(); ++it) {
    client = it->second;
    enet_peer_disconnect(client->peer, NULL);
    clientList->removeClient(client);
  }

  enet_host_flush(server);
  
  enet_host_destroy(server);
  atexit(enet_deinitialize);
}
