#include <enet/enet.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "Reckoner.hpp"

bool running = true;
bool _shutdown = false;
ENetPeer* server;


void forceShutdown(int UNUSED(param)) {
  printf("\rForcing shutdown...\n");
  running = false;
}

void startShutdown(int UNUSED(param)) {
  printf("\rShutting down...\n");
  _shutdown = true;
  signal(SIGINT, forceShutdown);
}


int main () {

  signal(SIGINT, startShutdown);

  if (enet_initialize () != 0) {
    fprintf (stderr, "An error occurred while initializing ENet.\n");
    return EXIT_FAILURE;
  }

  ENetHost* client = enet_host_create(NULL, 1, 2, 0, 0);

  if (client == NULL) {
    fprintf (stderr, "An error occurred while trying to create an ENet client host.\n");
    exit (EXIT_FAILURE);
  }

  ENetAddress address;
  enet_address_set_host(&address, "localhost");
  address.port = 8101;

  ENetPeer* server = enet_host_connect(client, &address, 2, 0);

  if (server == NULL) {
    fprintf (stderr, "Couldn't connect to server\n");
    exit(EXIT_FAILURE);
  }

  ENetEvent event;
  ENetPacket* packet;

  while (running) {

    if (_shutdown) {
      enet_peer_disconnect(server, NULL);
    }

    printf(".");
    fflush(stdout);

    int rv = enet_host_service(client, &event, 1000);

    if (rv == 0) continue;

    if (rv < 0) {
      if (_shutdown) continue;
      if (running) printf("Error listening for events.\n");
      break;
    }

    printf("\n");

    switch (event.type) {
    case ENET_EVENT_TYPE_CONNECT:
      printf ("Connected to server: %x:%u.\n", 
              event.peer -> address.host,
              event.peer -> address.port);

      packet = enet_packet_create("packet", 
                                  strlen("packet") + 1, 
                                  ENET_PACKET_FLAG_RELIABLE);

      enet_packet_resize(packet, strlen("packetfoo") + 1);
      strcpy((char*)&packet->data[strlen("packet")], "foo");

      enet_peer_send(event.peer, 0, packet);

      event.peer->data = (char*)"Client information";

      break;

    case ENET_EVENT_TYPE_RECEIVE:
      printf ("A packet of length %u containing %s was received from %s on channel %u.\n",
              (int)event.packet->dataLength,
              (char*)event.packet->data,
              (char*)event.peer->data,
              event.channelID);

      enet_packet_destroy(event.packet);
            
      break;
           
    case ENET_EVENT_TYPE_DISCONNECT:
      printf ("Disconected.\n");
      event.peer -> data = NULL;

      // Later, try reconnecting
      //if (_shutdown) running = false;
      running = false;

      break;

    default: break;
    }
  }

  enet_host_destroy(client);
  atexit(enet_deinitialize);
}
