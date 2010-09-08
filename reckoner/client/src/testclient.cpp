#include <stdio.h>
#include <iostream>
#include <string.h>
#include <signal.h>

#include "Client.hpp"

using namespace Reckoner::Client;

bool running = true;
bool _shutdown = false;

void forceShutdown(int UNUSED(param)) {
  printf("\rForcing shutdown...\n");
  running = false;
}

void startShutdown(int UNUSED(param)) {
  _shutdown = true;
  signal(SIGINT, forceShutdown);
  std::cout << "\r" << std::flush;
}

int main () {

  signal(SIGINT, startShutdown);

  if (enet_initialize () != 0) {
    fprintf (stderr, "An error occurred while initializing ENet.\n");
    return EXIT_FAILURE;
  }

  Connection* conn = new Connection();

  if (!conn->startConnect("localhost", 8101)) {
    std::cout << "Abandoning!" << std::endl;
    return EXIT_FAILURE;
  }

  while (running && !conn->mDisconnected) {
    if (_shutdown) conn->startDisconnect();
    conn->service(1000);
  }

  atexit(enet_deinitialize);
}
