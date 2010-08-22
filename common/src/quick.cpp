#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <errno.h>
#include <iostream>

#include <ev++.h>

#define LISTEN_PORT 8101
#define MAXPENDING 10
#define BUFF_SIZE 1024

ev_io stdin_watcher;
ev_io accept_watcher;
ev_signal signal_watcher;

char client_buffer[BUFF_SIZE];

void error(const std::string msg) {
  perror(msg.c_str());
  exit(1);
}


static void stdin_cb(EV_P_ ev_io *w, int revents) {
  std::string line;

  std::getline(std::cin, line);

  if (line == "quit") {
    printf("Exiting...\n");
    ev_io_stop(EV_A_ w);
    ev_unloop(EV_A_ EVUNLOOP_ALL);
    return;
  } else if (line.length()) {
    printf("Unknown command: %s\n", line.c_str());
  }

  if (std::cin.eof()) {
    printf("End of input, exiting...\n");
    ev_io_stop(EV_A_ w);
    ev_unloop(EV_A_ EVUNLOOP_ALL);
    return;
  }

}

static void signal_cb(EV_P_ ev_signal *w, int revents) {
  printf("\rCaught signal, exiting...\n");
  ev_signal_stop(EV_A_ w);
  ev_unloop(EV_A_ EVUNLOOP_ALL);
}


static void client_cb(EV_P_ ev_io *w, int revents) {
  int *sock = (int*)w->data;
  int received = -1;

  do {
    received = recv(*sock, client_buffer, BUFF_SIZE, 0);
    if (received == -1) continue;

    if (received) {
      client_buffer[received] = '\0';
      printf("Got client message (%d): %s\n", received, client_buffer);
    } else {
      printf("Client died!\n");
      ev_io_stop(EV_A_ w);
    }

  } while(errno != EAGAIN);

}

static void accept_cb(EV_P_ ev_io *w, int revents) {
  int *sock = (int*)malloc(sizeof(int));
  struct sockaddr_in addr;
  unsigned int size = sizeof(addr);
  int *acceptsock = (int*)w->data;
  if ((*sock = accept(*acceptsock, (struct sockaddr *)&addr, &size)) < 0) {
    error("Failed to accept client connection");
  }
  
  fcntl(*sock, F_SETFL, O_NONBLOCK);

  printf("Client connected: %s\n", inet_ntoa(addr.sin_addr));

  ev_io *watcher = (ev_io*)malloc(sizeof(ev_io));
  watcher->data = sock;
  ev_io_init(watcher, client_cb, *sock, EV_READ);
  ev_io_start(loop, watcher);

}


static void prepare_basic_watchers(struct ev_loop* loop) {
  ev_io_init(&stdin_watcher, stdin_cb, /*STDIN_FILENO*/ 0, EV_READ);
  ev_io_start(loop, &stdin_watcher);

  ev_signal_init(&signal_watcher, signal_cb, SIGINT);
  ev_signal_start(loop, &signal_watcher);
}

static void prepare_accept_socket(struct ev_loop* loop) {
  int *acceptsock = (int*)malloc(sizeof(int));
  *acceptsock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (acceptsock < 0) error("Failed to create accept socket");

  int val = 1;
  setsockopt(*acceptsock, SOL_SOCKET, SO_REUSEADDR, &val, NULL);

  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(LISTEN_PORT);

  if (bind(*acceptsock, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
    error("Failed to bind the server socket");
  }

  if (listen(*acceptsock, MAXPENDING) < 0) {
    error("Failed to listen on server socket");
  }

  accept_watcher.data = acceptsock;
  ev_io_init(&accept_watcher, accept_cb, *acceptsock, EV_READ);
  ev_io_start(loop, &accept_watcher);

}


int main() {
  struct ev_loop* loop;
  if (!(loop = ev_default_loop(0))) {
    printf("Could not initialise libev, bad $LIBEV_FLAGS in environment?\n");
    return 1;
  }

  prepare_basic_watchers(loop);
  prepare_accept_socket(loop);
 
  ev_loop(loop, 0);

  printf("Done\n");
  return 0;
}
