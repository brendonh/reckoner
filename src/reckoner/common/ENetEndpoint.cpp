#include <iostream>

#include "./ENetEndpoint.hpp"

using namespace Reckoner;

ENetEndpoint::ENetEndpoint(ENetPeer* peer) 
  : mPeer(peer), 
    mIdentifier("[UNKNOWN]"),
    mMessageBufferSize(sDefaultBufferSize),
    mDisconnecting(false),
    mDisconnected(false) {

  mMessageBuffer = (char*)malloc(mMessageBufferSize);
}

ENetEndpoint::~ENetEndpoint() {}


void ENetEndpoint::startDisconnect() {
  LOG("Disconnecting...");
  mDisconnecting = true;
  enet_peer_disconnect(mPeer, NULL);
}


void ENetEndpoint::disconnected() {
  LOG("Disconnected.");
  mDisconnected = true;
}


void ENetEndpoint::send(message_type messageType, 
                        google::protobuf::MessageLite* message, 
                        enet_uint32 flags) {

  int size = message->ByteSize() + MESSAGE_TYPE_PREFIX_LENGTH;

  if (size > mMessageBufferSize) {
    LOG("Reallocating buffer from " << mMessageBufferSize
        << " to " << size);
    mMessageBufferSize = size;
    mMessageBuffer = static_cast<char*>
      (realloc(mMessageBuffer, mMessageBufferSize));
  }

  memcpy(mMessageBuffer, &messageType, MESSAGE_TYPE_PREFIX_LENGTH);

  if (!message->SerializeToArray(mMessageBuffer + MESSAGE_TYPE_PREFIX_LENGTH, size)) {
    LOG("Message serialization failed!");
    return;
  }

  ENetPacket* packet = enet_packet_create(mMessageBuffer, size, flags);
  
  enet_peer_send(mPeer, 0, packet);
}


bool ENetEndpoint::extractMessage(::google::protobuf::MessageLite& message, 
                                  const ENetEvent* event) {

  char* data = (char*)event->packet->data + MESSAGE_TYPE_PREFIX_LENGTH;
  return message.ParseFromArray(data, event->packet->dataLength - MESSAGE_TYPE_PREFIX_LENGTH);
}
