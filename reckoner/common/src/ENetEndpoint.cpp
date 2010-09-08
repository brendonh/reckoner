#include <iostream>

#include "./ENetEndpoint.hpp"

using namespace Reckoner;

void ENetEndpoint::send(message_type t, google::protobuf::MessageLite* message, enet_uint32 flags) {
  int size = message->ByteSize() + MESSAGE_TYPE_PREFIX_LENGTH;

  if (size > mMessageBufferSize) {
    LOG("Reallocating buffer from " << mMessageBufferSize
        << " to " << size);
    
    free(mMessageBuffer);
    mMessageBufferSize = size;
    mMessageBuffer = (char*)malloc(mMessageBufferSize);
  }
    
  memcpy(mMessageBuffer, &t, MESSAGE_TYPE_PREFIX_LENGTH);

  if (!message->SerializeToArray(mMessageBuffer + MESSAGE_TYPE_PREFIX_LENGTH, size)) {
    LOG("Message serialization failed!");
    return;
  }

  ENetPacket* packet = enet_packet_create(mMessageBuffer, size, flags);
  
  enet_peer_send(mPeer, 0, packet);
}


bool ENetEndpoint::extractWireBuf(::google::protobuf::MessageLite* pb, const ENetEvent* event) {
  char* data = (char*)event->packet->data + MESSAGE_TYPE_PREFIX_LENGTH;
  return pb->ParseFromArray(data, event->packet->dataLength - MESSAGE_TYPE_PREFIX_LENGTH);
}
