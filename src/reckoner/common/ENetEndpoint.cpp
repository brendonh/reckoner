#include <iostream>
#include <functional>

#include "./ENetEndpoint.hpp"

using namespace Reckoner::Network;

ENetEndpoint::ENetEndpoint(ENetPeer& peer) 
  : mPeer(peer), 
    mIdentifier("[UNKNOWN]"),
    mMessageBufferSize(sDefaultBufferSize),
    mMessageHandlers(),
    mDisconnecting(false),
    mDisconnected(false) {

  if (!sInitialized) initialize();

  peer.data = this;

  mMessageBuffer = static_cast<char*>(malloc(mMessageBufferSize));
}

ENetEndpoint::~ENetEndpoint() {}


void ENetEndpoint::startDisconnect() {
  LOG("Disconnecting...");
  mDisconnecting = true;
  enet_peer_disconnect(&mPeer, 0);
}


void ENetEndpoint::disconnected() {
  LOG("Disconnected.");
  mDisconnected = true;
}


void ENetEndpoint::send(google::protobuf::MessageLite* message, enet_uint32 flags) {
  uint32_t messageType = sMessageIDMap[message->GetTypeName()];
  send(messageType, message, flags);
}

void ENetEndpoint::send(uint32_t messageType, 
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
  
  enet_peer_send(&mPeer, 0, packet);
}


void ENetEndpoint::registerHandler(std::string messageName, messageCallback_t handler) {
  auto it = sMessageIDMap.find(messageName);
  if (it == sMessageIDMap.end()) {
    std::cout << "Message name not found: " << messageName << std::endl;
    return;
  }
  uint32_t i = it->second;
  if (i >= mMessageHandlers.size()) {
    mMessageHandlers.resize(i+1, NULL);
  }
  mMessageHandlers[i] = handler;
}

void ENetEndpoint::handle(const ENetEvent& event) {
  //LOG("Received " << event.packet->dataLength << " bytes");
  if (event.packet->dataLength < 2) {
    LOG("Invalid message length " << event.packet->dataLength);
    return;
  }

  unsigned short messageType = (short)(*event.packet->data);

  char* data = (char*)event.packet->data + MESSAGE_TYPE_PREFIX_LENGTH;

  google::protobuf::MessageLite* message = sDispatchMap[messageType]->New();
  if (!message->ParseFromArray(data, event.packet->dataLength - MESSAGE_TYPE_PREFIX_LENGTH)) {
    LOG("PARSING FAILED: " << messageType << " :: " << message->GetTypeName());
    delete message;
    return;
  }

  if (messageType < mMessageHandlers.size()) {
    auto handler = mMessageHandlers[messageType];
    if (NULL != handler) {
      handler(*this, *message);
      return;
    }
  }

  if (messageType >= sStaticMessageHandlers.size()) {
    sDefaultMessageHandler(*this, *message);
    return;
  }

  auto handler = sStaticMessageHandlers[messageType];
  handler(*this, *message);
}

// --------------------------------
// Static
// --------------------------------

std::vector< google::protobuf::MessageLite* > ENetEndpoint::sDispatchMap;
std::unordered_map< std::string, int > ENetEndpoint::sMessageIDMap;
std::vector< ENetEndpoint::messageCallback_t > ENetEndpoint::sStaticMessageHandlers;

bool ENetEndpoint::sInitialized = false;
void ENetEndpoint::initialize() {
  mapMessageClass(new Reckoner::ProtoBufs::Login());
  mapMessageClass(new Reckoner::ProtoBufs::LoggedIn());
  sInitialized = true;
}

void ENetEndpoint::dumpMessageMap() { 
  int i=0;
  for (auto it = sDispatchMap.begin(); it != sDispatchMap.end(); ++it) {
    std::cout << i << " => " << (*it)->GetTypeName() << std::endl;
    ++i;
  }
}



void ENetEndpoint::registerStaticHandler(std::string messageName, messageCallback_t handler) {
  auto it = sMessageIDMap.find(messageName);
  if (it == sMessageIDMap.end()) {
    std::cout << "Message name not found: " << messageName << std::endl;
    return;
  }
  uint32_t i = it->second;
  if (i >= sStaticMessageHandlers.size()) {
    sStaticMessageHandlers.resize(i+1, &ENetEndpoint::sDefaultMessageHandler);
  }
  sStaticMessageHandlers[i] = handler;
}


void ENetEndpoint::sDefaultMessageHandler(ENetEndpoint& endpoint,
                                          const google::protobuf::MessageLite& message) {
  std::cout << endpoint.getIdentifier() << " No handler for: " 
            << message.GetTypeName() << std::endl;
}
