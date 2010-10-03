#include <iostream>
#include <functional>

#include "./ENetEndpoint.hpp"

using namespace Reckoner::Network;

ENetEndpoint::ENetEndpoint(ENetPeer& peer) 
  : mPeer(peer), 
    mIdentifier("[UNKNOWN]"),
    mMessageHandlers(),
    mImmediateBuffer(),
    mDisconnecting(false),
    mDisconnected(false) {

  if (!MessageMap::sInitialized) MessageMap::initialize();

  peer.data = this;
}

ENetEndpoint::~ENetEndpoint() {}


bool ENetEndpoint::send(google::protobuf::MessageLite* message, 
                        enet_uint32 flags) {
  if (!mImmediateBuffer.buffer(message, flags)) return false;
  send(mImmediateBuffer);
  return true;
}

bool ENetEndpoint::send(uint32_t messageType, 
                        google::protobuf::MessageLite* message, 
                        enet_uint32 flags) {
  if (!mImmediateBuffer.buffer(messageType, message, flags)) return false;
  send(mImmediateBuffer);
  return true;
}

void ENetEndpoint::registerHandler(std::string messageName, messageCallback_t handler) {
  auto it = MessageMap::sMessageIDMap.find(messageName);
  if (it == MessageMap::sMessageIDMap.end()) {
    LOG("registerHandler: Message name not found: " << messageName);
    return;
  }
  uint32_t i = it->second;
  if (i >= mMessageHandlers.size()) {
    mMessageHandlers.resize(i+1, NULL);
  }
  mMessageHandlers[i] = handler;
}

void ENetEndpoint::handle(const ENetEvent& event) {

  if (event.packet->dataLength < 2) {
    LOG("Invalid message length " << event.packet->dataLength);
    return;
  }

  unsigned short messageType = (short)(*event.packet->data);

  if (messageType >= MessageMap::sDispatchMap.size()) {
    LOG("handle: Ignoring invalid message type " << messageType);
    return;
  }

  google::protobuf::MessageLite* message = MessageMap::sDispatchMap[messageType]->New();

  char* data = (char*)event.packet->data + MESSAGE_TYPE_PREFIX_LENGTH;

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


void ENetEndpoint::startDisconnect() {
  LOG("Disconnecting...");
  mDisconnecting = true;
  enet_peer_disconnect(&mPeer, 0);
}


void ENetEndpoint::disconnected() {
  LOG("Disconnected.");
  mDisconnected = true;
}


// --------------------------------
// Static
// --------------------------------

std::vector< ENetEndpoint::messageCallback_t > ENetEndpoint::sStaticMessageHandlers;

void ENetEndpoint::registerStaticHandler(std::string messageName, messageCallback_t handler) {
  auto it = MessageMap::sMessageIDMap.find(messageName);
  if (it == MessageMap::sMessageIDMap.end()) {
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


// --------------------------------
// Message Map
// --------------------------------

bool MessageMap::sInitialized;
std::vector< google::protobuf::MessageLite* > MessageMap::sDispatchMap;
std::unordered_map< std::string, int > MessageMap::sMessageIDMap;

void MessageMap::initialize() {
  mapMessageClass(new Reckoner::ProtoBufs::Login());
  mapMessageClass(new Reckoner::ProtoBufs::LoggedIn());
  mapMessageClass(new Reckoner::ProtoBufs::ControlObject());
  sInitialized = true;
}

void MessageMap::mapMessageClass(google::protobuf::MessageLite* message) {
  sMessageIDMap[message->GetTypeName()] = sDispatchMap.size();
  sDispatchMap.push_back(message);
}

void MessageMap::dumpMessageMap() { 
  int i=0;
  for (auto it = sDispatchMap.begin(); it != sDispatchMap.end(); ++it) {
    std::cout << i << " => " << (*it)->GetTypeName() << std::endl;
    ++i;
  }
}


// --------------------------------
// Packet Buffer
// --------------------------------

ENetPacketBuffer::ENetPacketBuffer()
  : mBufferSize(sDefaultBufferSize) {
  mBuffer = static_cast<char*>(malloc(mBufferSize));
}

bool ENetPacketBuffer::buffer(google::protobuf::MessageLite* message, 
                              enet_uint32 flags) {

  auto it = MessageMap::sMessageIDMap.find(message->GetTypeName());
  if (it == MessageMap::sMessageIDMap.end()) {
    std::cout << "[Buffer]: Message name not found: " << message->GetTypeName() << std::endl;
    return false;
  }
  uint32_t messageType = it->second;
  return buffer(messageType, message, flags);
}

ENetPacketBuffer::~ENetPacketBuffer() {
  delete[] mBuffer;
}

bool ENetPacketBuffer::buffer(uint32_t messageType, 
                              google::protobuf::MessageLite* message, 
                              enet_uint32 flags) {

  mMessageSize = message->ByteSize() + MESSAGE_TYPE_PREFIX_LENGTH;

  if (mMessageSize > mBufferSize) {
    std::cout << "[Buffer]: Reallocating buffer from " << mBufferSize
              << " to " << mMessageSize << std::endl;
    mBufferSize = mMessageSize;
    mBuffer = static_cast<char*>(realloc(mBuffer, mBufferSize));
  }

  memcpy(mBuffer, &messageType, MESSAGE_TYPE_PREFIX_LENGTH);

  if (!message->SerializeToArray(mBuffer + MESSAGE_TYPE_PREFIX_LENGTH, 
                                 mMessageSize)) {
    std::cout << "[Buffer]: Message serialization failed! " 
              << message->GetTypeName() << std::endl;
    return false;
  }

  mFlags = flags;

  return true;
}

void ENetPacketBuffer::sendTo(ENetPeer& peer) const {
  ENetPacket* packet = enet_packet_create(mBuffer, mMessageSize, mFlags);
  enet_peer_send(&peer, 0, packet);
}
