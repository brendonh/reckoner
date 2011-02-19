#ifndef __RECKONER_SERVER_PACKETQUEUE
#define __RECKONER_SERVER_PACKETQUEUE

#include <vector>
#include <algorithm>

#include "reckoner/common/ReckonerTypes.hpp"
#include "reckoner/common/ENetEndpoint.hpp"
#include "reckoner/server/ClientList.hpp"

namespace Reckoner {
  namespace Server {

    class PacketQueueEntry {
    public:

      PacketQueueEntry(ClientID cid, const Reckoner::Network::ENetPacketBuffer* buff) 
        : clientID(cid), packetBuffer(buff) {}

      ClientID clientID;
      const Reckoner::Network::ENetPacketBuffer* packetBuffer;

    };

    class PacketQueue {
    public:
      PacketQueue() : mQueue() {}

      void queue(ClientID clientID, const Reckoner::Network::ENetPacketBuffer* buff) {
        mQueue.push_back(PacketQueueEntry(clientID, buff));
      }

      void sendTo(ClientList& clientList) {
        ClientID prevID = 0;
        Client* client;

        std::cout << "Queue length: " << mQueue.size() << std::endl;

        std::sort(mQueue.begin(), mQueue.end(), compareEntries);

        for(auto it = mQueue.begin(); it != mQueue.end(); ++it) {
          ClientID thisID = it->clientID;

          if (thisID != prevID) {
            client = clientList.clientByID(thisID);
            prevID = thisID;
          }

          if (NULL == client) continue;

          std::cout << "PACKET" << std::endl;
          client->send(*it->packetBuffer);
        }

        mQueue.clear();

      }
        
    private:
      std::vector<PacketQueueEntry> mQueue;

      static bool compareEntries(const PacketQueueEntry& pqe1,
                                 const PacketQueueEntry& pqe2) {
        return pqe1.clientID < pqe2.clientID;
      }
      
    };

  }
}

#endif
