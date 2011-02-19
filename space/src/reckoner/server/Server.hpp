#ifndef __RECKONER_SERVER
#define __RECKONER_SERVER

#include <string>

#include "reckoner/common/ReckonerTypes.hpp"
#include "reckoner/common/Region.hpp"

#include "ClientList.hpp"
#include "PacketQueue.hpp"

namespace Reckoner {
  namespace Server {

    bool initialize();

    typedef unsigned long ClientID;
    typedef std::string UserID;

    class Server {
    public:

      Server();
      virtual ~Server();

      int run();
      
      bool service(int timeout);

      void addObject(Reckoner::Framework::WorldObject& obj);

      void flushQueue();

      bool _shutdown;
      Region mRegion;

    private:
      ENetHost* mHost;
      ClientList mClientList;

      PacketQueue mPacketQueue;

      void disconnectAllClients();

    };
  }
}

#endif
