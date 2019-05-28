The program has two parts: server (av-server), and client (av-client).

The server starts a thread pool.
Once a client connects it assigns it to the next thread in the pool.

The client is single-threaded (didn't have time to add multiple threads).
It sends first the list of files to scan, then waits for the server.

The server requests entire file buffers for each file, then scans them and sends the results back to the client. Once all files were scanned, the client is disconnected.

Only the text plugin is implemented, and it takes its sequences from a TXT file, one sequence per line.

The client displays the scan results and exits.

Remaining things to do are in todo.txt


Building and running the thing:

I developed and tested the thing on Mac, but I used ANSI C++, in theory it could work on Linux, too. Probably not on Windows, though.

cd threads-sockets
make

./build/av-server -t 2

./build/av-client -d data1
./build/av-client -d data2


Some source code notes:

The socket code is in common/net.

For exchanging messages via the sockets, Google Protocol Buffers was used (common/proto).

The detection plugins implement the Command design pattern.

The design could be probably improved, had I had enough time.


Some notes on effort spent:

I estimate a total of 24h of effort spent.

I stopped because I just couldn't add more time into this project.


Lines of code statistics:

find src -name '*.cpp' -or -name '*.h' | xargs wc -l
     184 src/av-client/AvClient.cpp
      49 src/av-client/AvClient.h
      60 src/av-client/main-client.cpp
      54 src/av-server/AvServer.cpp
      32 src/av-server/AvServer.h
       9 src/av-server/ClientFileData.cpp
      33 src/av-server/ClientFileData.h
      15 src/av-server/DetectionPlugin.cpp
      24 src/av-server/DetectionPlugin.h
      41 src/av-server/DetectionPluginText.cpp
      32 src/av-server/DetectionPluginText.h
      54 src/av-server/main-server.cpp
      38 src/av-server/ServerCommonThread.cpp
      30 src/av-server/ServerCommonThread.h
      43 src/av-server/ServerDetectionPlugins.cpp
      29 src/av-server/ServerDetectionPlugins.h
     199 src/av-server/ServerWorkerThread.cpp
      59 src/av-server/ServerWorkerThread.h
       9 src/av-server/SocketStatus.cpp
      24 src/av-server/SocketStatus.h
      17 src/common/net/ClientSocket.cpp
      28 src/common/net/ClientSocket.h
      21 src/common/net/defs.h
      43 src/common/net/ServerSocket.cpp
      67 src/common/net/ServerSocket.h
      66 src/common/net/Socket.cpp
      76 src/common/net/Socket.h
       0 src/common/net/SocketAddress.cpp
      51 src/common/net/SocketAddress.h
      96 src/common/net/SocketList.cpp
      39 src/common/net/SocketList.h
      75 src/common/utils/Utils.cpp
      27 src/common/utils/Utils.h
    1624 total

find src -name '*.cpp' -or -name '*.h' | xargs sed '/^\s*$/d' | wc -l
    1328 (non-empty lines)
