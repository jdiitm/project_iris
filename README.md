# Project Iris

A distributed messaging system built in Erlang, designed to demonstrate core concepts of distributed systems: node connectivity, user presence, message routing, and offline storage.

## Architecture

The system consists of two main node types:

1.  **Core Node (`iris_core`)**:
    *   Acts as the central registry for user presence.
    *   Maintains a global user-to-node mapping using **Mnesia** (Distributed Erlang Database).
    *   Handles offline message storage (currently log-based simulation).
    *   **Note**: Uses Mnesia transactions for consistency and replication.

2.  **Edge Nodes (`iris_edge`)**:
    *   Handle TCP connections from clients.
    *   Protocols: Custom binary protocol over TCP.
    *   Forwards login and routing requests to the Core Node.
    *   Delivers messages to connected users.

## Prerequisites

*   **Erlang/OTP**: 24+ (Must include `mnesia` application).
    *   *Debian/Ubuntu*: `sudo apt-get install erlang-mnesia`
*   **Python 3**: For running verification scripts.
*   **Netcat (nc)**: Optional, for manual testing.

## Setup & Compilation

Compile the project using the provided Makefile:

```bash
make clean && make
```

## Running the System

The system uses a `Makefile` to simplify starting nodes with the correct configuration and Erlang binary paths.

### 1. Start Core Node
```bash
make start_core
```
*Wait for "Core DB Initialized" message.*

### 2. Start Edge Node 1 (Port 8085)
```bash
make start_edge1
```
*Wait for "Edge Listener started on port 8085".*

### 3. Start Edge Node 2 (Port 8086)
```bash
make start_edge2
```

*Note*: The `Makefile` includes "smart detection" logic. It will automatically search for an `erl` executable in your path or standard locations (like `/usr/bin/erl`) that supports `mnesia`. If found, it uses that binary potentially overriding your default `erl` to ensure the system runs correctly.

## Client Protocol

The system uses a simple binary protocol (Big-Endian):

1.  **Login**: `<<1, User/binary>>`
2.  **Send Message**: `<<2, TargetLen:16, Target:TargetLen/binary, Msg/binary>>`
3.  **Ack**: `<<3, MsgId/binary>>` (Server -> Client)

## Verification

The project includes Python scripts to verify end-to-end functionality.

### 1. Online Messaging Test (`test_iris.py`)
Simulates two users (Alice and Bob) logging in. Alice sends a message to Bob.
```bash
python3 test_iris.py
```
*Expected Output*: `SUCCESS: Bob received the message.`

### 2. Offline Storage Test (`test_offline.py`)
Simulates Alice sending a message to Charlie (who is offline).
```bash
python3 test_offline.py
```
*Expected Output*: `SUCCESS: Charlie received offline message.`
*Note*: The system now fully supports offline message retrieval. When Charlie logs in, the Edge node queries the Core node for offline messages (stored in `offline_msgs.dets`) and delivers them.

## Recent Improvements
*   **Portability & Autodetection**: The build system now automatically detects a valid Erlang installation (with `mnesia`) and adapts to the machine's hostname. No manual configuration is required.
*   **Dynamic Node Discovery**: Support for variable hostnames (fixes `localhost` hardcoding).
*   **Robustness**: Fixed TCP listener configuration bugs (`badarg` crashes).
*   **Mnesia Adoption**: Replaced ETS with Mnesia for distributed presence and transactional integrity.
