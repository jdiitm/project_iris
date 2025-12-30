# Project Iris

A distributed messaging system built in Erlang, designed to demonstrate core concepts of distributed systems: node connectivity, user presence, message routing, and offline storage.

## Architecture

The system consists of two main node types:

1.  **Core Node (`iris_core`)**:
    *   Acts as the central registry for user presence.
    *   Maintains a global user-to-node mapping using **Mnesia** (Distributed Erlang Database).
    *   **Offline Storage**: Uses Mnesia `offline_msg` table (`disc_copies`) for persistent, ordered message storage.
    *   **Note**: Uses Mnesia transactions for atomic operations.

2.  **Edge Nodes (`iris_edge`)**:
    *   Handle TCP connections from clients.
    *   Protocols: Custom binary protocol over TCP.
    *   Forwards login and routing requests to the Core Node.
    *   Delivers messages to connected users (including stored offline messages).

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

### Simple Binary Protocol
1.  **Login**: `0x01` | `User` (binary)
    *   Server responds with **ACK**: `0x03` | `"LOGIN_OK"`
2.  **Send Message**: `0x02` | `TargetLen` (16-bit) | `Target` (binary) | `MsgLen` (16-bit) | `Msg` (binary)
3.  **Ack**: `0x03` | `MsgId` (binary)

## Verification

The project includes Python scripts to verify end-to-end functionality.

### 1. Online Messaging Test (`test_iris.py`)
Simulates two users (Alice and Bob) logging in. Alice sends a message to Bob.
```bash
python3 test_iris.py
```
*Expected Output*: `SUCCESS: Bob received the message.`

### 2. Offline Storage Test (`test_offline.py`)
Simulates Alice sending a sequence of messages to Charlie (who is offline).
```bash
./test_offline.py
```
*Expected Output*: `SUCCESS: Received all messages in correct order.`
*Note*: The system stores messages persistently in Mnesia. When Charlie logs in, the Edge node queries the Core node for ordered offline messages and delivers them.

### 3. Performance Benchmark (`benchmark_iris.py`)
Stress tests the system to measure throughput and latency.
```bash
./benchmark_iris.py
```
**Typical Results** (Localhost):
*   **Throughput**: > 1000 msgs/sec
*   **Avg Latency**: < 1ms
*   **P99 Latency**: < 2ms
*   **Internal Routing Latency**: ~160 microseconds (via Telemetry)

## Recent Improvements
*   **Portability & Autodetection**: The build system now automatically detects a valid Erlang installation (with `mnesia`) and adapts to the machine's hostname. No manual configuration is required.
*   **Dynamic Node Discovery**: Support for variable hostnames (fixes `localhost` hardcoding).
*   **Robustness**: Fixed TCP listener configuration bugs (`badarg` crashes).
*   **Mnesia Adoption**: Replaced ETS with Mnesia for distributed presence and transactional integrity.
