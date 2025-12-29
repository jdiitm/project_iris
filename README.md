# Project Iris

A distributed messaging system built in Erlang, designed to demonstrate core concepts of distributed systems: node connectivity, user presence, message routing, and offline storage.

## Architecture

The system consists of two main node types:

1.  **Core Node (`iris_core`)**:
    *   Acts as the central registry for user presence.
    *   Maintains a global user-to-node mapping using **ETS** (Erlang Term Storage).
    *   Handles offline message storage (currently log-based simulation).
    *   **Note**: Originally designed for Mnesia, refactored to ETS for portability (running without system-level Mnesia dependency).

2.  **Edge Nodes (`iris_edge`)**:
    *   Handle TCP connections from clients.
    *   Protocols: Custom binary protocol over TCP.
    *   Forwards login and routing requests to the Core Node.
    *   Delivers messages to connected users.

## Prerequisites

*   **Erlang/OTP**: 24+ (Tested with current massive installation).
*   **Python 3**: For running verification scripts.
*   **Netcat (nc)**: Optional, for manual testing.

## Setup & Compilation

Compile the project using the provided Makefile:

```bash
make clean && make
```

## Running the System

Start the Core Node and at least one Edge Node in separate terminals. The system is configured to dynamically resolve node names based on your hostname.

### 1. Start Core Node
```bash
erl -sname iris_core -pa ebin -eval "application:ensure_all_started(iris_core)."
```
*Wait for "Core DB (ETS) Initialized" message.*

### 2. Start Edge Node
```bash
erl -sname iris_edge1 -pa ebin -iris_edge port 8085 -eval "application:ensure_all_started(iris_edge)."
```
*Wait for "Edge Listener started on port 8085".*

**Note**: The default port is **8085** (changed from 8000 to avoid conflicts).

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
*Expected Output*: `SUCCESS: Charlie received offline message.` (Note: Currently verifies routing to storage; delivery logic is a placeholder).

## Recent Improvements
*   **Dynamic Node Discovery**: Support for variable hostnames (fixes `localhost` hardcoding).
*   **Robustness**: Fixed TCP listener configuration bugs (`badarg` crashes).
*   **Dependency Removal**: Replaced Mnesia with ETS for lighter-weight deployment.
