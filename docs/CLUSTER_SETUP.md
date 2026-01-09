# Multi-Node Cluster Setup Guide

This guide details how to turn two independent Linux Mint laptops into a unified **Project Iris** cluster.

## Architecture
*   **Laptop A (Core Node)**: Runs `iris_core` (Mnesia Master, Presence DB).
*   **Laptop B (Edge Node)**: Runs `iris_edge` (Accepts Client Connections).

## Prerequisites (Both Machines)
1.  **OS**: Linux Mint 22.1 (Verified).
2.  **Dependencies**:
    ```bash
    sudo apt-get update
    sudo apt-get install git make erlang-base erlang-mnesia erlang-runtime-tools erlang-crypto erlang-public-key erlang-ssl erlang-inets python3
    ```
3.  **Codebase**:
    ```bash
    git clone https://github.com/your-repo/project_iris.git
    cd project_iris
    make clean && make
    ```

## Step 1: Networking & Hostnames

Erlang nodes communicate using **Hostnames**. You must ensure both machines can resolve each other by name.

### 1.1 Identify IP Addresses
Run `ip addr` on both machines to find their LAN IPs (e.g., `192.168.1.10` and `192.168.1.11`).

### 1.2 Configure `/etc/hosts` (On BOTH Machines)
Edit the hosts file:
```bash
sudo nano /etc/hosts
```
Add entries for **both** machines. Example:
```text
127.0.0.1       localhost
192.168.1.10    laptop-core    # Machine A
192.168.1.11    laptop-edge    # Machine B
```
*Tip: Use meaningful names like `laptop-core` and `laptop-edge`.*

### 1.3 Verify Connectivity
From `laptop-edge`, ping `laptop-core`:
```bash
ping laptop-core
```
*(If this fails, check your Wi-Fi/Switch).*

## Step 2: The Magic Cookie 🍪

Erlang nodes will only talk to nodes that share the exact same "Cookie" (a shared secret string).

### 2.1 Set the Cookie
You can set this via command line flags, or by creating a `.erlang.cookie` file.
**We will use the command line flag method for simplicity in the Makefile.**

*No action needed here, we will pass `-setcookie secret_iris_cookie` when starting.*

## Step 3: Start the Cluster

### 3.1 Start Core (Machine A)
On `laptop-core`:
```bash
# Start Core with specific name and cookie
erl -name iris_core@laptop-core -setcookie iris_secret -pa ebin -s iris_app
```
*Or if using our Makefile (you may need to edit it to accept custom names, or just use raw command):*
```bash
make start_core
# WARN: The default Makefile uses '-sname' (short name). 
# For multi-machine, we usually need '-name' (long name) with IP/FQDN 
# OR ensure 'sname' works because domains match.
# Recommendation: Use raw 'erl' command above for first test.
```

### 3.2 Start Edge (Machine B)
On `laptop-edge`:
```bash
# Start Edge and tell it where Core is
erl -name iris_edge1@laptop-edge -setcookie iris_secret -pa ebin -eval "application:ensure_all_started(iris)" -iris_core_node iris_core@laptop-core
```

## Step 4: Verification

### 4.1 Check Connection (From Core)
In the Erlang shell on `laptop-core`:
```erlang
nodes().
% Expected: ['iris_edge1@laptop-edge']
```

### 4.2 Verify Mnesia (From Edge)
In the Erlang shell on `laptop-edge`:
```erlang
mnesia:system_info(running_db_nodes).
% Expected: ['iris_core@laptop-core', 'iris_edge1@laptop-edge']
```

## Troubleshooting
*   **Firewall**: Linux Mint has `ufw` enabled by default.
    *   Allow traffic: `sudo ufw allow 4369/tcp` (EPMD) and `sudo ufw allow 9000:9100/tcp` (Erlang Distribution range).
    *   Or for testing: `sudo ufw disable`.
*   **Cookie Mismatch**: Ensure `-setcookie` string is IDENTICAL.
