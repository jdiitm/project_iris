# Hybrid Cloud Cluster Setup Guide (Project Iris)

This guide details how to deploy a **Hybrid Cloud Cluster** for Project Iris, consisting of:
*   **Core Tier**: 2 x Linux Mint Laptops (Localhost/Home Network).
*   **Edge Tier**: Public Cloud Instances (AWS/GCP/Azure Free Tier).

> [!IMPORTANT]
> **Networking Challenge**: Cloud VMs cannot connect to your home laptops directly due to NAT/Firewalls.
> **Solution**: We will use a Mesh VPN (**Tailscale** or **ZeroTier**) to flatten the network. This is the simplest, most robust way to achieve connectivity without port forwarding or public IPs.

## 1. Architecture

| Role | Node Name | Location | Function |
| :--- | :--- | :--- | :--- |
| **Core 1** | `iris_core1@laptop-a` | Laptop A (Home) | Mnesia Master, User Registry |
| **Core 2** | `iris_core2@laptop-b` | Laptop B (Home) | Mnesia Replica (High Availability) |
| **Edge 1** | `iris_edge1@cloud-vm` | Cloud VM (Public) | Client Gateway 1 |
| **Edge 2** | `iris_edge2@cloud-vm` | Cloud VM (Public) | Client Gateway 2 |

## 2. Prerequisites (All Nodes)

### 2.1 Install Dependencies
On **Laptops** and **Cloud VMs**:
```bash
# Ubuntu/Debian/Mint
sudo apt-get update
sudo apt-get install git make erlang-base erlang-mnesia erlang-runtime-tools erlang-crypto erlang-public-key erlang-ssl erlang-inets python3 curl
```

### 2.2 Install Tailscale (The "Magic" Network)
This connects your Cloud VMs and Laptops into a private, secure mesh network.
```bash
curl -fsSL https://tailscale.com/install.sh | sh
sudo tailscale up
```
*   Authenticate all machines to the same Tailscale account.
*   Note the **Tailscale IP addresses** (Starts with `100.x.y.z`) for each machine.

## 3. Configuration

### 3.1 Clone & Build
On all nodes:
```bash
git clone https://github.com/your-repo/project_iris.git
cd project_iris
make clean && make
```

### 3.2 Hosts File (DNS Resolution)
Edit `/etc/hosts` on **ALL** machines (Laptops & VMs). Add the Tailscale IPs:
```text
# Tailscale Mesh IPs
100.10.20.1    laptop-a
100.10.20.2    laptop-b
100.50.60.1    cloud-edge-1
```

## 4. Start the Cluster

### 4.1 Start Core Tier (Local Laptops)

**Laptop A (Core 1):**
```bash
# Start master node
erl -name iris_core1@laptop-a -setcookie iris_secret -pa ebin -eval "application:ensure_all_started(iris_core)"
```

**Laptop B (Core 2):**
```bash
# Start replica node and join cluster
erl -name iris_core2@laptop-b -setcookie iris_secret -pa ebin -eval "application:ensure_all_started(iris_core)"
```
*(In the Erlang shell of Core 2, connect to Core 1)*:
```erlang
net_adm:ping('iris_core1@laptop-a').
% Expected: pong
```

### 4.2 Start Edge Tier (Cloud VMs)

**Cloud VM (Edge 1):**
```bash
# Start Edge and point to BOTH Cores
erl -name iris_edge1@cloud-edge-1 -setcookie iris_secret -pa ebin \
    -eval "application:ensure_all_started(iris_edge)" \
    -iris_core_nodes "['iris_core1@laptop-a', 'iris_core2@laptop-b']"
```

## 5. Verification

### 5.1 Verify Connectivity from Cloud
On the Cloud VM, check if it sees the laptops:
```erlang
nodes().
% Expected: ['iris_core1@laptop-a', 'iris_core2@laptop-b']
```

### 5.2 Verify Latency
Run a ping test from Cloud VM to Laptop:
```bash
ping laptop-a
# If latency is >100ms, expect higher end-to-end messaging latency.
# Tailscale is usually very efficient (WireGuard based).
```

## 6. Running the Stress Test
To certify this hybrid cluster, run the verification suite from the **Cloud VM** (as it simulates external clients):

```bash
# On Cloud VM
python3 verify_all.py
```
This will generate traffic from the cloud, routing through your home laptops, proving true Hybrid Cloud capability!
