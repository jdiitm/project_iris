import socket
import sys
import struct

def check_epmd(host, port=4369):
    print(f"Checking EPMD on {host}:{port}...")
    try:
        s = socket.create_connection((host, port), timeout=5)
        # EPMD_NAMES_REQ = 110 (ascii 'n')
        # Packet format: Length(2 bytes), Command(1 byte)
        # \x00\x01\x6e
        s.sendall(b'\x00\x01n')
        
        # Read response
        resp = b""
        while True:
            chunk = s.recv(4096)
            if not chunk: break
            resp += chunk
        s.close()
        
        # Parse response
        # First 4 bytes = Port No (for EPMD itself? or number of bytes? documentation varies, usually ignore first 4 for names response)
        # Actually EPMD response to NAMES_REQ is: port_no(4 bytes) + names(N bytes)
        
        if len(resp) < 4:
            print("Response too short.")
            return

        print(f"EPMD Response length: {len(resp)} bytes")
        raw_output = resp[4:].decode('utf-8', errors='ignore')
        print(f"Registered nodes:\n{raw_output}")
        
        # Parse for iris_core1
        target_node = "iris_core1"
        target_port = None
        
        for line in raw_output.split('\n'):
            if f"name {target_node} at port" in line:
                parts = line.split()
                # line format: "name <name> at port <port>"
                try:
                    port_idx = parts.index("port") + 1
                    target_port = int(parts[port_idx])
                    print(f"\nFOUND {target_node} on port {target_port}")
                except (ValueError, IndexError):
                    pass
        
        if target_port:
            check_node_port(host, target_port)
        else:
            print(f"\nNode {target_node} NOT FOUND in EPMD registration.")

    except Exception as e:
        print(f"FAILED to query EPMD: {e}")

def check_node_port(host, port):
    print(f"Checking connectivity to node port {host}:{port}...")
    try:
        s = socket.create_connection((host, port), timeout=5)
        print("SUCCESS: Connection to node port established.")
        s.close()
    except Exception as e:
        print(f"FAILED to connect to node port: {e}")
        print("POSSIBLE CAUSE: Firewall is blocking the random Erlang distribution port.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 debug_epmd.py <hostname>")
        sys.exit(1)
    
    check_epmd(sys.argv[1])
