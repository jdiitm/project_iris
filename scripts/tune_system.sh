#!/bin/bash
echo "--- TUNING MACOS FOR PROJECT IRIS (EXTREME SCALE) ---"
echo "Requires Sudo access for sysctl and launchctl"

# 1. Kernel Level Limits (sysctl)
echo "[*] Setting Kernel Limits..."
sudo sysctl -w kern.maxfiles=2000000
sudo sysctl -w kern.maxfilesperproc=1000000
sudo sysctl -w kern.ipc.somaxconn=10000

# 2. Launchd Limits (Global/Shell)
echo "[*] Setting Launchctl Limits..."
sudo launchctl limit maxfiles 1000000 1000000

# 3. Verify
echo "[*] Verification:"
sysctl kern.maxfiles kern.maxfilesperproc
ulimit -n

echo "--- TUNING COMPLETE ---"
echo "NOTE: specific shell sessions may still need 'ulimit -n 1000000'"
