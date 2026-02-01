#!/bin/bash
# scripts/tune_for_1m.sh
#
# Linux kernel tuning for 1M+ concurrent connections
# 
# This script configures system parameters required to handle
# 1 million concurrent TCP connections on a single machine.
#
# REQUIREMENTS:
#   - Root privileges (sudo)
#   - Linux kernel 4.x or later
#   - Minimum 64GB RAM recommended
#   - SSD storage recommended
#
# USAGE:
#   sudo ./scripts/tune_for_1m.sh [--apply] [--check] [--revert]
#
#   --apply   Apply all tuning parameters (requires root)
#   --check   Check current settings vs recommended
#   --revert  Revert to conservative defaults
#
# REFERENCE:
#   - RFC NFR-6: Message durability 99.999%
#   - RFC NFR-10: Support 5B+ users globally (1M per region)

set -euo pipefail

# =============================================================================
# Configuration
# =============================================================================

# Target connections
TARGET_CONNECTIONS=1200000  # 1.2M to have buffer

# File descriptor limits
FILE_MAX=2000000
NOFILE_SOFT=1500000
NOFILE_HARD=1500000

# TCP parameters
TCP_MAX_SYN_BACKLOG=65535
SOMAXCONN=65535
TCP_TW_REUSE=1
IP_LOCAL_PORT_RANGE="1024 65535"
NETDEV_MAX_BACKLOG=65535

# Memory parameters (bytes)
RMEM_MAX=16777216       # 16MB
WMEM_MAX=16777216       # 16MB
RMEM_DEFAULT=262144     # 256KB
WMEM_DEFAULT=262144     # 256KB
TCP_RMEM="4096 262144 16777216"
TCP_WMEM="4096 262144 16777216"

# Keep-alive parameters
TCP_KEEPALIVE_TIME=300      # 5 minutes
TCP_KEEPALIVE_INTVL=30      # 30 seconds
TCP_KEEPALIVE_PROBES=5      # 5 probes

# VM parameters
VM_SWAPPINESS=10            # Reduce swapping
VM_DIRTY_RATIO=15           # % of RAM for dirty pages
VM_DIRTY_BG_RATIO=5         # % of RAM for background writeback

# =============================================================================
# Helper Functions
# =============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "[INFO] $1"
}

log_ok() {
    echo -e "[${GREEN}OK${NC}] $1"
}

log_warn() {
    echo -e "[${YELLOW}WARN${NC}] $1"
}

log_error() {
    echo -e "[${RED}ERROR${NC}] $1"
}

check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root (sudo)"
        exit 1
    fi
}

get_sysctl() {
    sysctl -n "$1" 2>/dev/null || echo "N/A"
}

check_param() {
    local param=$1
    local expected=$2
    local current
    current=$(get_sysctl "$param")
    
    if [[ "$current" == "$expected" ]]; then
        log_ok "$param = $current"
        return 0
    else
        log_warn "$param = $current (expected: $expected)"
        return 1
    fi
}

# =============================================================================
# Check Current Settings
# =============================================================================

do_check() {
    echo "=============================================="
    echo "  System Tuning Check for 1M Connections"
    echo "=============================================="
    echo ""
    
    local issues=0
    
    # System info
    log_info "System: $(uname -r)"
    log_info "CPUs: $(nproc)"
    log_info "RAM: $(free -h | awk '/^Mem:/{print $2}')"
    echo ""
    
    # File descriptors
    echo "--- File Descriptors ---"
    check_param "fs.file-max" "$FILE_MAX" || ((issues++))
    
    current_nofile=$(ulimit -n)
    if [[ "$current_nofile" -ge "$NOFILE_SOFT" ]]; then
        log_ok "ulimit -n = $current_nofile"
    else
        log_warn "ulimit -n = $current_nofile (expected: >= $NOFILE_SOFT)"
        ((issues++))
    fi
    echo ""
    
    # TCP parameters
    echo "--- TCP Parameters ---"
    check_param "net.core.somaxconn" "$SOMAXCONN" || ((issues++))
    check_param "net.ipv4.tcp_max_syn_backlog" "$TCP_MAX_SYN_BACKLOG" || ((issues++))
    check_param "net.ipv4.tcp_tw_reuse" "$TCP_TW_REUSE" || ((issues++))
    check_param "net.ipv4.ip_local_port_range" "$IP_LOCAL_PORT_RANGE" || ((issues++))
    check_param "net.core.netdev_max_backlog" "$NETDEV_MAX_BACKLOG" || ((issues++))
    echo ""
    
    # Memory parameters
    echo "--- Memory Parameters ---"
    check_param "net.core.rmem_max" "$RMEM_MAX" || ((issues++))
    check_param "net.core.wmem_max" "$WMEM_MAX" || ((issues++))
    check_param "vm.swappiness" "$VM_SWAPPINESS" || ((issues++))
    echo ""
    
    # Summary
    echo "=============================================="
    if [[ $issues -eq 0 ]]; then
        log_ok "All parameters are correctly configured"
        echo ""
        echo "System is ready for 1M connections test."
        return 0
    else
        log_warn "$issues parameter(s) need adjustment"
        echo ""
        echo "Run 'sudo $0 --apply' to apply recommended settings."
        return 1
    fi
}

# =============================================================================
# Apply Settings
# =============================================================================

do_apply() {
    check_root
    
    echo "=============================================="
    echo "  Applying System Tuning for 1M Connections"
    echo "=============================================="
    echo ""
    
    # Backup current settings
    BACKUP_FILE="/tmp/sysctl_backup_$(date +%Y%m%d_%H%M%S).conf"
    log_info "Backing up current settings to $BACKUP_FILE"
    sysctl -a > "$BACKUP_FILE" 2>/dev/null || true
    
    # --- File Descriptors ---
    log_info "Configuring file descriptors..."
    
    # fs.file-max
    sysctl -w fs.file-max=$FILE_MAX
    
    # Persistent limits.conf
    if ! grep -q "^\* soft nofile" /etc/security/limits.conf 2>/dev/null; then
        echo "* soft nofile $NOFILE_SOFT" >> /etc/security/limits.conf
        echo "* hard nofile $NOFILE_HARD" >> /etc/security/limits.conf
        echo "root soft nofile $NOFILE_SOFT" >> /etc/security/limits.conf
        echo "root hard nofile $NOFILE_HARD" >> /etc/security/limits.conf
        log_ok "Added file descriptor limits to /etc/security/limits.conf"
    else
        log_info "limits.conf already has nofile entries"
    fi
    
    # --- TCP Parameters ---
    log_info "Configuring TCP parameters..."
    
    sysctl -w net.core.somaxconn=$SOMAXCONN
    sysctl -w net.ipv4.tcp_max_syn_backlog=$TCP_MAX_SYN_BACKLOG
    sysctl -w net.ipv4.tcp_tw_reuse=$TCP_TW_REUSE
    sysctl -w net.ipv4.ip_local_port_range="$IP_LOCAL_PORT_RANGE"
    sysctl -w net.core.netdev_max_backlog=$NETDEV_MAX_BACKLOG
    
    # --- Memory Parameters ---
    log_info "Configuring memory parameters..."
    
    sysctl -w net.core.rmem_max=$RMEM_MAX
    sysctl -w net.core.wmem_max=$WMEM_MAX
    sysctl -w net.core.rmem_default=$RMEM_DEFAULT
    sysctl -w net.core.wmem_default=$WMEM_DEFAULT
    sysctl -w net.ipv4.tcp_rmem="$TCP_RMEM"
    sysctl -w net.ipv4.tcp_wmem="$TCP_WMEM"
    
    # --- Keep-alive ---
    log_info "Configuring TCP keep-alive..."
    
    sysctl -w net.ipv4.tcp_keepalive_time=$TCP_KEEPALIVE_TIME
    sysctl -w net.ipv4.tcp_keepalive_intvl=$TCP_KEEPALIVE_INTVL
    sysctl -w net.ipv4.tcp_keepalive_probes=$TCP_KEEPALIVE_PROBES
    
    # --- VM Parameters ---
    log_info "Configuring VM parameters..."
    
    sysctl -w vm.swappiness=$VM_SWAPPINESS
    sysctl -w vm.dirty_ratio=$VM_DIRTY_RATIO
    sysctl -w vm.dirty_background_ratio=$VM_DIRTY_BG_RATIO
    
    # --- Persist to sysctl.conf ---
    log_info "Persisting settings to /etc/sysctl.d/99-iris-1m.conf..."
    
    cat > /etc/sysctl.d/99-iris-1m.conf << EOF
# Iris Project: 1M Connections Tuning
# Generated by tune_for_1m.sh on $(date)

# File descriptors
fs.file-max = $FILE_MAX

# TCP parameters
net.core.somaxconn = $SOMAXCONN
net.ipv4.tcp_max_syn_backlog = $TCP_MAX_SYN_BACKLOG
net.ipv4.tcp_tw_reuse = $TCP_TW_REUSE
net.ipv4.ip_local_port_range = $IP_LOCAL_PORT_RANGE
net.core.netdev_max_backlog = $NETDEV_MAX_BACKLOG

# Memory parameters
net.core.rmem_max = $RMEM_MAX
net.core.wmem_max = $WMEM_MAX
net.core.rmem_default = $RMEM_DEFAULT
net.core.wmem_default = $WMEM_DEFAULT
net.ipv4.tcp_rmem = $TCP_RMEM
net.ipv4.tcp_wmem = $TCP_WMEM

# TCP keep-alive
net.ipv4.tcp_keepalive_time = $TCP_KEEPALIVE_TIME
net.ipv4.tcp_keepalive_intvl = $TCP_KEEPALIVE_INTVL
net.ipv4.tcp_keepalive_probes = $TCP_KEEPALIVE_PROBES

# VM parameters
vm.swappiness = $VM_SWAPPINESS
vm.dirty_ratio = $VM_DIRTY_RATIO
vm.dirty_background_ratio = $VM_DIRTY_BG_RATIO
EOF
    
    log_ok "Settings persisted"
    
    echo ""
    echo "=============================================="
    log_ok "System tuning applied successfully!"
    echo ""
    echo "IMPORTANT: You must log out and back in (or reboot)"
    echo "for ulimit changes to take effect in new sessions."
    echo ""
    echo "To verify: $0 --check"
    echo ""
    
    # Print Erlang VM flags for 1M
    echo "Recommended Erlang VM flags for 1M connections:"
    echo "  erl +P 1500000 +Q 1500000 +K true +S 32:32 +SDcpu 32:32 +SDio 64"
    echo ""
}

# =============================================================================
# Revert Settings
# =============================================================================

do_revert() {
    check_root
    
    echo "=============================================="
    echo "  Reverting to Conservative Defaults"
    echo "=============================================="
    echo ""
    
    log_info "Removing Iris sysctl config..."
    rm -f /etc/sysctl.d/99-iris-1m.conf
    
    log_info "Applying conservative defaults..."
    
    # Reset to typical defaults
    sysctl -w fs.file-max=1048576
    sysctl -w net.core.somaxconn=4096
    sysctl -w net.ipv4.tcp_max_syn_backlog=4096
    sysctl -w net.ipv4.tcp_tw_reuse=0
    sysctl -w net.ipv4.ip_local_port_range="32768 60999"
    sysctl -w net.core.netdev_max_backlog=1000
    sysctl -w net.core.rmem_max=212992
    sysctl -w net.core.wmem_max=212992
    sysctl -w vm.swappiness=60
    
    log_ok "Reverted to conservative defaults"
    echo ""
    echo "Note: ulimit changes in /etc/security/limits.conf were NOT reverted."
    echo "Edit /etc/security/limits.conf manually if needed."
    echo ""
}

# =============================================================================
# Print Erlang Recommendations
# =============================================================================

print_erlang_flags() {
    echo ""
    echo "=============================================="
    echo "  Erlang VM Flags for 1M Connections"
    echo "=============================================="
    echo ""
    
    CPU_CORES=$(nproc 2>/dev/null || echo 32)
    DIRTY_IO=$((CPU_CORES * 2))
    
    cat << EOF
For 1M concurrent connections, start Erlang with:

  erl \\
    +P 1500000 \\
    +Q 1500000 \\
    +K true \\
    +S $CPU_CORES:$CPU_CORES \\
    +SDcpu $CPU_CORES:$CPU_CORES \\
    +SDio $DIRTY_IO \\
    +stbt ts \\
    +sbwt none \\
    +swt low \\
    +A $((CPU_CORES * 2)) \\
    +MBas aoffcbf \\
    +MHas aoffcbf \\
    +MMmcs 30

Or use scripts/auto_tune.sh to generate optimal flags automatically:

  ERL_FLAGS=\$(./scripts/auto_tune.sh)
  erl \$ERL_FLAGS -pa ebin ...

Key flags explained:
  +P 1500000     Max processes (connections + internal)
  +Q 1500000     Max ports (file descriptors)
  +K true        Enable kernel poll (epoll)
  +S N:N         Schedulers (= CPU cores)
  +SDcpu N:N     Dirty CPU schedulers
  +SDio M        Dirty IO schedulers (2x CPU cores)
  +stbt ts       Thread spread for NUMA
  +A N           Async thread pool size

EOF
}

# =============================================================================
# Main
# =============================================================================

usage() {
    echo "Usage: $0 [--apply|--check|--revert|--erlang]"
    echo ""
    echo "Options:"
    echo "  --apply   Apply all tuning parameters (requires root)"
    echo "  --check   Check current settings vs recommended"
    echo "  --revert  Revert to conservative defaults (requires root)"
    echo "  --erlang  Print recommended Erlang VM flags"
    echo ""
    echo "Example:"
    echo "  sudo $0 --apply    # Apply tuning"
    echo "  $0 --check         # Check current settings"
    echo ""
}

main() {
    case "${1:-}" in
        --apply)
            do_apply
            ;;
        --check)
            do_check
            ;;
        --revert)
            do_revert
            ;;
        --erlang)
            print_erlang_flags
            ;;
        --help|-h)
            usage
            ;;
        "")
            # Default: check
            do_check
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
}

main "$@"
