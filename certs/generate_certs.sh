#!/bin/bash
# =============================================================================
# mTLS Certificate Generator for Project Iris
# =============================================================================
# Generates CA, server, and client certificates for inter-node mTLS
# 
# Usage: ./generate_certs.sh
# =============================================================================

set -e

CERT_DIR="$(dirname "$0")"
cd "$CERT_DIR"

# Certificate validity (days)
CA_DAYS=3650      # 10 years
NODE_DAYS=365     # 1 year

# Clean previous certificates
rm -f *.pem *.key *.csr *.srl 2>/dev/null || true

echo "=== Generating Iris mTLS Certificates ==="

# -----------------------------------------------------------------------------
# 1. Generate CA (Certificate Authority)
# -----------------------------------------------------------------------------
echo "1. Generating CA..."

openssl genrsa -out ca.key 4096

openssl req -new -x509 -days $CA_DAYS -key ca.key -out ca.pem \
    -subj "/C=US/ST=California/L=San Francisco/O=Iris/OU=Infrastructure/CN=Iris CA"

echo "   ✓ CA certificate: ca.pem"

# -----------------------------------------------------------------------------
# 2. Generate Node Certificates (for Core and Edge nodes)
# -----------------------------------------------------------------------------
generate_node_cert() {
    local NODE_NAME=$1
    local NODE_TYPE=$2  # core or edge
    
    echo "   Generating certificate for $NODE_NAME..."
    
    # Generate private key
    openssl genrsa -out "${NODE_NAME}.key" 2048
    
    # Create CSR with SAN (Subject Alternative Names)
    cat > "${NODE_NAME}.cnf" << EOF
[req]
default_bits = 2048
prompt = no
default_md = sha256
req_extensions = req_ext
distinguished_name = dn

[dn]
C = US
ST = California
L = San Francisco
O = Iris
OU = ${NODE_TYPE}
CN = ${NODE_NAME}

[req_ext]
subjectAltName = @alt_names

[alt_names]
DNS.1 = ${NODE_NAME}
DNS.2 = localhost
DNS.3 = *.iris.local
IP.1 = 127.0.0.1
EOF
    
    # Generate CSR
    openssl req -new -key "${NODE_NAME}.key" -out "${NODE_NAME}.csr" \
        -config "${NODE_NAME}.cnf"
    
    # Sign with CA
    openssl x509 -req -in "${NODE_NAME}.csr" -CA ca.pem -CAkey ca.key \
        -CAcreateserial -out "${NODE_NAME}.pem" -days $NODE_DAYS \
        -extfile "${NODE_NAME}.cnf" -extensions req_ext
    
    # Clean up CSR and config
    rm -f "${NODE_NAME}.csr" "${NODE_NAME}.cnf"
    
    echo "   ✓ ${NODE_NAME}.pem + ${NODE_NAME}.key"
}

echo "2. Generating Core node certificates..."
generate_node_cert "core-east-1" "core"
generate_node_cert "core-east-2" "core"
generate_node_cert "core-west-1" "core"
generate_node_cert "core-west-2" "core"
generate_node_cert "core-eu-1" "core"
generate_node_cert "core-eu-2" "core"

echo "3. Generating Edge node certificates..."
generate_node_cert "edge-east-1" "edge"
generate_node_cert "edge-east-2" "edge"
generate_node_cert "edge-west-1" "edge"
generate_node_cert "edge-west-2" "edge"
generate_node_cert "edge-eu-1" "edge"
generate_node_cert "edge-eu-2" "edge"
generate_node_cert "edge-sydney-1" "edge"
generate_node_cert "edge-sydney-2" "edge"
generate_node_cert "edge-saopaulo" "edge"

# -----------------------------------------------------------------------------
# 3. Generate Test Certificates (for mTLS testing)
# -----------------------------------------------------------------------------
echo "4. Generating test certificates..."

# Valid client certificate
generate_node_cert "test-client" "client"

# Expired certificate (for testing expiry rejection)
echo "   Generating expired certificate..."
openssl genrsa -out expired.key 2048
openssl req -new -key expired.key -out expired.csr \
    -subj "/C=US/ST=California/O=Iris/CN=expired-client"
# Create certificate that expired yesterday
openssl x509 -req -in expired.csr -CA ca.pem -CAkey ca.key \
    -CAcreateserial -out expired.pem -days -1 2>/dev/null || \
    faketime 'last year' openssl x509 -req -in expired.csr -CA ca.pem -CAkey ca.key \
    -CAcreateserial -out expired.pem -days 1 2>/dev/null || \
    echo "   ⚠ Could not create expired cert (needs faketime)"
rm -f expired.csr

# Self-signed certificate (untrusted CA - for testing rejection)
echo "   Generating untrusted certificate..."
openssl genrsa -out untrusted-ca.key 2048
openssl req -new -x509 -days 365 -key untrusted-ca.key -out untrusted-ca.pem \
    -subj "/C=US/ST=California/O=Evil Corp/CN=Untrusted CA"
openssl genrsa -out untrusted.key 2048
openssl req -new -key untrusted.key -out untrusted.csr \
    -subj "/C=US/ST=California/O=Evil Corp/CN=untrusted-client"
openssl x509 -req -in untrusted.csr -CA untrusted-ca.pem -CAkey untrusted-ca.key \
    -CAcreateserial -out untrusted.pem -days 365
rm -f untrusted.csr untrusted-ca.key untrusted-ca.srl

echo "   ✓ Test certificates generated"

# -----------------------------------------------------------------------------
# 4. Set permissions
# -----------------------------------------------------------------------------
echo "5. Setting permissions..."
chmod 644 *.pem
chmod 600 *.key

# -----------------------------------------------------------------------------
# 5. Verify certificates
# -----------------------------------------------------------------------------
echo "6. Verifying certificates..."
for cert in core-east-1.pem edge-east-1.pem test-client.pem; do
    if openssl verify -CAfile ca.pem "$cert" > /dev/null 2>&1; then
        echo "   ✓ $cert verified"
    else
        echo "   ✗ $cert FAILED verification"
        exit 1
    fi
done

echo ""
echo "=== Certificate Generation Complete ==="
echo "CA Certificate:     ca.pem"
echo "Node Certificates:  core-*.pem, edge-*.pem"
echo "Test Certificates:  test-client.pem, expired.pem, untrusted.pem"
echo ""
echo "To use in Erlang:"
echo "  {ssl, [{cacertfile, \"certs/ca.pem\"},"
echo "         {certfile, \"certs/node.pem\"},"
echo "         {keyfile, \"certs/node.key\"},"
echo "         {verify, verify_peer}]}."
