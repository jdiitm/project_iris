#!/bin/sh
# Generate a per-container ssl_dist.conf using IRIS_MTLS_* env vars.
# This avoids the shared-volume symlink race where all containers
# overwrote /app/certs/node.pem on the same host mount.
#
# Note: versions constraint is omitted to let OTP negotiate the best
# available TLS version. OTP 26 supports TLS 1.2 and 1.3 by default.
# Pinning to TLS 1.3 only triggers a hostname verification bug in
# OTP 26's inet_tls_dist (erlang/otp#7497).
cat > /tmp/ssl_dist.conf <<EOF
[{server, [{certfile, "$IRIS_MTLS_CERT"}, {keyfile, "$IRIS_MTLS_KEY"}, {cacertfile, "$IRIS_MTLS_CA"}, {verify, verify_peer}, {fail_if_no_peer_cert, true}, {secure_renegotiate, true}]}, {client, [{certfile, "$IRIS_MTLS_CERT"}, {keyfile, "$IRIS_MTLS_KEY"}, {cacertfile, "$IRIS_MTLS_CA"}, {verify, verify_peer}, {secure_renegotiate, true}]}].
EOF
