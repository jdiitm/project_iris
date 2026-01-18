# RFC-001 Compliance Status

**Last Updated**: 2026-01-18  
**Reference**: [RFC-001-SYSTEM-REQUIREMENTS.md](rfc/RFC-001-SYSTEM-REQUIREMENTS.md)

---

## Implemented ✅

| RFC Section | Requirement | Implementation |
|-------------|-------------|----------------|
| NFR-6 | 99.999% durability | `sync_transaction` in `iris_offline_storage.erl` |
| NFR-8 | Zero data loss (RPO=0) | Sync writes to Mnesia disc_copies |
| NFR-14 | TLS mandatory | `check_tls_policy/1` in `iris_edge_listener.erl` |
| NFR-16 | JWT validation | `iris_auth.erl` with expiry/signature |
| NFR-17 | Rate limiting | `iris_rate_limiter.erl` |
| FR-9/10/11 | Token auth + revocation | Mnesia-backed revocation |
| Section 8 | Abuse prevention | Rate limiting per-user/IP |

---

## Deferred 📋

| RFC Section | Requirement | Reason | Effort |
|-------------|-------------|--------|--------|
| NFR-15 | mTLS inter-node | Requires Erlang distribution TLS config | 1-2 days |
| Section 9.1 | Protocol version negotiation | Breaking change, needs client migration | 1 week |
| Section 5.2 | UUIDv7 message IDs | Requires protocol change | 3-5 days |

---

## Configuration Notes

### TLS Enforcement (NFR-14)

Production **MUST** run with TLS enabled:
```erlang
[{iris_edge, [
    {tls_enabled, true},
    {tls_certfile, "/path/to/cert.pem"},
    {tls_keyfile, "/path/to/key.pem"}
]}].
```

For testing only, TLS can be bypassed:
```erlang
[{iris_edge, [
    {allow_insecure, true}  %% NEVER use in production
]}].
```

---

## Test Coverage

All RFC requirements have corresponding tests. See [TEST_STATUS.md](TEST_STATUS.md) for details.
