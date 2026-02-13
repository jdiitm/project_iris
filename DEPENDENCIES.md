# Dependencies

Project Iris has **zero external Erlang dependencies**. The entire system is built on pure OTP 26.

## Runtime Dependencies

| Dependency | Type | Required | Notes |
|---|---|---|---|
| Erlang/OTP 26 | Runtime | Yes | Core platform. No third-party rebar/hex packages. |
| Mnesia | OTP built-in | Yes | Distributed database for durable storage. |
| crypto | OTP built-in | Yes | HMAC-SHA256 and EdDSA signature verification. |
| ssl | OTP built-in | Yes | TLS for client connections and mTLS for inter-node. |
| zlib | OTP built-in | Yes | Default compression algorithm. |

## Optional Dependencies

| Dependency | Type | Required | Notes |
|---|---|---|---|
| libzstd-dev | C library | No | Enables zstd compression NIF. See below. |

### Zstd NIF (Optional)

The `iris_zstd_nif` module provides zstd compression via a C NIF. It is **optional** — the system falls back to zlib when the NIF is not available.

To enable:

```bash
sudo apt-get install libzstd-dev   # Debian/Ubuntu
make nif                           # Compiles priv/iris_zstd_nif.so
```

Runtime detection is automatic: `iris_compression:available_algorithms/0` checks for the NIF `.so` at startup and only advertises `zstd` in `SERVER_CAPABILITIES` when present.

## Test Dependencies

| Dependency | Type | Notes |
|---|---|---|
| Python 3.11+ | Test runner | Integration/E2E/chaos tests. |
| pytest | Python package | Test framework (see `requirements-test.txt`). |
| Docker | Infrastructure | Required only for Tier 1 chaos tests. |

## Design Philosophy

No external Erlang dependencies reduces:
- Supply chain attack surface
- Version conflict risk
- Build complexity
- Audit scope

All protocol parsing, JWT validation, JSON handling, and compression are implemented in-tree using OTP primitives.
