# Dependencies

## Erlang Dependencies

This project uses **zero external Erlang dependencies**. All functionality is implemented against OTP 26 standard applications:

| OTP Application | Purpose |
|-----------------|---------|
| `kernel`        | Core runtime, distribution, networking |
| `stdlib`        | Standard library, gen_server, supervisor |
| `crypto`        | HMAC-SHA256, EdDSA signing/verification, secure random |
| `ssl`           | TLS termination, mTLS enforcement |
| `public_key`    | Certificate parsing, key management |
| `mnesia`        | Distributed database for offline messages, dedup, tokens |

## Native Dependencies

| Library | Required | Purpose |
|---------|----------|---------|
| `libzstd-dev` | Optional | Zstd compression NIF (RFC Section 11.1) |

When `libzstd-dev` is not installed, the system falls back to zlib compression. The `iris_compression:available_algorithms/0` function dynamically detects NIF availability at runtime.

Install on Debian/Ubuntu:
```
apt-get install libzstd-dev
```

## Build Tools

| Tool | Version | Purpose |
|------|---------|---------|
| Erlang/OTP | 26+ | Runtime and compiler |
| GNU Make | Any | Build orchestration |
| Python 3.11+ | Test-only | Integration test framework |

## Why No rebar3?

This project deliberately avoids rebar3 and external Hex packages to:
1. Eliminate supply-chain attack surface (no transitive dependencies)
2. Ensure reproducible builds with zero network fetches
3. Simplify deployment (single `ebin/` directory, no `_build/` tree)

Since there are no external dependencies, there is no `rebar.lock` or dependency manifest to scan. Automated security scanning (Dependabot, `mix audit`) is not applicable.

## Security Scanning

In lieu of dependency scanning, the CI pipeline runs:
- **Dialyzer** static analysis for type safety and undefined function detection
- **Xref** cross-reference analysis for dead code and missing function calls
- **Property-based tests** for protocol and crypto edge cases
