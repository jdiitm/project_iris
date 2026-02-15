# Iris Protocol v1.0 - Specification Freeze

**Status**: FROZEN  
**Freeze Date**: 2026-01-23  
**Version**: 1.1 (Standards Audit fixes — 2026-02-06)  

---

## 1. Overview

This document is the **canonical protocol specification** for Iris. All opcodes, message formats, and cryptographic specifications are defined here. Changes require an RFC amendment.

> **v1.1 changes**: Fixed opcode 0x10 collision (P1-2), added control opcodes 0x08–0x0B (P2-1, P2-4, P2-7).

---

## 2. Frozen Protocol Opcodes

### 2.1 Core Messaging (v0.x - STABLE)

| Opcode | Name | Direction | Status |
|--------|------|-----------|--------|
| 0x01 | LOGIN | Client→Server | Stable |
| 0x02 | SEND_MESSAGE | Client→Server | **DEPRECATED** (use 0x23) |
| 0x03 | ACK | Both | Stable |
| 0x04 | BATCH_SEND | Client→Server | Stable |
| 0x05 | GET_STATUS | Client→Server | Stable |
| 0x06 | STATUS_RESPONSE | Server→Client | Stable |
| 0x07 | SEND_SEQ | Client→Server | Stable |
| 0x08 | PING | Client→Server | Stable (v1.1) |
| 0x09 | PONG | Server→Client | Stable (v1.1) |
| 0x0A | RESUME | Client→Server | Stable (v1.1) |
| 0x0B | TOKEN_REFRESH | Client→Server | Stable (v1.1) |
| 0x0C | VERSION_NEGOTIATE | Both | Stable (v1.1) |
| 0x0D | SEND_SEQ_V2 | Client→Server | Stable (v1.2, RFC v4.0) |
| 0x11 | RELIABLE_MSG | Server→Client | Stable (moved from 0x10 in v1.1) |

> **v1.1 breaking change**: `RELIABLE_MSG` moved from `0x10` to `0x11` to resolve collision with `CBOR_MSG`. Migration: server sends on both during transition; clients signal `0x11` support via capability negotiation.

> **v1.2 addition** (RFC v4.0): `SEND_SEQ_V2` (`0x0D`) extends `SEND_SEQ` (`0x07`) with a mandatory 128-bit UUIDv7 idempotency key. Wire format: `0x0D | TargetLen(16) | Target | IdempotencyKey(128) | SeqNo(64) | MsgLen(16) | Msg`. The idempotency key enables server-side deduplication by `(user_id, idempotency_key)` per RFC Section 1.2. `0x07` remains unchanged for backward compatibility.

### 2.2 CBOR Extension (v1.0)

| Opcode | Name | Direction | Status |
|--------|------|-----------|--------|
| 0x10 | CBOR_MSG | Both | Stable |

### 2.3 E2EE Protocol (v1.0)

| Opcode | Name | Direction | Status |
|--------|------|-----------|--------|
| 0x20 | UPLOAD_PREKEYS | Client→Server | Stable |
| 0x21 | FETCH_PREKEYS | Client→Server | Stable |
| 0x22 | PREKEY_RESPONSE | Server→Client | Stable |
| 0x23 | E2EE_MSG | Client→Server | Stable |
| 0x24 | E2EE_DELIVERY | Server→Client | Stable |

### 2.4 Group Messaging (v1.0) - Reserved

| Opcode | Name | Direction | Status |
|--------|------|-----------|--------|
| 0x30 | GROUP_CREATE | Client→Server | Reserved |
| 0x31 | GROUP_JOIN | Server→Client | Reserved |
| 0x32 | GROUP_LEAVE | Client→Server | Reserved |
| 0x33 | GROUP_MSG | Client→Server | Reserved |
| 0x34 | GROUP_DELIVERY | Server→Client | Reserved |
| 0x35 | GROUP_ROSTER | Client→Server | Reserved |
| 0x36 | SENDER_KEY_DIST | Client→Server | Reserved |

### 2.5 Post-Launch Reserved (v1.2+)

| Opcode | Name | Direction | Status |
|--------|------|-----------|--------|
| 0x40 | EDIT_MSG | Client→Server | Reserved |
| 0x41 | DELETE_MSG | Client→Server | Reserved |
| 0x42 | EDIT_NOTIFY | Server→Client | Reserved |
| 0x43 | DEL_NOTIFY | Server→Client | Reserved |
| 0x50 | BLOCK_USER | Client→Server | Reserved |
| 0x51 | UNBLOCK_USER | Client→Server | Reserved |
| 0x52 | REPORT_USER | Client→Server | Reserved |

---

## 3. Frozen Cryptographic Specifications

### 3.1 Key Exchange (X3DH)

- **Identity Key**: Curve25519 (256-bit)
- **Signed Pre-Key**: Curve25519 (256-bit), rotates weekly
- **One-Time Pre-Key**: Curve25519 (256-bit), pool ≥100
- **Ephemeral Key**: Curve25519 (256-bit), per-message

### 3.2 Message Encryption (Double Ratchet)

- **Symmetric Cipher**: AES-256-GCM
- **MAC**: 16-byte authentication tag
- **Key Derivation**: HKDF-SHA256
- **Signature**: Ed25519

### 3.3 Group E2EE (Sender Keys)

- **Sender Key**: Chain key (256-bit) + Signature key (Ed25519)
- **Distribution**: Via pairwise E2EE sessions
- **Rotation**: On member removal

---

## 4. Message Format Specifications

### 4.1 E2EE Message (0x23)

```
0x23 | RecipientLen:16 | Recipient:var |
     | HeaderLen:16 | Header:CBOR | 
     | CipherLen:32 | Ciphertext:var |
     | MAC:16
```

**Header CBOR Map**:
- `ik`: Sender Identity Key (32 bytes)
- `ek`: Ephemeral Key (32 bytes)  
- `opk_id`: One-Time Pre-Key index (optional)
- `pn`: Previous chain length
- `n`: Message number

### 4.2 CBOR Message (0x10)

```
0x10 | TargetLen:16 | Target:var |
     | CborLen:32 | CborPayload:var
```

---

## 5. Deprecation Schedule

| Version | Date | Change |
|---------|------|--------|
| v0.9 | 2026-01-23 | Emit warning on plaintext (0x02) |
| v1.0 | 2026-04-01 | Require E2EE (0x23) for new messages |
| v1.1 | 2026-07-01 | Remove plaintext opcode support |

---

## 6. Backward Compatibility Guarantees

### 6.1 What WILL NOT Change

- Opcode numbers for stable features
- CBOR encoding/decoding semantics
- Cryptographic algorithms and key sizes
- Message framing format (length prefixes)

### 6.2 What MAY Change (Minor Version)

- Adding new optional CBOR fields
- Adding new opcodes in reserved ranges
- Performance optimizations (internal)
- Error message text

### 6.3 What REQUIRES Major Version

- Changing opcode semantics
- Removing opcodes (requires deprecation)
- Changing cryptographic algorithms
- Breaking message format changes

---

## 7. Implementation Status

### 7.1 Erlang Server Components

| Module | Feature | Status |
|--------|---------|--------|
| `iris_proto.erl` | Protocol codec | ✅ Complete |
| `iris_keys.erl` | Key bundle storage | ✅ Complete |
| `iris_x3dh.erl` | X3DH key exchange | ✅ Complete |
| `iris_ratchet.erl` | Double Ratchet | ✅ Complete |
| `iris_group.erl` | Group management | ✅ Complete |
| `iris_sender_keys.erl` | Sender Keys | ✅ Complete |
| `iris_group_fanout.erl` | Group fan-out | ✅ Complete |
| ~~`iris_router_pool.erl`~~ | Worker pool | Removed (dead code) |

### 7.2 Test Coverage

See [TESTING.md](TESTING.md) for authoritative counts. Summary as of 2026-02-15:

| Category | Tests | Status |
|----------|-------|--------|
| Erlang EUnit | 185 test modules + 7 support | ✅ All passing |
| Python integration | 43 | ✅ All passing |
| Python e2e | 11 | ✅ All passing |
| Python security | 26 | ✅ All passing |
| Python stress | 20 | ✅ All passing |
| Python chaos (Docker) | 26 | ✅ All passing |
| Other suites | 43 | ✅ All passing |

---

## 8. Post-Freeze Process

### 8.1 Bug Fixes

Bug fixes that do not change protocol semantics MAY be applied without RFC amendment.

### 8.2 Security Fixes

Critical security fixes MAY be applied with expedited review. Security patches MUST be backward compatible when possible.

### 8.3 Protocol Extensions

New features MUST:
1. Use reserved opcode ranges
2. Go through full RFC amendment process
3. Maintain backward compatibility
4. Include deprecation plan if replacing existing features

---

## Approval

- [x] Protocol Implementation Complete (30+ opcodes)
- [x] Test Suite Passing (169 Python + 185 Erlang tests, all TLS-enabled)
- [x] Security Review (E2EE implementation)
- [x] Performance Validation

**Protocol Status**: FROZEN for v1.1 release

---

*This specification freeze document supersedes all prior draft protocol specifications. Any changes require a new RFC amendment.*
