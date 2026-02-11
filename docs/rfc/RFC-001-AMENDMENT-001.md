# RFC-001-AMENDMENT-001: End-to-End Encryption and Group Messaging

**Status**: IMPLEMENTED  
**Authors**: System Architecture Team  
**Created**: 2026-01-23  
**Protocol Freeze Date**: 2026-01-23  
**Amends**: RFC-001-SYSTEM-REQUIREMENTS.md v4.0  
**Revision**: 1.3

---

## Abstract

This amendment adds End-to-End Encryption (E2EE) and Group Messaging as **launch-blocking requirements** for Project Iris v1.0. These features were previously deferred to RFC-002 but are now recognized as essential for competitive parity with WhatsApp/Signal.

---

## 1. Changes to Section 2.4 (Future Scope)

### REMOVE from "Deferred to RFC-002":

- ~~Group messaging~~
- ~~End-to-end encryption~~

### RETAIN as deferred:

- Multi-device sync (deferred to v1.5)
- Media messages (deferred to RFC-002)
- Voice/video calls (deferred to RFC-002)

---

## 2. New Functional Requirements

### 2.5 End-to-End Encryption [MUST]

| ID | Requirement | Definition | Test Criteria |
|----|-------------|------------|---------------|
| FR-12 | E2EE mandatory | All message content encrypted client-to-client | Server cannot decrypt any message payload |
| FR-13 | Key bundle upload | Client uploads Identity Key + Signed Pre-Key + One-Time Pre-Keys | Verify bundle stored with 99.999% durability |
| FR-14 | Key bundle fetch | Recipient's public keys available to sender | Fetch returns valid bundle within 50ms P99 |
| FR-14a | OPK exhaustion fallback | When OPK pool empty, X3DH proceeds with SPK-only (3-DH) | Verify SK derived from DH1,DH2,DH3 only |
| FR-15 | Forward secrecy | Compromise of long-term keys does not reveal past messages | Simulate key compromise, verify old messages unreadable |
| FR-16 | Post-compromise security | After key recovery, attacker loses access | Advance ratchet 100 times, verify old session keys invalid |

### 2.6 Group Messaging [MUST]

| ID | Requirement | Definition | Test Criteria |
|----|-------------|------------|---------------|
| FR-17 | Group creation | User can create a group with unique ID | Create 1000 groups, verify unique IDs |
| FR-18 | Group membership | Add/remove members, max 256 members | Add/remove 100 members, verify roster correct |
| FR-19 | Group messaging | Message delivered to all online members | Send to 100-member group, verify 100 deliveries |
| FR-20 | Group E2EE | Group messages encrypted with Sender Keys | Server cannot decrypt group message payload |
| FR-21 | Group offline delivery | Offline members receive messages on reconnect | Member reconnects, receives all missed messages |
| FR-22 | Member leave | User can leave group voluntarily | Leave group, verify no further messages received |
| FR-23 | Member removal | Admin can remove member | Remove member, verify immediate exclusion |

---

## 3. New Non-Functional Requirements

### 3.6 E2EE Performance [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-22 | E2EE overhead | ≤5ms | Time from plaintext to ciphertext |
| NFR-23 | Key bundle storage durability | 99.999% | Same as message durability |
| NFR-24 | One-Time Pre-Key pool | ≥100 keys | Alert when pool < 20 |
| NFR-25 | Key rotation | SPK rotates weekly | Automated rotation without user action |

### 3.7 Group Performance [MUST]

| ID | Metric | Target | Measurement |
|----|--------|--------|-------------|
| NFR-26 | Group fan-out latency | ≤200ms P99 | Time from sender to last recipient (in-region) |
| NFR-27 | Group size limit | 256 members | Reject add beyond limit |
| NFR-28 | Sender Key distribution | ≤500ms | Time to distribute key to all members |
| NFR-29 | Group roster query | ≤50ms P99 | Time to fetch member list |

> **Implementation Note (NFR-27):** The current implementation uses `iris_limits` as the
> single source of truth: `max_e2ee_group_members() -> 256` (E2EE groups via Sender Keys)
> and `max_broadcast_group_members() -> 10000` (non-E2EE broadcast groups). The 256 limit
> for E2EE groups matches the RFC baseline. Broadcast groups support much larger membership
> since they do not require pairwise key distribution.

---

## 4. Protocol Extensions

> **Canonical reference**: All opcodes, message formats, and wire specifications are defined in **PROTOCOL_V1_FREEZE.md**. This section lists the opcodes introduced by this amendment for traceability only.

**Opcodes added**: `0x10` CBOR_MSG, `0x20–0x24` E2EE protocol, `0x30–0x36` Group protocol.  
**Message formats**: See PROTOCOL_V1_FREEZE Sections 4.1 (E2EE) and 4.2 (CBOR).

---

## 5. Security Model Amendments

### 5.1 Additional Threat Model Entries

| Threat | Control |
|--------|---------|
| Server reads messages | E2EE (server never sees plaintext) |
| Key server compromise | Pre-keys are public; private keys never leave client |
| Group key leakage | Sender Key rotation on member removal |
| Metadata analysis | Future: sealed sender (deferred) |

### 5.2 E2EE Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│ CLIENT: Plaintext, Private keys, Ratchet state              │
│         (NEVER leaves device)                                │
└─────────────────────────────────────────────────────────────┘
                              │
                       E2EE Ciphertext Only
                              │
┌─────────────────────────────────────────────────────────────┐
│ SERVER: Public key bundles, Encrypted message blobs          │
│         (CANNOT decrypt, CANNOT forge)                       │
└─────────────────────────────────────────────────────────────┘
```

### 5.3 Key Verification

**5.3.1 Safety Number Display**: Each E2EE session MUST be representable as a "safety number" — `SHA-256(sort(IK_A, IK_B))[:30]` displayed as 12 groups of 5 digits (60 digits total), matching Signal's UX pattern.

**5.3.2 Key Change Notification**: When a user's Identity Key changes, the server MUST notify all active sessions. Clients MUST display: "Alice's security code changed."

**5.3.3 Key Transparency Log** *(deferred to post-launch)*: Append-only Merkle tree of all public key operations per CONIKS or similar scheme.

---

## 6. Cryptographic Specifications

> **Canonical reference**: Algorithm table and key sizes are in **PROTOCOL_V1_FREEZE.md Section 3**. This section covers protocol-level behavior only.

### 6.1 X3DH Key Exchange

Per Signal Protocol specification:
1. Alice fetches Bob's key bundle: (IK_B, SPK_B, OPK_B)
2. Alice generates ephemeral key EK_A
3. Alice computes: DH1=DH(IK_A, SPK_B), DH2=DH(EK_A, IK_B), DH3=DH(EK_A, SPK_B), DH4=DH(EK_A, OPK_B)
4. Shared secret SK = HKDF(DH1 || DH2 || DH3 || DH4)
5. Alice sends initial message with (IK_A, EK_A, OPK_index, ciphertext)

**OPK Exhaustion Fallback** (FR-14a): If no OPK available, DH4 is omitted. SK = HKDF(DH1 || DH2 || DH3). Cryptographically secure but loses one-time pre-key uniqueness. Client MUST replenish when pool < 20 keys; upload batch of 100 via opcode 0x20.

### 6.2 Double Ratchet

Per Signal Protocol specification:
- Symmetric-key ratchet: Derive new message keys for each message
- DH ratchet: Advance on each reply to achieve forward secrecy
- Header encryption: Optional (deferred to v1.5)

### 6.3 Sender Keys (Groups)

Per Signal Protocol specification:
1. Each member generates a Sender Key for the group
2. Sender Key = (chain_key, signature_key)
3. Distribute via 1:1 E2EE pairwise sessions
4. On member removal: All remaining members generate new Sender Keys

---

## 7. Deprecation Schedule

> See **PROTOCOL_V1_FREEZE.md Section 5** for canonical deprecation timeline (v0.9 warn → v1.0 require E2EE → v1.1 remove plaintext).

---

## 8. Testing Requirements Amendment

### 8.1 New Test Coverage

| Requirement | Test Type | Test File |
|-------------|-----------|-----------|
| FR-12 | Integration | `tests/suites/integration/test_offline_e2ee.py` |
| FR-13 | Unit | `test_utils/iris_keys_tests.erl` |
| FR-14 | Integration | `tests/suites/chaos_dist/test_key_bundle_durability.py` |
| FR-15 | Integration | `tests/suites/integration/test_group_e2ee.py` (test_key_rotation_on_member_leave) |
| FR-16 | E2E | `tests/suites/e2e/test_post_compromise.py` |
| FR-17 | Unit | `test_utils/iris_group_tests.erl` |
| FR-18 | Integration | `tests/suites/integration/test_group_membership.py` |
| FR-19 | Integration | `tests/suites/integration/test_group_e2ee.py` |
| FR-20 | Integration | `tests/suites/integration/test_group_e2ee.py` |
| FR-21 | Integration | `tests/suites/integration/test_offline_e2ee.py` |
| NFR-22 | Performance | `tests/suites/performance_light/benchmark_e2ee_latency.py` |
| NFR-26 | Performance | `tests/suites/integration/test_group_e2ee.py` (test_group_message_encryption) |

### 8.2 Security Test Requirements

| Test | Description | Pass Criteria |
|------|-------------|---------------|
| Server decryption attempt | Server tries to decrypt message | MUST fail |
| Key extraction attempt | Attempt to extract private key from server | MUST return only public keys |
| Replay attack | Replay old E2EE message | MUST be rejected (dedup) |
| Sender Key forward secrecy | Compromise current key | Old messages MUST remain secure |

---

## 9. Migration Path

### 9.1 Client Upgrade Path

1. **Phase 1**: New clients generate key bundles, upload to server
2. **Phase 2**: Server stores both plaintext and E2EE capable status per user
3. **Phase 3**: When both parties E2EE capable, use E2EE
4. **Phase 4**: After 90% adoption, force E2EE (reject plaintext)

### 9.2 Backward Compatibility

- Old clients (pre-E2EE) continue to work until v1.0 cutoff
- Server maintains compatibility shim during transition
- Grace period: 6 months from E2EE launch to mandatory

---

## Appendix A: References

1. Signal Protocol Specification: https://signal.org/docs/
2. X3DH Key Agreement Protocol: https://signal.org/docs/specifications/x3dh/
3. Double Ratchet Algorithm: https://signal.org/docs/specifications/doubleratchet/
4. Sender Keys: https://signal.org/docs/specifications/sender-keys/

---

## Appendix B: Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-23 | 1.0 | Initial amendment |
| 2026-01-25 | 1.2 | Updated Section 8.1 test paths to reflect actual implementation locations; added NFR-27 implementation note documenting 1000-member capacity |
| 2026-02-06 | 1.3 | Standards Audit: added FR-14a OPK exhaustion fallback (P1-6), Section 5.3 Key Verification (P1-5); DRY consolidation — protocol details reference PROTOCOL_V1_FREEZE |

---

**Approval**:

- [ ] Engineering Lead
- [ ] Security Review  
- [ ] Cryptography Review (REQUIRED for E2EE)
- [ ] Operations Review

---

*This amendment, once approved, becomes part of RFC-001 and supersedes the "Future Scope" exclusions for E2EE and Group Messaging.*
