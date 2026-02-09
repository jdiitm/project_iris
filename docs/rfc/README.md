# RFCs

Formal requirements and design documents for Project Iris.

## Active RFCs

| RFC | Title | Version | Status |
|-----|-------|---------|--------|
| [RFC-001](RFC-001-SYSTEM-REQUIREMENTS.md) | System Requirements | v4.0 | Implemented |
| [RFC-001-AMENDMENT-001](RFC-001-AMENDMENT-001.md) | E2EE + Group Messaging | v1.0 | Approved / Implementing |

## RFC-001 Contents (v4.0)

| Section | Description |
|---------|-------------|
| 1 | System Goals — 5B DAU, 500M concurrent, SLOs |
| 2 | Functional Requirements — Messaging, presence, groups, auth |
| 3 | Non-Functional Requirements — Performance, reliability, security |
| 4 | Architecture Constraints — Hub-and-spoke, layer separation |
| 5 | Delivery Guarantees — At-least-once, HLC ordering, dedup |
| 6 | Security Model — Threat model, trust boundaries, E2EE |
| 7 | Failure Semantics — Degradation hierarchy, partition handling |
| 8 | Abuse Prevention — Rate limits, payload limits, inbox limits |
| 9 | Client Protocol — Wire format, JWT auth, key isolation |
| 10 | Capacity Planning — Per-connection budget, scaling thresholds |
| 11 | Compatibility — Version negotiation, protocol freeze |
| 12 | Testing Requirements — Test categories, CI tiers, compliance |
| 13 | Testing Standards — Determinism, `time.sleep` prohibition |

## Related Documents

| Document | Purpose |
|----------|---------|
| [RFC_COMPLIANCE.md](../RFC_COMPLIANCE.md) | Implementation verification status |
| [PROTOCOL_V1_FREEZE.md](../PROTOCOL_V1_FREEZE.md) | Canonical wire protocol specification |
| [TESTING.md](../TESTING.md) | Test suite details |

## RFC Process

1. **Draft** — Open for feedback
2. **Review** — Stakeholder review
3. **Approved** — Authoritative
4. **Implemented** — Code matches spec (verified via compliance tests)
5. **Superseded** — Replaced by newer RFC
