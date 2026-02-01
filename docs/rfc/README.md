# RFCs

Formal requirements and design documents for Project Iris.

## Active RFCs

| RFC | Title | Status |
|-----|-------|--------|
| [RFC-001](RFC-001-SYSTEM-REQUIREMENTS.md) | System Requirements | Draft |
| [RFC-001-AMENDMENT-001](RFC-001-AMENDMENT-001.md) | E2EE + Groups | Approved |

## RFC-001 Contents

| Section | Description |
|---------|-------------|
| 1. System Goals | 5B DAU, 500M concurrent, SLOs |
| 2. Functional Requirements | Messaging, presence, auth |
| 3. Non-Functional Requirements | Performance, reliability, security |
| 4. Architecture Constraints | Hub-and-spoke, layer requirements |
| 5. Delivery Guarantees | At-least-once, idempotency, ordering |
| 6. Security Model | Threat model, trust boundaries |
| 7. Failure Semantics | Failure modes, degradation hierarchy |
| 8. Abuse Prevention | Rate limits, spam controls |
| 9. Client Protocol | Wire format, sync protocol |
| 10-12 | Capacity, Compatibility, Testing |

## Related Documents

| Document | Purpose |
|----------|---------|
| [TESTING.md](../TESTING.md) | Test status, coverage, deviations |
| [RFC_COMPLIANCE.md](../RFC_COMPLIANCE.md) | Implementation status |

## RFC Process

1. **Draft** — Open for feedback
2. **Review** — Stakeholder review
3. **Approved** — Authoritative
4. **Superseded** — Replaced
