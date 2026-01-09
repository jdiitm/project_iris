# Project Iris: Performance & Reliability Standards (Baseline)

## 1. Service Level Objectives (SLOs)

We hold Project Iris to the following implicit standards derived from world-class messaging platforms (WhatsApp, Telegram, Signal).

### 1.1 Availability & Reliability
| Metric | Target | Definition |
| :--- | :--- | :--- |
| **Availability** | **99.99%** | < 52 mins downtime / year. |
| **Message Durability** | **99.999%** | < 1 in 100,000 messages lost after ACK. |
| **Error Rate** | **< 0.01%** | 5xx errors per total requests. |

### 1.2 Performance (Latency)
| Metric | Target (P50) | Target (P99) | Conditions |
| :--- | :--- | :--- | :--- |
| **Send Latency** | **< 20ms** | **< 200ms** | Client -> Server ACK |
| **E2E Delivery** | **< 100ms** | **< 500ms** | Online -> Online (Same Region) |
| **Geo Delivery** | **< 500ms** | **< 2s** | Online -> Online (Inter-Region) |

### 1.3 Throughput & Scale
| Metric | Minimum Target | Rationale |
| :--- | :--- | :--- |
| **Single Node Cap** | **500,000 Users** | Efficient hardware usage (Erlang process density). |
| **Cluster Throughput** | **100M Msgs/Day** | ~1,150 msgs/sec sustained avg; 50k peak/sec. |
| **Fan-In Burst** | **50,000 Msgs/Sec** | "Justin Bieber" scenario handling. |

### 1.4 Resilience (RTO/RPO)
| Scenario | Recovery Time Objective (RTO) | Recovery Point Objective (RPO) |
| :--- | :--- | :--- |
| **Process Crash** | **< 10ms** | **0 seconds** (Supervision restart) |
| **Node Failure** | **< 30s** | **0 seconds** (Consistent Hashing handoff) |
| **Network Partition**| **Auto-Heal** | **Zero Data Loss** (Queued on Edge) |

## 2. Evaluation Methodology

### 2.1 Assumptions
1.  **Hardware**: Reference benchmarks run on standard cloud commodity hardware (e.g., AWS c5.xlarge or equivalent).
2.  **Network**: Assumes <100ms RTT inter-region latency.
3.  **Client**: Assumes compliant clients that respect backpressure signals (429/503).

### 2.2 Telemetry Source of Truth
All claims must be backed by CSV data exports generated during "God Level" certification runs:
- `latency_metrics.csv`
- `throughput_metrics.csv`
- `resource_usage.csv`
- `recovery_metrics.csv`

---
**Status**: BINDING | **Date**: 2026-01-09
