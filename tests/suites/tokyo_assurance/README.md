# Tokyo Proof of Exercise

**Audit-Grade System Behavior Validation for Tokyo Region**

This suite provides **publicly defensible proof** of Iris system behavior in the Tokyo region. All tests produce machine-readable CSV outputs and a human-readable technical report.

---

## Quick Start

```bash
# Prerequisites
# - Edge node running at iris_edge1@100.82.212.50 (Tokyo)
# - Core nodes running at iris_core1@..., iris_core2@... (Bangalore)
# - Python 3.9+, Erlang/OTP 25+

# Run full proof
python3 tokyo_proof.py \
    --edge-node iris_edge1@100.82.212.50 \
    --core1 iris_core1@100.95.21.52 \
    --core2 iris_core2@100.68.74.48
```

---

## What It Tests

| Proof | Description | Output |
|-------|-------------|--------|
| **Scalability** | Find maximum sustainable load | `tokyo_scalability_metrics.csv` |
| **Latency** | Measure P50/P95/P99 under load | Included in scalability CSV |
| **Reliability** | Failover resilience (Core1 killed) | `tokyo_reliability_metrics.csv` |
| **Resources** | CPU, memory, connections over time | `tokyo_resource_metrics.csv` |

---

## Outputs

After running `tokyo_proof.py`, these files are generated in `proof_outputs/`:

```
proof_outputs/
├── tokyo_scalability_metrics.csv   # Load vs throughput/latency
├── tokyo_latency_metrics.csv       # (if separate latency test run)
├── tokyo_resource_metrics.csv      # CPU/memory/connections
├── tokyo_reliability_metrics.csv   # Failover test results
├── tokyo_summary.csv               # Pass/fail summary table
├── TOKYO_PROOF_REPORT.md           # Human-readable report
└── tokyo_proof.log                 # Execution log
```

---

## CSV Formats

### tokyo_scalability_metrics.csv
```csv
timestamp,load_level,sent,recv,queue_depth,delivery_rate,p50_ms,p95_ms,p99_ms,status
2026-01-11T17:00:00,100,1500,1450,50,0.9667,45,120,250,STABLE
```

### tokyo_resource_metrics.csv
```csv
timestamp,elapsed_s,cpu_percent,memory_rss_mb,memory_available_mb,disk_free_gb,open_connections,process_count
2026-01-11T17:00:00,0.0,15.2,512.0,7680.0,50.5,150,10
```

### tokyo_summary.csv
```csv
metric,value,unit,pass_fail,threshold,notes
max_stable_load,500,users,PASS,100,Minimum viable load
p99_latency_at_stable,250,ms,PASS,500,At 500 users
reliability_core1_failover,95.2,%,PASS,90,Sent=1000, Recv=952
```

---

## Pass/Fail Criteria

| Metric | Pass | Fail |
|--------|------|------|
| Queue Growth Rate | ≤ 10% | > 10% (overloaded) |
| P99 Latency | ≤ 500ms | > 500ms |
| Failover Delivery | ≥ 90% | < 90% |
| CPU Average | < 90% | ≥ 90% |

---

## Options

```bash
# Skip reliability test (doesn't kill Core1)
python3 tokyo_proof.py \
    --edge-node iris_edge1@100.82.212.50 \
    --core1 iris_core1@100.95.21.52 \
    --core2 iris_core2@100.68.74.48 \
    --skip-reliability
```

---

## Verifying Results

```bash
# Check summary
cat proof_outputs/tokyo_summary.csv | column -t -s,

# Count failures
grep -c "FAIL" proof_outputs/tokyo_summary.csv && echo "HAS FAILURES" || echo "ALL PASS"

# View report
cat proof_outputs/TOKYO_PROOF_REPORT.md
```

---

## Individual Tests

You can also run individual test components:

```bash
# Scalability only
python3 test_scalability.py --edge-node ... --core ... --levels "100,500,1000"

# Failover only
python3 test_failover_proof.py --edge-node ... --core1 ... --core2 ...

# Chaos scenarios
python3 test_tokyo_chaos.py --edge-node ... --core-nodes ...
```

---

## Comparison with Other Regions

CSV outputs are designed for cross-region comparison. To compare Tokyo with another region:

```bash
# Diff summaries
diff tokyo_summary.csv singapore_summary.csv

# Compare max stable load
grep "max_stable_load" tokyo_summary.csv singapore_summary.csv
```

---

## Troubleshooting

### "Node not reachable"
Ensure nodes are running and network allows Erlang distribution (port 4369 + ephemeral ports).

### "Load generator failed"
Check `proof_outputs/loadgen_*.log` for connection errors.

### Low delivery rate
Check Edge server logs for circuit breaker trips or RPC failures.

---

## Contributing

When modifying these tests:

1. All metrics must be captured to CSV
2. Pass/fail criteria must be explicit and testable
3. Report must be machine-generated, not hand-edited
4. Changes must be reproducible by independent verification

---

*Maintained by Senior Principal Engineer - Audit Grade Testing*
