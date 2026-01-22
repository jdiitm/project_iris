# Docker Test Environment

This directory contains the Docker configuration for deterministic test execution.

## Quick Start

```bash
# Run all tests (deterministic)
make test-docker

# Run with custom seed for reproduction
TEST_SEED=12345 make test-docker

# Run specific suite
TEST_SUITE=unit make test-docker
```

## Files

| File | Purpose |
|------|---------|
| `Dockerfile` | Test runner image definition |
| `docker-compose.test.yml` | Full test cluster with core, edge, and test runner |

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `TEST_SEED` | `42` | Master seed for deterministic random |
| `TEST_RUN_ID` | `docker_run` | Unique identifier for this test run |
| `IRIS_COOKIE` | `iris_secret` | Erlang distribution cookie |
| `CI` | `true` | Enables CI-specific behaviors |
| `TEST_SUITE` | (all) | Specific suite to run |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Docker Network (test-network)            │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  core-east   │  │  core-west   │  │  edge-sydney │      │
│  │  (primary)   │──│  (replica)   │  │  :8090       │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│         │                │                   │              │
│         └────────────────┼───────────────────┘              │
│                          │                                  │
│  ┌──────────────┐  ┌──────────────┐                        │
│  │  edge-east   │  │  edge-west   │                        │
│  │  :8085       │  │  :8087       │                        │
│  └──────────────┘  └──────────────┘                        │
│         │                │                                  │
│         └────────────────┘                                  │
│                │                                            │
│  ┌─────────────────────────────────────────────────┐       │
│  │              test-runner                         │       │
│  │  (runs tests/run_tests.py --all)                │       │
│  └─────────────────────────────────────────────────┘       │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Reproduction

To reproduce a specific test failure:

1. Find the seed from the CI logs:
   ```
   [test_runner] Determinism: seed=42, run_id=docker_run
   ```

2. Run with that seed:
   ```bash
   TEST_SEED=42 make test-docker
   ```

3. If it still fails, the issue is deterministic. If it passes, the issue is environmental.

## Manual Execution

```bash
# Build the test image
docker build -t iris-test -f docker/test/Dockerfile .

# Run tests with default settings
docker run --rm iris-test

# Run with custom seed
docker run --rm -e TEST_SEED=12345 iris-test

# Run specific suite
docker run --rm iris-test python3 tests/run_tests.py --suite unit

# Interactive debugging
docker run --rm -it iris-test bash
```

## Cluster Mode

For tests requiring the full cluster:

```bash
# Start the full test cluster
docker-compose -f docker/test/docker-compose.test.yml up --build

# Run and exit on first failure
docker-compose -f docker/test/docker-compose.test.yml up --build --abort-on-container-exit

# Clean up
docker-compose -f docker/test/docker-compose.test.yml down -v
```
