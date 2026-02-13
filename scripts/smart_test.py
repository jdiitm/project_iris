#!/usr/bin/env python3
"""
smart_test.py — Robust test wrapper for Project Iris.

Ensures:
1. Clean environment (kills stale Erlang nodes, removes leftover Mnesia dirs)
2. Log persistence to tests/artifacts/logs/
3. Post-run analysis for hidden errors (badarg, failed to open wal, etc.)

Usage:
    python3 scripts/smart_test.py -- <TEST_COMMAND> [ARGS...]

Examples:
    python3 scripts/smart_test.py -- ./tests/run_all_tests.sh --quick
    python3 scripts/smart_test.py -- ./docker/global-cluster/cluster.sh up
"""

import os
import re
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

# Project root is parent of scripts/
PROJECT_ROOT = Path(__file__).resolve().parent.parent
LOG_DIR = PROJECT_ROOT / "tests" / "artifacts" / "logs"

# Patterns that indicate hidden errors in Erlang output
HIDDEN_ERROR_PATTERNS = [
    (r"badarg", "badarg error detected"),
    (r"failed to open wal", "WAL file open failure"),
    (r"noproc", "Process not found (noproc)"),
    (r"noconnection", "Node connection lost"),
    (r"\{error,\s*\{already_started", "Already-started conflict"),
    (r"\*\* exception error:", "Unhandled exception"),
    (r"CRASH REPORT", "Process crash report"),
    (r"Mnesia\(.*\): .*failed", "Mnesia operation failed"),
    (r"Schema merge failed", "Mnesia schema merge failure"),
    (r"killed", "Process killed"),
]


def nuke_environment():
    """Kill stale Erlang nodes and clean transient data."""
    print("[smart_test] Nuking stale environment...", flush=True)

    # Kill any lingering beam.smp processes for iris nodes
    try:
        result = subprocess.run(
            ["pkill", "-f", "beam.smp.*iris"],
            capture_output=True, timeout=5
        )
        if result.returncode == 0:
            print("[smart_test]   Killed stale Erlang nodes", flush=True)
            time.sleep(1)  # Let processes die
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass

    # Remove leftover Mnesia directories and core dumps in project root
    for entry in PROJECT_ROOT.iterdir():
        if entry.is_dir() and entry.name.startswith("Mnesia."):
            try:
                subprocess.run(["rm", "-rf", str(entry)], timeout=5)
                print(f"[smart_test]   Removed {entry.name}", flush=True)
            except subprocess.TimeoutExpired:
                pass
        elif entry.is_file() and entry.name.startswith("MnesiaCore."):
            try:
                entry.unlink()
                print(f"[smart_test]   Removed {entry.name}", flush=True)
            except OSError:
                pass

    # Remove stale DETS files
    for dets in PROJECT_ROOT.glob("*.dets"):
        try:
            dets.unlink()
            print(f"[smart_test]   Removed {dets.name}", flush=True)
        except OSError:
            pass

    print("[smart_test] Environment clean.", flush=True)


def run_command(cmd_args):
    """Run the test command and capture output."""
    LOG_DIR.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = LOG_DIR / f"run_{timestamp}.log"

    print(f"[smart_test] Running: {' '.join(cmd_args)}", flush=True)
    print(f"[smart_test] Log: {log_file}", flush=True)
    print("=" * 70, flush=True)

    start_time = time.time()

    with open(log_file, "w") as log:
        log.write(f"# smart_test.py run at {datetime.now().isoformat()}\n")
        log.write(f"# Command: {' '.join(cmd_args)}\n")
        log.write(f"# CWD: {os.getcwd()}\n")
        log.write("=" * 70 + "\n\n")

        proc = subprocess.Popen(
            cmd_args,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            cwd=str(PROJECT_ROOT),
            text=True,
            bufsize=1,
        )

        output_lines = []
        for line in proc.stdout:
            sys.stdout.write(line)
            sys.stdout.flush()
            log.write(line)
            output_lines.append(line)

        proc.wait()
        elapsed = time.time() - start_time

        log.write(f"\n{'=' * 70}\n")
        log.write(f"# Exit code: {proc.returncode}\n")
        log.write(f"# Duration: {elapsed:.1f}s\n")

    print("=" * 70, flush=True)
    print(f"[smart_test] Exit code: {proc.returncode} | Duration: {elapsed:.1f}s", flush=True)

    return proc.returncode, output_lines, log_file


def analyze_output(output_lines):
    """Scan output for hidden errors that tests may not catch."""
    findings = []

    for i, line in enumerate(output_lines, 1):
        for pattern, description in HIDDEN_ERROR_PATTERNS:
            if re.search(pattern, line, re.IGNORECASE):
                findings.append((i, description, line.strip()))
                break  # One finding per line is enough

    return findings


def main():
    # Parse args: everything after "--" is the test command
    if "--" not in sys.argv:
        print("Usage: python3 scripts/smart_test.py -- <TEST_COMMAND> [ARGS...]")
        print("Example: python3 scripts/smart_test.py -- ./tests/run_all_tests.sh --quick")
        sys.exit(1)

    separator_idx = sys.argv.index("--")
    cmd_args = sys.argv[separator_idx + 1:]

    if not cmd_args:
        print("Error: No command specified after '--'")
        sys.exit(1)

    # Phase 1: Clean slate
    nuke_environment()

    # Phase 2: Run the command
    exit_code, output_lines, log_file = run_command(cmd_args)

    # Phase 3: Post-run analysis
    findings = analyze_output(output_lines)

    if findings:
        print(f"\n[smart_test] POST-RUN ANALYSIS: {len(findings)} hidden error(s) detected!")
        print("-" * 70, flush=True)
        for line_no, desc, text in findings[:20]:  # Cap at 20
            print(f"  Line {line_no}: [{desc}] {text[:120]}", flush=True)
        if len(findings) > 20:
            print(f"  ... and {len(findings) - 20} more", flush=True)
        print("-" * 70, flush=True)
    else:
        print("\n[smart_test] POST-RUN ANALYSIS: No hidden errors detected.", flush=True)

    print(f"[smart_test] Full log: {log_file}", flush=True)

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
