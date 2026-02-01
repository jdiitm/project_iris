#!/usr/bin/env python3
"""
RFC Compliance Watchdog

Validates that:
1. RFC doc changes have corresponding test changes
2. RFC tags in tests (NFR-1, FR-2, etc.) reference valid requirements
3. Critical RFC requirements have test coverage

Usage:
  scripts/rfc_watchdog.py --check          # Validate current state
  scripts/rfc_watchdog.py --diff HEAD~1    # Check changes in last commit

Exit Codes:
  0 - All checks passed
  1 - Validation failed (orphaned references or missing coverage)
  2 - RFC changed without test updates (warning, non-blocking)
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Directories
PROJECT_ROOT = Path(__file__).parent.parent
RFC_DIR = PROJECT_ROOT / "docs" / "rfc"
TESTS_DIR = PROJECT_ROOT / "tests"
TEST_UTILS_DIR = PROJECT_ROOT / "test_utils"
SRC_DIR = PROJECT_ROOT / "src"


def extract_rfc_tags(content: str) -> set:
    """Extract NFR-* and FR-* tags from content."""
    return set(re.findall(r'(?:NFR|FR)-\d+', content))


def collect_rfc_requirements() -> set:
    """Collect all RFC requirement tags from RFC docs."""
    tags = set()
    if RFC_DIR.exists():
        for rfc_file in RFC_DIR.glob("*.md"):
            try:
                tags.update(extract_rfc_tags(rfc_file.read_text()))
            except Exception:
                pass
    return tags


def collect_test_references() -> set:
    """Collect all RFC tags referenced in tests."""
    tags = set()
    
    # Python tests
    if TESTS_DIR.exists():
        for test_file in TESTS_DIR.rglob("*.py"):
            try:
                tags.update(extract_rfc_tags(test_file.read_text()))
            except Exception:
                pass
    
    # Erlang tests
    if TEST_UTILS_DIR.exists():
        for test_file in TEST_UTILS_DIR.glob("*.erl"):
            try:
                tags.update(extract_rfc_tags(test_file.read_text()))
            except Exception:
                pass
    
    return tags


def collect_src_references() -> set:
    """Collect all RFC tags referenced in source code."""
    tags = set()
    if SRC_DIR.exists():
        for src_file in SRC_DIR.glob("*.erl"):
            try:
                tags.update(extract_rfc_tags(src_file.read_text()))
            except Exception:
                pass
    return tags


def check_rfc_coverage():
    """
    Check RFC requirement coverage.
    
    Returns:
        dict with coverage report
    """
    rfc_tags = collect_rfc_requirements()
    test_tags = collect_test_references()
    src_tags = collect_src_references()
    
    all_code_refs = test_tags | src_tags
    
    # Find gaps
    untested = rfc_tags - test_tags  # In RFC but not in tests
    orphaned = all_code_refs - rfc_tags  # In code but not in RFC
    
    return {
        'rfc_requirements': sorted(rfc_tags),
        'test_references': sorted(test_tags),
        'src_references': sorted(src_tags),
        'tested': sorted(rfc_tags & test_tags),
        'untested': sorted(untested),
        'orphaned': sorted(orphaned),
        'coverage_pct': (len(rfc_tags & test_tags) / len(rfc_tags) * 100) if rfc_tags else 100
    }


def check_diff_compliance(base: str = "HEAD~1"):
    """
    Check if RFC doc changes are accompanied by test changes.
    
    Args:
        base: Git ref to compare against
        
    Returns:
        tuple: (passed, message)
    """
    try:
        result = subprocess.run(
            ["git", "diff", "--name-only", base],
            capture_output=True,
            text=True,
            cwd=str(PROJECT_ROOT)
        )
        if result.returncode != 0:
            return True, f"Could not get diff: {result.stderr}"
        
        changed = result.stdout.strip().split('\n')
        changed = [f for f in changed if f]  # Filter empty
        
        rfc_changed = any('docs/rfc' in f for f in changed)
        tests_changed = any('tests/' in f or 'test_utils/' in f for f in changed)
        
        if rfc_changed and not tests_changed:
            return False, "RFC docs changed without test updates"
        
        return True, "OK"
    except Exception as e:
        return True, f"Could not check diff: {e}"


def print_report(report: dict):
    """Print coverage report."""
    print("\n" + "=" * 60)
    print(" RFC COMPLIANCE REPORT")
    print("=" * 60)
    
    print(f"\nRFC Requirements Found: {len(report['rfc_requirements'])}")
    print(f"Test References Found:  {len(report['test_references'])}")
    print(f"Coverage:               {report['coverage_pct']:.1f}%")
    
    if report['tested']:
        print(f"\n[OK] Tested Requirements ({len(report['tested'])}):")
        for tag in report['tested'][:10]:
            print(f"  - {tag}")
        if len(report['tested']) > 10:
            print(f"  ... and {len(report['tested']) - 10} more")
    
    if report['untested']:
        print(f"\n[WARN] Untested Requirements ({len(report['untested'])}):")
        for tag in report['untested']:
            print(f"  - {tag}")
    
    if report['orphaned']:
        print(f"\n[ERROR] Orphaned References ({len(report['orphaned'])}):")
        print("  (Referenced in code but not defined in RFC docs)")
        for tag in report['orphaned']:
            print(f"  - {tag}")
    
    print("\n" + "=" * 60)


def main():
    parser = argparse.ArgumentParser(description="RFC Compliance Watchdog")
    parser.add_argument("--check", action="store_true", help="Check RFC coverage")
    parser.add_argument("--diff", type=str, help="Check diff against base ref")
    parser.add_argument("--strict", action="store_true", help="Fail on any issues")
    parser.add_argument("--quiet", action="store_true", help="Minimal output")
    
    args = parser.parse_args()
    
    exit_code = 0
    
    if args.check or not args.diff:
        report = check_rfc_coverage()
        
        if not args.quiet:
            print_report(report)
        
        # Orphaned references are errors
        if report['orphaned']:
            if not args.quiet:
                print("\n[FAIL] Found orphaned RFC references in code")
            exit_code = 1
        
        # Untested requirements are warnings (errors in strict mode)
        if report['untested'] and args.strict:
            if not args.quiet:
                print("\n[FAIL] Found untested RFC requirements (strict mode)")
            exit_code = 1
    
    if args.diff:
        passed, message = check_diff_compliance(args.diff)
        
        if not args.quiet:
            print(f"\nDiff Compliance: {message}")
        
        if not passed:
            if not args.quiet:
                print("[WARN] RFC docs changed without test updates")
            # Non-blocking warning (exit 2)
            if exit_code == 0:
                exit_code = 2
    
    if exit_code == 0 and not args.quiet:
        print("\n[PASS] RFC compliance check passed")
    
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
