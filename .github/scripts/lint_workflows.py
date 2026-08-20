#!/usr/bin/env python3
"""Check the timeout invariants every workflow in this repo has to hold.

Two rules, both learned the hard way on 2026-08-19:

  1. Every job sets timeout-minutes.
     Without it a job runs to GitHub's 6-hour default. A daily that normally
     takes four minutes hung for 73 and would have burned six.

  2. A job's cap must be strictly GREATER than the sum of its step caps.
     Otherwise the job dies first and the step caps never fire -- which is
     worse than having none, because a step timeout names the action that
     hung ("the action 'Run r-lib/actions/setup-r@v2' has timed out after 15
     minutes") and a job-level cancel names only the job. Raising a step cap
     without checking what contains it broke eight jobs at once, and the
     symptom was a run that looked like a mystery until the elapsed time
     happened to match the job cap.

Run: python3 .github/scripts/lint_workflows.py
Exits 1 on any violation.
"""
import glob
import os
import sys

try:
    import yaml
except ImportError:                                    # pragma: no cover
    # Loudly, not silently. A lint that skips itself is indistinguishable from
    # a lint that passes, which is the whole failure mode this file exists for.
    sys.exit("FATAL: PyYAML not available; cannot lint workflows. "
             "Add a `pip install pyyaml` step rather than skipping this check.")


def main() -> int:
    files = sorted(glob.glob(".github/workflows/*.yml")) + \
            sorted(glob.glob(".github/workflows/*.yaml"))
    if not files:
        return sys.exit("FATAL: no workflow files found; is the working directory the repo root?")

    problems, checked = [], 0
    for path in files:
        with open(path) as fh:
            doc = yaml.safe_load(fh)
        for job_name, job in (doc.get("jobs") or {}).items():
            if not isinstance(job, dict):
                continue
            checked += 1
            where = f"{os.path.basename(path)}:{job_name}"
            job_cap = job.get("timeout-minutes")

            # Rule 1.
            if job_cap is None:
                problems.append(f"{where}: no timeout-minutes (would run to the 6-hour default)")
                continue
            # An expression rather than a number is not something this can reason
            # about; say so instead of pretending it passed.
            if not isinstance(job_cap, int):
                problems.append(f"{where}: timeout-minutes is {job_cap!r}, not a plain number")
                continue

            # Rule 2.
            step_caps = [s.get("timeout-minutes") for s in (job.get("steps") or [])
                         if isinstance(s, dict) and s.get("timeout-minutes") is not None]
            if not all(isinstance(c, int) for c in step_caps):
                problems.append(f"{where}: a step timeout-minutes is not a plain number")
                continue
            total = sum(step_caps)
            if step_caps and job_cap <= total:
                problems.append(
                    f"{where}: job cap {job_cap} <= sum of its {len(step_caps)} step cap(s) "
                    f"({total}); the job would die before any step timeout could name the culprit")

    print(f"{len(files)} workflow file(s), {checked} job(s) checked")
    if problems:
        print(f"\n{len(problems)} violation(s):")
        for p in problems:
            print(f"  - {p}")
        return 1
    print("timeout invariants hold: every job is capped, and every cap outranks its steps.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
