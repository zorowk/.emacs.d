#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)
emacs_binary=${EMACS:-emacs}
run_count=${ZORO_STARTUP_RUNS:-5}
maximum_ms=${ZORO_STARTUP_MAX_MS:-2000}
results_dir=$(mktemp -d /tmp/zoro-startup.XXXXXX)
trap 'rm -rf "$results_dir"' EXIT

: >"$results_dir/totals"

for run in $(seq 1 "$run_count"); do
  if ! output=$(
    cd "$repo_root"
    "$emacs_binary" --batch -Q \
      --eval '(setq user-emacs-directory default-directory)' \
      --eval '(when-let* ((directory (getenv "ZORO_PACKAGE_DIR"))) (setq package-user-dir (file-name-as-directory directory)))' \
      -l early-init.el \
      -l elisp/benchmark-startup.el \
      --eval '(zoro-startup-benchmark-activate-packages)' \
      -l init.el \
      --eval '(zoro-startup-benchmark-report)' 2>&1
  ); then
    printf '%s\n' "$output" >&2
    exit 1
  fi
  printf '%s\n' "$output"
  total=$(printf '%s\n' "$output" | sed -n 's/^Total: \([0-9.]*\)ms.*/\1/p')
  if [[ -z "$total" ]]; then
    printf 'Could not parse startup total for run %s.\n' "$run" >&2
    exit 1
  fi
  printf '%s\n' "$total" >>"$results_dir/totals"
done

sort -n "$results_dir/totals" >"$results_dir/sorted"
median=$(awk '{ values[NR] = $1 } END { if (NR % 2) print values[(NR + 1) / 2]; else printf "%.2f\n", (values[NR / 2] + values[NR / 2 + 1]) / 2 }' "$results_dir/sorted")
slowest=$(tail -n 1 "$results_dir/sorted")

report=$(printf '### Startup performance\n\n- Emacs: `%s`\n- Warm runs: %s\n- Median: %sms\n- Slowest: %sms\n- Safety ceiling: %sms\n' \
  "$($emacs_binary --batch -Q --eval '(princ emacs-version)' 2>/dev/null)" \
  "$run_count" "$median" "$slowest" "$maximum_ms")
printf '%s\n' "$report"

if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
  printf '%s\n' "$report" >>"$GITHUB_STEP_SUMMARY"
fi
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  printf 'median_ms=%s\nslowest_ms=%s\n' "$median" "$slowest" >>"$GITHUB_OUTPUT"
fi

if ! awk -v observed="$slowest" -v maximum="$maximum_ms" 'BEGIN { exit !(observed <= maximum) }'; then
  printf 'Slowest startup %sms exceeds the %sms ceiling.\n' "$slowest" "$maximum_ms" >&2
  exit 1
fi
