#!/usr/bin/env bash
# publish_site.sh --------------------------------------------------------------
# Commit a gh-pages checkout and push it, surviving a concurrent push from
# another workflow. Run from INSIDE the checkout:
#
#   working-directory: site
#   run: ../.github/scripts/publish_site.sh "Daily dashboard refresh $(date -u +%F)"
#
# Four workflows publish to gh-pages -- daily, conferences, rerender-dockets and
# fill-throttled-dockets -- and each carried its own copy of this loop, with its
# own comment explaining why a rebase could never conflict. All three of those
# explanations were wrong:
#
#   daily.yml            "conferences touch conferences/ + funnel/"
#   conferences.yml      "daily touches dashboards/ + the landing index"
#   rerender-dockets.yml "this job only touches cases/"
#
# They were true once. render_conferences.R has since gained a
# render_dockets_for() call, so conferences writes cases/ as well, and every one
# of these jobs now touches cases/.manifest.json and cases/search.json. On
# 2026-07-31 a daily and a conference run overlapped, the rebase conflicted on
# the manifest, and because a `run:` step is `bash -e` the failed `git pull
# --rebase` killed the job outright -- a five-attempt retry loop that got one
# attempt, and lost a clean 153/153 fetch at the last step.
#
# So: one copy, and it resolves the conflict it will actually meet.

set -euo pipefail

# With no paths, this is a full site publish: stage everything and re-assert the
# domain and favicon. With paths, stage only those -- the QP backfills touch one
# cache file and should not sweep up whatever else is in the tree, nor claim to
# be republishing the site.
MSG="${1:?usage: publish_site.sh <commit message> [path ...]}"
shift
PATHS=("$@")

# Every derived cache published to gh-pages. All are JSON objects keyed by
# docket, and each is written by more than one workflow, so a conflict has a
# well-defined resolution -- the union -- rather than a side to pick. Anything
# ELSE conflicting is a real disagreement about page content, and this script
# refuses to guess at those.
#
#   cases/.manifest.json                 render hashes; daily, conferences,
#                                        rerender, fill-throttled
#   cases/search.json                    docket -> caption, same four
#   cases/forecasts.json                 "<docket>@<conf_date>" -> what the site
#                                        forecast BEFORE that conference.
#                                        Append-only per key by construction (see
#                                        R/forecast_log.R), so a union is exactly
#                                        right -- and a union can never resurrect
#                                        a retired key here, because nothing ever
#                                        retires one.
#   cases/grants.json                    docket -> grant date/caption/order, for
#                                        the Atom feeds. Written by conferences
#                                        (full-term data, where grants are
#                                        visible) and by the daily (which can
#                                        only see one inside its trailing fetch
#                                        window). Append-only per key, so a union
#                                        is exactly right.
#
# cases/pending.json is deliberately NOT in this list, and that is a semantic
# distinction rather than an oversight. Every cache above is append-only per key,
# which is what makes `*` the right resolution. pending.json is the opposite: a
# docket disposed of since the last run has to DISAPPEAR from it, or the fetch
# list only ever grows. Unioning it would resurrect exactly the keys the render
# step just retired. Only conferences.yml writes it, and that workflow does not
# run concurrently with itself (concurrency: conference-reports), so a conflict
# on it means something genuinely unexpected -- which the fall-through below
# already refuses to guess at.
#   conferences/qp_cache.json            render_conferences.R AND both QP
#                                        backfills -- 16 MB, the most expensive
#                                        thing here to lose
#   dashboards/qp_cache.json             the daily
#   dashboards/petition_signals_cache.json  Rule 10 signals, the daily
#   arguments/qp_cache.json              render_arguments.R
DERIVED='cases/.manifest.json cases/search.json cases/grants.json cases/forecasts.json
         conferences/qp_cache.json dashboards/qp_cache.json
         dashboards/petition_signals_cache.json arguments/qp_cache.json'

git config user.name  "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"

if [ ${#PATHS[@]} -eq 0 ]; then
  # GitHub Pages serves supremecourt.report only while this CNAME exists at the
  # gh-pages root; the favicon is re-asserted for the same reason. Both are
  # rewritten on every full publish so no rebuild can silently drop them.
  echo "supremecourt.report" > CNAME
  cp ../favicon.svg ../favicon.ico . 2>/dev/null || true
  git add -A
else
  git add -- "${PATHS[@]}"
fi
if git diff --cached --quiet; then
  echo "No changes to publish."
  exit 0
fi
git commit -m "$MSG"

# Resolve a conflict in the derived caches by unioning them. Returns non-zero if
# anything is still unmerged afterwards, which means the conflict was somewhere
# this script has no business deciding.
resolve_derived() {
  local f ours theirs ok=1
  for f in $DERIVED; do
    git ls-files -u -- "$f" | grep -q . || continue
    ours=$(mktemp); theirs=$(mktemp)
    # During a rebase the stages invert relative to intuition: :2 is the
    # upstream tip already on gh-pages, :3 is the commit being replayed (ours).
    git show ":3:$f" > "$ours"   2>/dev/null || echo '{}' > "$ours"
    git show ":2:$f" > "$theirs" 2>/dev/null || echo '{}' > "$theirs"
    # `*` merges right into left, so our freshly-rendered entries win per key.
    if jq -s '.[0] * .[1]' "$theirs" "$ours" > "$f"; then
      git add "$f"
      echo "  resolved $f by union ($(jq 'length' "$f") keys)"
    else
      echo "  jq failed on $f" >&2; ok=0
    fi
    rm -f "$ours" "$theirs"
  done
  [ "$ok" = 1 ] && ! git ls-files -u | grep -q .
}

for i in 1 2 3 4 5; do
  if git push origin gh-pages; then exit 0; fi
  echo "push rejected; rebasing onto latest gh-pages (attempt $i)"
  if ! git pull --rebase origin gh-pages; then
    if resolve_derived; then
      GIT_EDITOR=true git rebase --continue
    else
      git rebase --abort || true
      echo "rebase conflict outside the derived caches -- refusing to resolve blind" >&2
      exit 1
    fi
  fi
  sleep $((RANDOM % 5 + 3))
done

echo "still could not publish after 5 attempts" >&2
exit 1
