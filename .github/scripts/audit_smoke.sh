#!/usr/bin/env bash
# Run the REAL audit_site.R against a synthetic site, end to end.
#
# The 2026-08-11 failure was a definition-order bug: a helper used 67 lines above
# where it was defined. Every previous test of these checks extracted the block
# and supplied the helper by hand, which is exactly why none of them caught it.
# This runs the actual file, so "does it reach the verdict" is a thing the test
# can answer.
set -uo pipefail
S="${1:-$(mktemp -d)}/auditsmoke"; rm -rf "$S"; mkdir -p "$S"
# Run from the repo root.

TV=$(grep '^PAGE_TEMPLATE_VERSION <- ' R/docket_page.R | sed 's/.*"\(v[0-9]*\)".*/\1/')
MAST="<span class='smast-wm'>SCR</span>"
FEEDS='<link rel="alternate" type="application/atom+xml" title="Supreme Court Report" href="/feed.xml"><link rel="alternate" type="application/atom+xml" title="Certiorari grants" href="/grants.xml">'

mkdir -p "$S"/{cases,dashboards,conferences,arguments,funnel,relists}

# Docket pages: tv stamp + the three nav markers the sampled check wants.
for d in 25-1 25-2 26-1; do
  printf "<!DOCTYPE html><html><head><meta name='tv' content='%s'></head><body>%s<nav class='bcrumb'></nav><div>x</div><footer class='cfoot'></footer></body></html>" \
    "$TV" "$MAST" > "$S/cases/$d.html"
done
echo '{"25-1":"A v B","25-2":"C v D","26-1":"E v F"}' > "$S/cases/search.json"
for f in index ot25 ot26; do echo "<html><head>$FEEDS</head><body>$MAST</body></html>" > "$S/cases/$f.html"; done

for p in index about methods; do
  echo "<html><head>$FEEDS</head><body>$MAST</body></html>" > "$S/$p.html"
done
# methods.html is hand-authored: no generated head, so no feed links.
echo "<html><body>$MAST</body></html>" > "$S/methods.html"
for d in dashboards conferences arguments funnel relists; do
  echo "<html><head>$FEEDS</head><body>$MAST</body></html>" > "$S/$d/index.html"
done
# Dated leaves need a prev/next slot and (now) autodiscovery.
echo "<html><head>$FEEDS</head><body>$MAST<!--PNAV--></body></html>" > "$S/conferences/conf_2026-09-28.html"
echo "<html><head>$FEEDS</head><body>$MAST<!--PNAV--></body></html>" > "$S/dashboards/dash_2026-08-10.html"
echo "<html><head>$FEEDS</head><body>$MAST<!--PNAV--></body></html>" > "$S/arguments/arg_2026.html"

# Feeds, sitemaps, robots.
cat > "$S/feed.xml" <<'X'
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom"><title>t</title><subtitle>s</subtitle>
<link rel="self" href="https://supremecourt.report/feed.xml"/>
<id>https://supremecourt.report/feed.xml</id><updated>2026-08-10T00:00:00Z</updated>
<entry><title>a</title><link href="https://supremecourt.report/cases/25-1.html"/>
<id>https://supremecourt.report/cases/25-1.html</id><updated>2026-08-10T00:00:00Z</updated></entry>
</feed>
X
sed 's|/feed.xml|/grants.xml|g' "$S/feed.xml" > "$S/grants.xml"
cat > "$S/sitemap.xml" <<'X'
<?xml version="1.0" encoding="utf-8"?>
<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
<sitemap><loc>https://supremecourt.report/sitemap-pages.xml</loc></sitemap>
</sitemapindex>
X
cat > "$S/sitemap-pages.xml" <<'X'
<?xml version="1.0" encoding="utf-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
<url><loc>https://supremecourt.report/</loc></url>
</urlset>
X
printf 'User-agent: *\nAllow: /\n\nSitemap: https://supremecourt.report/sitemap.xml\n' > "$S/robots.txt"
echo '{"25-1@2026-09-28":{"docket":"25-1","conf_date":"2026-09-28","scored_on":"2026-08-10","model_id":"abc123","p_grant_now":0.03}}' > "$S/cases/forecasts.json"

echo "=== running the real audit_site.R ==="
SITE_DIR="$S" Rscript .github/scripts/audit_site.R > "$S/out.txt" 2>&1
rc=$?
grep -E "\[ok|\[WARN|\[FAIL|checks:" "$S/out.txt" | sed 's/^/  /'
echo
if grep -q "checks:" "$S/out.txt"; then
  echo "REACHED THE VERDICT (exit $rc) -- the script ran end to end."
else
  echo "*** DIED BEFORE THE VERDICT (exit $rc) -- this is the 2026-08-11 failure mode ***"
  tail -6 "$S/out.txt" | sed 's/^/    /'
fi
