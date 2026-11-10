#!/usr/bin/env python3
"""The structured opinion (extract_opinion.py) -> Markdown, EPUB, and an HTML body.

    python render_opinion.py <opinion.json> <meta.json> <out stem>

writes <stem>.md, <stem>.epub and <stem>.body.html. The HTML is the reader's
body only -- sections, notes, contents -- and R/opinion_page.R sets it inside
the site's own head and masthead, so the page chrome and every colour stay
where the rest of the site keeps them (R/palette.R).

`meta` is what the site knows and the PDF does not say reliably: docket,
caption, date decided, citation, the PDF's URL. See docs/opinions.md.

Nothing here reads the PDF. A fault in the text is a fault in the extraction
and is fixed there; this file only decides how a paragraph, a note reference
or a page break is written in each format.
"""
import html
import io
import json
import re
import sys
import zipfile

RENDER_VERSION = "o1"
SOURCE_NOTE = ("Prepared by Supreme Court Report from the PDF published by the Supreme Court of the United States. "
               "The PDF is the authoritative text; this copy can contain errors of conversion.")


# ---- runs ---------------------------------------------------------------------------

def settle_breaks(runs):
    """A page break that falls inside a word is moved to the end of that word."""
    out = []
    held = None
    for r in runs:
        if "pb" in r:
            if r.get("mid"):
                held = {k: v for k, v in r.items() if k != "mid"}
            else:
                out.append(r)
            continue
        if held is not None:
            m = re.match(r"^\S*", r["t"])
            cut = m.end()
            if cut < len(r["t"]) or not r["t"]:
                if cut:
                    out.append(dict(r, t=r["t"][:cut]))
                out.append(held); held = None
                if r["t"][cut:]:
                    out.append(dict(r, t=r["t"][cut:]))
                continue
        out.append(r)
    if held is not None:
        out.append(held)
    return out


def esc(t):
    return html.escape(t, quote=False)


def runs_html(runs, sid, epub=False, seen=None):
    out = []
    for r in settle_breaks(runs):
        if "pb" in r:
            if r["pb"]:
                pid = "%s-p%s" % (sid, r["pb"])
                if epub:
                    out.append('<span epub:type="pagebreak" role="doc-pagebreak" id="%s" aria-label="%s"></span>' % (pid, esc(r["pb"])))
                else:
                    out.append('<span class="pg" id="%s" title="page %s of the original">%s</span>' % (pid, esc(r["pb"]), esc(r["pb"])))
            continue
        t = esc(r["t"])
        if r.get("fn"):
            n = r["fn"]
            first = seen is None or n not in seen
            if seen is not None:
                seen.add(n)
            rid = ' id="%s-r%s"' % (sid, n) if first else ""
            if epub:
                out.append('<sup><a epub:type="noteref" role="doc-noteref"%s href="#%s-n%s">%s</a></sup>' % (rid, sid, n, esc(n)))
            else:
                out.append('<sup class="fnr"><a%s href="#%s-n%s">%s</a></sup>' % (rid, sid, n, esc(n)))
            continue
        if r.get("sup"):
            t = "<sup>%s</sup>" % t
        if r.get("sc"):
            t = '<span class="sc">%s</span>' % t
        if r.get("i"):
            t = "<em>%s</em>" % t
        out.append(t)
    s = "".join(out)
    # an italic run usually carries its trailing space: "<em>Bruen </em>held"
    s = re.sub(r"(\s+)</em>", r"</em>\1", s)
    s = re.sub(r"(\s+)</span>", r"</span>\1", s)
    return s


MD_ESC = re.compile(r"([\\`*_\[\]<>])")


def runs_md(runs, sid):
    out = []
    for r in settle_breaks(runs):
        if "pb" in r:
            if r["pb"]:
                out.append("[\\*%s] " % r["pb"])
            continue
        if r.get("fn"):
            out.append("[^%s-%s]" % (sid, r["fn"]))
            continue
        t = r["t"]
        if r.get("sc"):
            t = t.upper()
        t = MD_ESC.sub(r"\\\1", t)
        if r.get("i") and t.strip():
            # the emphasis marks hug the words; the run's own spaces stay outside
            lead = t[:len(t) - len(t.lstrip())]
            trail = t[len(t.rstrip()):]
            t = "%s*%s*%s" % (lead, t.strip(), trail)
        out.append(t)
    return "".join(out).strip()


def md_block_start(t):
    """Keep Markdown from reading a paragraph's opening as a list or a heading."""
    t = re.sub(r"^(\d+)([.)])", r"\1\\\2", t)
    t = re.sub(r"^([#>+\-])", r"\\\1", t)
    return t


# ---- the document's own description ---------------------------------------------------

def describe(meta):
    bits = []
    if meta.get("docket"):
        d = meta["docket"]
        bits.append(("Nos. " if "," in d else "No. ") + d)
    if meta.get("decided_long"):
        bits.append("Decided " + meta["decided_long"])
    if meta.get("cite"):
        bits.append(meta["cite"])
    return bits


def toc_entries(doc):
    return [(s["id"], s["title"]) for s in doc["sections"]]


# ---- HTML body ----------------------------------------------------------------------------

def section_html(s, epub=False):
    sid = s["id"]
    seen = set()
    out = []
    pend = ""
    in_quote = False

    def close_quote():
        nonlocal in_quote
        if in_quote:
            out.append("</blockquote>"); in_quote = False

    if s.get("byline"):
        out.append('<p class="byline">%s</p>' % esc(s["byline"]))
    for b in s["blocks"]:
        if b["type"] == "pb":
            if b.get("pb"):
                pend += runs_html([b], sid, epub)
            continue
        body = pend + runs_html(b["runs"], sid, epub, seen); pend = ""
        if b["type"] == "quote":
            if not in_quote:
                out.append("<blockquote>"); in_quote = True
            out.append("<p>%s</p>" % body)
            continue
        close_quote()
        if b["type"] == "h":
            out.append('<h3 class="oh">%s</h3>' % body)
        elif b["type"] == "sep":
            out.append('<p class="sep">* * *</p>')
        else:
            out.append('<p%s>%s</p>' % (' class="cont"' if b.get("cont") else "", body))
    close_quote()
    notes = s.get("notes") or []
    if notes:
        if epub:
            out.append('<section class="notes" epub:type="footnotes" role="doc-endnotes"><h3>Notes</h3>')
            for n in notes:
                paras = "".join("<p>%s%s</p>" % (('<a href="#%s-r%s">%s</a>. ' % (sid, n["n"], esc(n["n"]))) if k == 0 and n["n"].isdigit() and n["n"] in seen
                                                 else (esc(n["n"]) + " " if k == 0 else ""), runs_html(p, sid, epub))
                                for k, p in enumerate(n["paras"]))
                if n["n"].isdigit():
                    out.append('<aside epub:type="footnote" role="doc-footnote" id="%s-n%s">%s</aside>' % (sid, n["n"], paras))
                else:
                    out.append('<aside>%s</aside>' % paras)
            out.append("</section>")
        else:
            out.append('<aside class="notes" aria-label="Footnotes"><h3>Notes</h3><ol>')
            for n in notes:
                paras = ["<p>%s</p>" % runs_html(p, sid) for p in n["paras"]]
                if n["n"].isdigit():
                    back = ' <a class="back" href="#%s-r%s" aria-label="Back to the text">↩</a>' % (sid, n["n"]) if n["n"] in seen else ""
                    paras[-1] = paras[-1][:-4] + back + "</p>"
                    out.append('<li id="%s-n%s" value="%s">%s</li>' % (sid, n["n"], n["n"], "".join(paras)))
                else:
                    out.append('<li class="sym" value="0"><span class="mk">%s</span>%s</li>' % (esc(n["n"]), "".join(paras)))
            out.append("</ol></aside>")
    return "\n".join(out)


def body_html(doc, meta):
    out = ['<nav class="otoc" aria-label="Contents"><ol>']
    for sid, title in toc_entries(doc):
        out.append('<li><a href="#%s">%s</a></li>' % (sid, esc(title)))
    out.append("</ol></nav>")
    for s in doc["sections"]:
        cls = "op op-%s" % s["kind"]
        if s["kind"] == "counsel":
            out.append('<section id="%s" class="%s"><details><summary>Counsel</summary>%s</details></section>' % (s["id"], cls, section_html(s)))
        else:
            out.append('<section id="%s" class="%s"><h2>%s</h2>\n%s</section>' % (s["id"], cls, esc(s["title"]), section_html(s)))
    return "\n".join(out)


# ---- Markdown -----------------------------------------------------------------------------

def markdown(doc, meta):
    L = ["# " + meta.get("caption", "Opinion"), ""]
    bits = describe(meta)
    if bits:
        L += [" · ".join(bits), ""]
    L += ["> " + SOURCE_NOTE + (" Source: <%s>" % meta["pdf_url"] if meta.get("pdf_url") else ""), ""]
    for s in doc["sections"]:
        sid = s["id"]
        L += ["## " + s["title"], ""]
        if s.get("byline"):
            L += ["*%s*" % MD_ESC.sub(r"\\\1", s["byline"]), ""]
        pend = ""
        for b in s["blocks"]:
            if b["type"] == "pb":
                if b.get("pb"):
                    pend = "[\\*%s] " % b["pb"]
                continue
            t = pend + runs_md(b["runs"], sid); pend = ""
            if b["type"] == "h":
                L += ["### " + t, ""]
            elif b["type"] == "sep":
                L += ["\\* \\* \\*", ""]
            elif b["type"] == "quote":
                L += ["> " + md_block_start(t), ""]
            else:
                L += [md_block_start(t), ""]
        for n in s.get("notes") or []:
            paras = [runs_md(p, sid) for p in n["paras"]]
            if n["n"].isdigit():
                L.append("[^%s-%s]: %s" % (sid, n["n"], paras[0]))
                for p in paras[1:]:
                    L += ["", "    " + p]
            else:
                L.append("%s %s" % (MD_ESC.sub(r"\\\1", n["n"]), "\n\n".join(paras)))
            L.append("")
    return "\n".join(L).rstrip() + "\n"


# ---- EPUB -----------------------------------------------------------------------------------

EPUB_CSS = """
body { font-family: serif; line-height: 1.5; margin: 0 4%; }
h1 { font-size: 1.5em; line-height: 1.25; text-align: center; margin: 2em 0 0.6em; }
h2 { font-size: 1.25em; margin: 2em 0 1em; text-align: center; }
h3 { font-size: 1em; text-align: center; margin: 1.6em 0 0.8em; }
p { margin: 0; text-indent: 1.3em; }
p.cont, p.byline, p.meta, p.src, h2 + p, h3 + p, blockquote p:first-child, aside p { text-indent: 0; }
p.byline { margin: 0 0 1em; }
p.meta { text-align: center; margin: 0.3em 0; }
p.src { font-size: 0.85em; margin: 2em 0 0; }
p.sep { text-align: center; text-indent: 0; margin: 1.2em 0; }
blockquote { margin: 0.8em 1.6em; }
.sc { font-variant: small-caps; }
sup { font-size: 0.7em; line-height: 0; }
section.notes { margin-top: 2.5em; font-size: 0.9em; }
aside { margin: 0 0 0.7em; }
nav ol { list-style: none; padding-left: 0; }
nav li { margin: 0.4em 0; }
"""

XHTML = ('<?xml version="1.0" encoding="utf-8"?>\n<!DOCTYPE html>\n'
         '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" lang="en" xml:lang="en">\n'
         '<head><meta charset="utf-8"/><title>%s</title><link rel="stylesheet" type="text/css" href="style.css"/></head>\n'
         '<body>\n%s\n</body></html>\n')


def xhtml(s):
    """The HTML this file writes, as XML: void forms closed, entities numeric."""
    return s.replace("&nbsp;", "&#160;")


def epub(doc, meta, path):
    title = meta.get("caption", "Opinion")
    uid = "urn:supremecourt-report:opinion:%s:%s" % (meta.get("term", ""), meta.get("docket_id", meta.get("docket", "")))
    modified = (meta.get("decided") or "2000-01-01") + "T00:00:00Z"
    files = []
    front = ['<h1>%s</h1>' % esc(title)]
    for b in describe(meta):
        front.append('<p class="meta">%s</p>' % esc(b))
    front.append('<p class="meta">Supreme Court of the United States</p>')
    front.append('<p class="src">%s%s</p>' % (esc(SOURCE_NOTE), (' Source: <a href="%s">%s</a>' % (esc(meta["pdf_url"]), esc(meta["pdf_url"]))) if meta.get("pdf_url") else ""))
    files.append(("title.xhtml", "Title", XHTML % (esc(title), "\n".join(front))))
    pages = []
    for s in doc["sections"]:
        name = "%s.xhtml" % s["id"]
        inner = '<section epub:type="chapter" role="doc-chapter" id="%s"><h2>%s</h2>\n%s</section>' % (s["id"], esc(s["title"]), section_html(s, epub=True))
        for m in re.finditer(r'epub:type="pagebreak" role="doc-pagebreak" id="([^"]+)" aria-label="([^"]+)"', inner):
            pages.append((name, m.group(1), m.group(2)))
        files.append((name, s["title"], XHTML % (esc(s["title"]), xhtml(inner))))
    nav = ['<nav epub:type="toc" role="doc-toc" id="toc"><h2>Contents</h2><ol>']
    for name, t, _ in files:
        nav.append('<li><a href="%s">%s</a></li>' % (name, esc(t)))
    nav.append("</ol></nav>")
    if pages:
        nav.append('<nav epub:type="page-list" id="pages" hidden="hidden"><ol>')
        seen = set()
        for name, pid, label in pages:
            if pid in seen:
                continue
            seen.add(pid)
            nav.append('<li><a href="%s#%s">%s</a></li>' % (name, pid, esc(label)))
        nav.append("</ol></nav>")
    nav_doc = XHTML % ("Contents", "\n".join(nav))
    manifest = ['<item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav"/>',
                '<item id="css" href="style.css" media-type="text/css"/>']
    spine = []
    for k, (name, _, _) in enumerate(files):
        manifest.append('<item id="f%d" href="%s" media-type="application/xhtml+xml"/>' % (k, name))
        spine.append('<itemref idref="f%d"/>' % k)
        if k == 0:
            spine.append('<itemref idref="nav"/>')
    opf = ('<?xml version="1.0" encoding="utf-8"?>\n'
           '<package xmlns="http://www.idpf.org/2007/opf" version="3.0" unique-identifier="uid" xml:lang="en">\n'
           '<metadata xmlns:dc="http://purl.org/dc/elements/1.1/">\n'
           '<dc:identifier id="uid">%s</dc:identifier>\n<dc:title>%s</dc:title>\n<dc:language>en</dc:language>\n'
           '<dc:creator>Supreme Court of the United States</dc:creator>\n<dc:publisher>Supreme Court Report</dc:publisher>\n'
           '%s<meta property="dcterms:modified">%s</meta>\n</metadata>\n<manifest>\n%s\n</manifest>\n<spine>\n%s\n</spine>\n</package>\n') % (
        esc(uid), esc(title), ("<dc:date>%s</dc:date>\n" % meta["decided"]) if meta.get("decided") else "",
        modified, "\n".join(manifest), "\n".join(spine))
    container = ('<?xml version="1.0" encoding="utf-8"?>\n<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">'
                 '<rootfiles><rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/></rootfiles></container>\n')
    stamp = (2000, 1, 1, 0, 0, 0)        # a fixed time, so the same opinion builds the same file

    def put(z, name, data, stored=False):
        zi = zipfile.ZipInfo(name, date_time=stamp)
        zi.compress_type = zipfile.ZIP_STORED if stored else zipfile.ZIP_DEFLATED
        zi.external_attr = 0o644 << 16
        z.writestr(zi, data)

    with zipfile.ZipFile(path, "w") as z:
        put(z, "mimetype", "application/epub+zip", stored=True)
        put(z, "META-INF/container.xml", container)
        put(z, "OEBPS/content.opf", opf)
        put(z, "OEBPS/nav.xhtml", nav_doc)
        put(z, "OEBPS/style.css", EPUB_CSS.strip() + "\n")
        for name, _, data in files:
            put(z, "OEBPS/" + name, data)


def render(doc, meta, stem):
    with io.open(stem + ".md", "w", encoding="utf-8", newline="\n") as f:
        f.write(markdown(doc, meta))
    with io.open(stem + ".body.html", "w", encoding="utf-8", newline="\n") as f:
        f.write(body_html(doc, meta))
    epub(doc, meta, stem + ".epub")


def main():
    doc = json.load(io.open(sys.argv[1], encoding="utf-8"))
    meta = json.load(io.open(sys.argv[2], encoding="utf-8"))
    render(doc, meta, sys.argv[3])


if __name__ == "__main__":
    main()
