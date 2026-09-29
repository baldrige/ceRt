"""Is this extracted "questions presented" text actually questions presented?

About 7% of extracted QPs are not: OCR of handwriting that came out as symbol
soup, a brief's cover page (counsel's address, "PETITION FOR A WRIT"), or a
table of contents. Jev classifies them anyway, and not with low confidence --
on a 150-petition sample its confidence on the 11 unreadable inputs ran from
0.32 to 1.0 -- so a confidence threshold cannot catch them. This check runs
before classification and such a case gets no label at all.

Deliberately simple and explainable: prose has common English words and a high
share of letters; the failures have neither, or are dominated by front-matter
markers. Tuned on that sample (11 unreadable, 139 readable) and checked against
the 573 decided cases scored in eval_scdb.py.
"""
import re

COMMON = {
    'the', 'of', 'to', 'a', 'in', 'and', 'whether', 'is', 'that', 'for', 'court', 'does', 'did', 'may',
    'under', 'an', 'be', 'by', 'or', 'as', 'on', 'this', 'with', 'when', 'not', 'its', 'which', 'are',
    'was', 'it', 'has', 'have', 'can', 'where', 'from', 'state', 'federal', 'law', 'right', 'rights',
}
FRONT_MATTER = re.compile(
    r'table of contents|table of authorities|petition for (a )?writ of certiorari|counsel of record|'
    r'p\.?\s?o\.? box|@\w+\.(com|org|net|gov)|\(\d{3}\)\s?\d{3}-\d{4}|\.{8,}', re.I)


# Front matter that follows the questions on the same extracted page. Cut there
# (only past the first 80 characters, so a QP that merely starts after a cover
# page is not cut to nothing): what follows is noise to the classifier too.
TRAILER = re.compile(
    r'\b(table of contents|table of authorities|list of parties|parties to the proceeding|'
    r'corporate disclosure|rule 29\.6|related (cases|proceedings))\b', re.I)


def clean_qp(qp: str) -> str:
    text = ' '.join((qp or '').split())
    m = next((m for m in TRAILER.finditer(text) if m.start() >= 80), None)
    return text[:m.start()].rstrip() if m else text


def readable(qp: str) -> tuple[bool, str]:
    """(ok, reason). Reason is empty when ok."""
    text = clean_qp(qp)
    if len(text) < 40:
        return False, 'too short'
    chars = [c for c in text if not c.isspace()]
    letters = sum(c.isalpha() for c in chars) / len(chars)
    words = re.findall(r"[A-Za-z]+(?:'[a-z]+)?", text)
    if not words:
        return False, 'no words'
    common = sum(w.lower() in COMMON for w in words) / len(words)
    # Common-word share is the main separator: every readable sample QP scored
    # >= 0.20 and every unreadable one <= 0.12 bar two front-matter pages. The
    # letter share is only a backstop -- a short QP dense with citations
    # ("Does 18 U.S.C. § 922(g)(1) violate...") is 69% letters and fine.
    if common < 0.15:
        return False, f'common words {common:.2f}'
    if letters < 0.45:
        return False, f'letters {letters:.2f}'
    # Front matter counts against text with no question in it (a QP that follows
    # the cover page on the same extracted page is fine), or when it dominates.
    fm = len(FRONT_MATTER.findall(text))
    has_question = bool(re.search(r'\bwhether\b|\?', text, re.I))
    if (fm >= 2 and not has_question) or fm >= 8:
        return False, 'front matter'
    return True, ''
