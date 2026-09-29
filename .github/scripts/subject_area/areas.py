"""The 14 subject-matter areas, as the Supreme Court Database codes them.

The labels are the Database's `issueArea` values (1-14), so a classification can
be scored against its coding of every decided case. The docstrings are the only
thing Jev learns the categories from -- it takes no labelled examples -- so each
one states the Database's own conventions, including the ones that surprise a
reader: habeas corpus is criminal procedure, deportation and civil-rights
damages suits are civil rights, takings and personal jurisdiction are due
process, standing and review of agency action are judicial power. Source: the
online codebook, variables `issue` and `issueArea` (scdb.la.psu.edu).

Tune these against the development Terms only (see eval_scdb.py). Editing them
while looking at the test Terms turns the test into a second development set.
"""
from enum import Enum

from pydantic import BaseModel, Field
from pydantic_ai import UseEnumMemberDocstrings


class Area(UseEnumMemberDocstrings, str, Enum):
    criminal_procedure = 'Criminal Procedure'
    """The rights of people accused or convicted of crimes, and the reading of criminal statutes. Includes habeas
    corpus and post-conviction relief (AEDPA, section 2254 and 2255), search and seizure, Miranda and
    self-incrimination, right to counsel, confrontation, jury trial and jury selection, double jeopardy, the death
    penalty and other Eighth Amendment punishment claims, sentencing (the Guidelines, ACCA and other enhancements),
    restitution, Eighth Amendment claims about how convicted prisoners are treated, the Second Amendment and the
    right to bear arms (including challenges to felon-in-possession and other gun laws, civil or criminal), the
    Federal Rules of Criminal
    Procedure and of evidence at a criminal trial, and what a federal criminal statute covers (fraud, firearms,
    drugs, immigration crimes, obstruction). Not a claim that a criminal statute is void for vagueness, or other
    due-process limits on what a state may make a crime (Due Process)."""

    civil_rights = 'Civil Rights'
    """Discrimination and the rights of classes of people, other than under the First Amendment. Includes equal
    protection; race, sex, age and national-origin discrimination; employment discrimination (Title VII, ADEA);
    disability rights (ADA, Rehabilitation Act); affirmative action; voting, the Voting Rights Act, redistricting
    and ballot access, and presidential electors; immigration, deportation and removal, asylum, and citizenship,
    including whether courts may review immigration decisions; American Indians and tribes, including the authority
    of tribal courts and tribal police; children, including custody and international child abduction; military
    service members and veterans; Social Security and welfare benefits; the procedural rights of indigent people;
    and damages suits against state and local officials under section 1983, including qualified immunity."""

    first_amendment = 'First Amendment'
    """Freedom of speech, press, assembly and religion. Includes free speech and its limits, protest, campaign
    finance, commercial speech (other than lawyers' advertising), defamation, obscenity, free exercise of religion,
    the Establishment Clause, and government aid to religious schools."""

    due_process = 'Due Process'
    """Due process guarantees other than the criminal-trial rights. Includes notice and hearing before the
    government takes something away, the hearing rights of government employees, the due-process rights of pretrial
    detainees and prisoners (including force used against a detainee), vagueness challenges to criminal statutes and
    other due-process limits on what a state may make a crime, an impartial decision maker, a state court's personal
    jurisdiction over an out-of-state defendant, and the Takings Clause and other government taking of property:
    eminent domain and condemnation, and forfeitures, including under the Excessive Fines Clause."""

    privacy = 'Privacy'
    """Personal privacy and autonomy, and access to government records. Includes a constitutional right of privacy,
    abortion and contraception, the right to die, and the Freedom of Information Act and similar disclosure
    statutes."""

    attorneys = 'Attorneys'
    """Lawyers and officials as such. Includes who pays attorney's fees -- fee-shifting under any statute, whatever
    the underlying subject -- and the compensation of lawyers and government officials, admission to the bar,
    attorney discipline and disbarment, and lawyers' advertising."""

    unions = 'Unions'
    """Organized labor and labor-management relations. Includes the National Labor Relations Act, collective
    bargaining, union elections, strikes and picketing, union-member disputes, union trust funds, the Fair Labor
    Standards Act (wages and overtime), workplace safety (OSHA), railroad workers' benefits, and arbitration between
    employers and employees, including under the Federal Arbitration Act."""

    economic_activity = 'Economic Activity'
    """Business, commerce and economic regulation. Includes antitrust and mergers, bankruptcy, securities, ERISA,
    arbitration of commercial disputes, consumer protection statutes, patents, copyrights and trademarks,
    environmental and natural-resource regulation, statutory liability and punitive damages, suits against the
    federal government or federal officers for damages (the Federal Tort Claims Act, Bivens), suits against foreign
    governments (the Foreign Sovereign Immunities Act), state and local
    taxes and state regulation of business, transportation, energy, utility and communications regulation,
    government corruption statutes, zoning, and employee suits against employers that are not discrimination or
    labor-union cases."""

    judicial_power = 'Judicial Power'
    """How courts exercise their own power. Includes federal-court jurisdiction (subject-matter, appellate and the
    Supreme Court's own), standing, mootness and ripeness, whether a private right of action exists, the Federal
    Rules of Civil and Appellate Procedure and civil evidence, class actions, venue, removal, res judicata and
    collateral estoppel, timeliness of filings, abstention and comity toward state courts, judicial review of
    administrative agencies (the APA, deference to agency interpretations), and remedies such as injunctions."""

    federalism = 'Federalism'
    """The relationship between the federal government and the states, other than between federal and state courts.
    Includes preemption of state law by federal law, state sovereign immunity and the Eleventh Amendment, the limits
    of Congress's enumerated powers (commerce, spending, necessary and proper, enforcement) -- including a claim that
    a federal criminal statute exceeds the Commerce Clause -- intergovernmental tax immunity, and
    federal-state disputes over land or resources, and the federal government's relationship with territories such
    as Puerto Rico."""

    interstate_relations = 'Interstate Relations'
    """Disputes between states. Includes boundary disputes, water rights and other disputes over property between
    states, typically in the Supreme Court's original jurisdiction."""

    federal_taxation = 'Federal Taxation'
    """The Internal Revenue Code and other federal tax statutes, in civil tax disputes. Not tax crimes (Criminal
    Procedure) and not state or local taxes (Economic Activity)."""

    miscellaneous = 'Miscellaneous'
    """Separation of powers between Congress and the President that does not fit any other area, including the
    legislative veto, the President's power to remove officers, and other disputes over executive power."""

    private_action = 'Private Action'
    """Common-law disputes between private parties: real and personal property, contracts, wills and trusts,
    commercial transactions, and common-law torts such as negligence or products liability under state or maritime
    law, even when a constitutional defense is raised to them."""


class Classification(BaseModel):
    area: Area = Field(description=(
        'Which subject-matter area is this United States Supreme Court case about? Judge by what the '
        'controversy is about, as the questions presented state it, not by the procedural posture.'))


# The Database's issueArea code for each label, for scoring.
SCDB_CODE = {a: i for i, a in enumerate(Area, start=1)}
