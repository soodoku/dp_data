# Baseline positions and group affiliations

`Rscript --vanilla scripts/build_baseline_positions.R` writes a typed handoff in
`output/baseline_positions/` for the other-knowledge coding pilot. This additive
export does not replace the canonical analysis tables. It records recruitment
Euro preference in Denmark (`s_15`), pre-event party identification in America
in One Room (`D1`), and pre-event religious affiliation in Northern Ireland
(`t1q31`, with `t1q30` identifying an explicit no-religion response).

One row is a canonical source respondent and measure, keyed by `poll_id`,
`source_dataset`, `respondent_id`, and `measure_id`. The export preserves the
original value, auxiliary screening column/value, source column/row/wave,
data and dictionary hashes, and recode evidence.
Missing values, nonanswers, undecided respondents, independents, and other
religions remain distinguishable. It does not infer beliefs from membership.

Northern Ireland control-source records can include returners also represented
in the Cor-Sood source. Those namespaces remain separate upstream. Downstream
users must apply the documented person bridge and avoid double counting them.
True follow-up-only controls have no recruitment religion measure here; a
later religion answer is not backfilled as if it preceded the event.

The builder verifies raw-source hashes, matches Denmark DELNR and Northern
Ireland CSERIAL exactly, and preserves A1R's immutable file-row identity.
It checks the participant-input hash against its canonical manifest and verifies
the Parquet round trip. The accompanying coverage table counts source records,
not necessarily distinct people across source namespaces. Forced Euro preference,
party leaners, religious background, and post-event identity are not substituted
for the specified baseline measures.
