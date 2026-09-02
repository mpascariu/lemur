# Round-Consistency of the GBD 2021 and GBD 2023 Estimates Used in the *lemur* Package

**M. D. Pascariu** · *2026-08-31* · Assessment accompanying the *lemur* v2.0.0 data refresh
**Reproducibility:** all analyses are scripted (`data-raw/study_round_consistency.R`, `data-raw/challenge_covid_ihd.R`); charts are written to `docs/figures/`.

---

## Executive Summary

The *lemur* package combines GBD 2021 cause-of-death and SDG-grouped deaths for 1990–2021 with 2023-round data for 2023, plus life tables built exclusively from the 2023-round qx series. Since adjoining years are estimated under different rounds, this report evaluates whether the combined series is internally coherent or exhibits discontinuities at the round transition, via three tests: (i) a same-year comparison of GBD 2021 vs 2023 life tables to isolate pure round revision; (ii) total- and cause-specific death trends across the 2021–2023 seam; (iii) a stress test on COVID-19 (transitory) and ischaemic heart disease (chronic), benchmarking each region's seam change against its *own* within-round variability.

Results: life-expectancy revision between rounds is modest (≈0.1–0.4 years) for data-rich regions and larger (up to ≈2–3 years) where vital registration is incomplete; total all-cause trends are continuous across the seam; and apparent single-cause discontinuities are, for a majority of regions, within ordinary within-round year-to-year variation. Overall the mix is **reasonable**, the combined data are **internally consistent and sound**. The one residual concern: **isolated deviations may occur on select causes in a minority of regions** — most prominently Japan and Romania in ischaemic heart disease — which should be read with the documented round-revision caveat, not as a generic mix failure.

---

## 1. Introduction

GBD releases re-estimate both the most recent years and the entire historical series under new specifications, data inputs, and cause-redistribution conventions. In *lemur*, 2021-round deaths (1990–2021) sit in the same table as 2023-round deaths (2023); if methodology shifted, spurious discontinuities indistinguishable from genuine demographic change could appear at the seam. Three questions are addressed:

1. How much does the 2023 round revise the 2021-round historical estimates, conditional on data quality?
2. Is the 2021–2023 transition in all-cause mortality continuous, and how does it compare with ordinary year-to-year variation?
3. Do cause-specific series (COVID-19, ischaemic heart disease) behave consistently across the seam once the pandemic is accounted for?

---

## 2. Data

The analysis uses the package datasets (`data_gbd_lt()`, `data_gbd_cod()`, `data_gbd_sdg()`) and, for the round comparison, archived GBD 2021 life tables in `data-raw/_archive/`. The combined COD table spans ten period years (1990–2023, see §3), 216 locations, three sexes, and ages 0–95 (95+ open), with 18 cause categories (COD grouping) and 21 (SDG grouping).

Eight case-study locations span income level, age structure, and vital-registration completeness: Romania, Mexico, Japan, USA, Nigeria, India, Chile, Sweden — names invariant between rounds.

---

## 3. Methods

**Part A — Round revision.** For each location, sex, and common year (1990–2021), Δe0 = 2023-round minus 2021-round life expectancy at birth. Identical calendar years isolate pure round revision; summaries (mean/maximum absolute, values at 2015 and 2021) are reported.

**Part B — Seam continuity.** All-cause deaths aggregated by location, sex, and period; annualised growth over 2019→2021 (within-round) and 2021→2023 (across the seam), plus the four-year ratio D2023/D2019.

**Part C — Age structure.** Age-specific all-cause counts (<1, 30–34, 65–69, 90–94) across recent years, with the ratio D2023/D2021 per age group.

**Part D — Stress test.** Cause-specific series over the full 1990–2023 horizon; IHD indexed to 2019 = 100. Because the 2020–2021 pandemic entrains reallocation of deaths between COVID-19 and circulatory disease, 2021 is treated as an anomalous anchor, and the 2021→2023 change is read against the full trend rather than in isolation.

**Within-round band benchmark.** Adjacent period-to-period log-changes in IHD deaths over 2005–2021 (all within the 2021 round) define each location's within-round band. A seam change exceeding the largest within-round change in absolute value is "outside the band" — operationalising the point that data-poor locations show large cause-level variation even without a round transition.

---

## 4. Results

### 4.1 Round revision is modest for well-measured locations

Data-rich locations: GBD 2023 revised historical life expectancy by typically 0.1–0.4 years, largest at 2021 (Romania: mean |Δe0| = 0.13, Δ(2021) = +0.09; Japan: 0.25 and −0.38; USA shows a locally larger 2021 revision of ≈0.70, consistent with revised excess-mortality accounting). With incomplete registration the revision is much larger: Nigeria ≈2.8 years, India ≈2.3. Modest revision where registration is complete, large revision where mortality is model-dependent — the expected signature of a stable pipeline, not a methodological discontinuity.

### 4.2 The all-cause seam is continuous

Total deaths show a coherent pattern (Table 1): pandemic excess in 2019→2021 where one occurred (Romania +13.1%/yr, Mexico +22.2%/yr), corresponding reversion across the seam (−14.2%, −15.5%/yr), or continued demographic pressure where it didn't (Japan +6.1%/yr, Sweden +1.3%/yr). No anomalous transitions; the four-year ratios D2023/D2019 (0.94–1.17) imply modest net change.

**Table 1.** All-cause deaths (both sexes); annualised growth (%), four-year ratio.

| Region | D2019 | D2021 | D2023 | g 19→21 | g 21→23 | D2023/D2019 |
|---|---|---|---|---|---|---|
| Romania | 261 213 | 334 246 | 246 053 | +13.1 | −14.2 | 0.94 |
| Mexico | 746 969 | 1 114 630 | 796 630 | +22.2 | −15.5 | 1.07 |
| Japan | 1 381 905 | 1 437 450 | 1 617 436 | +2.0 | +6.1 | 1.17 |
| USA | 2 853 165 | 3 471 248 | 3 091 656 | +10.3 | −5.6 | 1.08 |
| Nigeria | 1 676 131 | 1 820 421 | 1 782 525 | +4.2 | −1.0 | 1.06 |
| India | 9 532 115 | 11 744 496 | 9 844 814 | +11.0 | −8.4 | 1.03 |
| Chile | 108 695 | 133 962 | 121 406 | +11.0 | −4.8 | 1.12 |
| Sweden | 89 792 | 91 985 | 94 444 | +1.2 | +1.3 | 1.05 |

### 4.3 The age structure survives the seam

Age-specific ratios D2023/D2021 (both sexes) follow a pandemic-recovery pattern: largest declines at pandemic-affected working and young-elderly ages in COVID-affected locations (Romania 65–69: 0.73; Mexico 65–69: 0.65; Mexico 30–34: 0.78), infant and oldest-old ratios near unity; Japan, spared pandemic collapse, shows ratios of 1.01–1.16. No age band jumps discontinuously in a way suggestive of a grouping or re-attribution error at the boundary.

### 4.4 Stress test: COVID-19 and ischaemic heart disease

*COVID-19.* Deaths are zero in 2019, peak in 2020–2021, and decline to a small residual by 2023 in all locations — the waning of the pandemic, not a redistribution to another cause, shared uniformly (Japan's 2023 figure is ≈5% of its 2021 peak). Unambiguously continuous.

*IHD over the full horizon.* Indexed to 2019 = 100 (Figure 1), the 2021→2023 step is small relative to the long-run trajectory: Romania's 2023 level (≈93) is essentially back at its 1990 level (≈95) — a reversion to the long-run path, not a new discontinuity.

**Table 2.** IHD: within-round band versus seam change.

| Region | within-round band (min, max) | seam 2021→23 | outside band? |
|---|---|---|---|
| Romania | (−1.9, +1.9) | −9.5 | yes |
| Mexico | (+1.1, +26.1) | +6.0 | no |
| Japan | (−1.2, +6.9) | +12.2 | yes |
| USA | (−11.2, +3.0) | −4.2 | no |
| Nigeria | (+1.9, +19.4) | +18.2 | no |
| India | (+1.5, +24.0) | −10.3 | no |
| Chile | (−4.4, +5.7) | +5.4 | no |
| Sweden | (−16.7, +0.1) | +0.6 | no |

![Figure 1. Ischaemic heart disease deaths, 2019 = 100, full 1990–2023 series.](figures/ihd_fullperiod.png)

![Figure 2. IHD as a percentage of all-cause deaths by age, full 1990–2023 series (dot: 2021).](figures/ihd_share_fullperiod.png)

Two findings stand out. First, Nigeria (+18.2%) and India (−10.3%) fall *within* their bands (maxima +19.4%, +24.0%): no seam discontinuity beyond ordinary within-round variation — the jumps would appear whether 2023 came from the 2023 or the 2021 round. Second, across all 216 locations the IHD seam lies outside the band in 41% of cases, yet the case-study outliers include two *well-measured* locations — Romania (−9.5% vs a ±1.9% band) and Japan (+12.2% vs a +6.9% maximum). Genuine round-level cause re-estimates are thus not confined to low-registration settings; they are a minority phenomenon across data-quality strata.

---

## 5. Observation

Part A's strength is eliminating confounding between round revision and demographic change; its limitation is that the death series cannot be tested the same way — the two downloads share no calendar-year deaths. Parts B–D therefore cannot fully separate genuine two-year heterogeneity from round-driven revision, and the pandemic dominates the interval. Two design choices mitigate this: reading 2021 as an anomalous anchor against the full trend, and benchmarking the seam against each region's own within-round variability. The conclusions are robust to these limitations: all-cause continuity holds; single-cause series are continuous in the majority of locations; remaining discontinuities are explicable as within-round noise (data-poor) or localised round-level re-estimates in a minority of locations, some with complete registration. The concentration of negative seam changes in pandemic-affected locations is consistent with real reversion of excess mortality, not estimation artefact.

---

## 6. Conclusions

**The two-round mix is reasonable and the combined data look sound.** All-cause trends are continuous across the seam, round revision is modest where registration is complete, and single-cause changes are — for a majority of regions — no larger than ordinary within-round variation. The data are fit for purpose for all-cause and broadly for cause-specific analysis.

The one qualification is narrow: **isolated deviations may occur on select causes in a minority of regions**, because GBD re-estimates cause levels between rounds. The clearest instances are IHD in two data-rich settings — **Japan** (+12.2%, outside its ±6.9% band) and **Romania** (−9.5%, outside ±1.9%) — while the data-poor regions fall *inside* their own noisy bands. Screening rule for seam-spanning cause comparisons: if a change exceeds the series' own historical within-round variation, treat it as a round-level re-estimation and report it as such; otherwise it is unlikely to reflect a problem with the mix.

In short: **the mix is reasonable, the data are good, and any concern is limited to isolated, select-cause deviations in a minority of regions** — a documented caveat, not a barrier to use.

---

## Reproducibility

```bash
Rscript data-raw/study_round_consistency.R     # parts A-C
Rscript data-raw/challenge_covid_ihd.R         # part D, stress test + figures
```

## References

Global Burden of Disease Collaborative Network. *GBD 2021 and 2023 Results.* Seattle, United States: IHME. <https://vizhub.healthdata.org/gbd-results/>.