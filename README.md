# Folders and Files
## Analysis Files
Contains set up for running one iteration of the EM algorithm at a specific seed (1066) for up to R1 + R2 = 6. Can be formatted to loop through seed-list, as found in Analysis Files in Results folders.
## Results
Results for combinations R1-R2. Unnested folders (e.g. R1.1_R2.1) contain results for 9 POS. Files containing **HJ123** include Hebrews + 1, 2, 3 John. 

*Folder* named **9-POS** contains only initial solo runs (i.e. single-shot) for R1-R2 combinations on d1 = 7, plus some d1 = 10 variations. 

Folder **6-POS** contains results for 6-POS R1-R2 on 13 book corpus. 13 book corpus + Hebrews + 1, 2, 3 John on 6-POS (R1 = 2, R2 = 1) found in Results/HJ123_R1.2-R2.1/EPS_10e-6.

**Result file nomenclature** (e.g. EMR.HJ123-R1.2-R2.1_NEW_1066.rds)
1. EMR - EM algorithm
2. HJ123 - appendix denoting additional books to the 13 book Pauline corpus
3. R1.2-R2.1 - Number of R1 and R2 regimes/styles
4. NEW - New eps (i.e. 10e-6). Absense indicates eps = 0.001.
5. 1066 - Seed number for iteration

## POS Files
POS files for different sets of book:
- paul_6pos.rds = 13 book Pauline corpus on 6 POS tags.
- paul_9pos.rds = 13 book Pauline corpus on 9 POS tags.
- paul_6pos-HJ123.rds = 13 book Pauline corpus + Hebrews (attributed to Paul) and 1, 2, 3 John (Johannine letters).
- paul_9pos-plus.6test.rds = 13 book Pauline corpus + Hebrews + James + 2 Peter + 1, 2, 3 John on 9 POS tags. Subset out 16 (James) and 17 (2 Peter) to match 6 POS equivalent.

## Files
- hmm-paul.R = EM algorithm.
- Regime.Plots.R = Sequence and aggregate plots.
- RandIndex.R = Rand Index calcuations for each combination in the 25 iterations.
- Analysis_Combined.R = Examines BIC values for all tested R1-R2 combinations. Generates some objects necessary for Regime.Plots.R (though they might be calculated internally to Regimes.Plots.R).

# Process
1. Load EM functions from hmm-paul.R.
2. Download the desired POS file.
3. Download the desired looped Analysis File from the proper folder in **Results**.
4. Run Analysis file, which, if looped, will automatically output properly named .RDS files.
