# montagu-demography-tools

These are scripts that work on the demographic data tables in montagu-db - see montagu-demography.

## igme_wpp_comparison

Run generate.R to add these graphs into a `graphs/` folder, for each country in the IGME dataset.

  1. Under-5 mortality rate (U5MR), comparison of IGME (lower, median, upper), and UNWPP.
  2. Under-1 mortality rate (IMR), comparison of IGME (lower, median, upper), and UNWPP.
  3. 28-day neonatal mortality rate (NMR) as a fraction of IMR. (IGME-NMR-median/UNWPP-IMR-median, and IGME-NMR-x/IGME-IMR-x)
  4. Same as 3, but plotted against the denominator IMR on the x-axis, instead of against year.
    
