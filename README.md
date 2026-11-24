rmmc is an R package for calculating multimarket contact.

It implements seven measures: count, Evans & Kessides (1994), Feinberg (1985), Chen (1996), Baum & Korn (1996), Baum & Korn (1999), and Boeker, Goodstein, Stephan, & Murmann (1997). Each can be computed at the dyad, dyad-in-market, firm-in-market, market and market-firm levels.

The measures follow the definitions in Gimeno, J. and Jeong, E. (2001), "Multimarket contact: meaning and measurement at multiple levels of analysis", in Baum, J.A.C. and Greve, H.R. (Ed.) Multiunit Organization and Multimarket Strategy (Advances in Strategic Management, Vol. 18), Emerald Group Publishing Limited, Bingley, pp. 357-408. https://doi.org/10.1016/S0742-3322(01)18013-7

## Example

```r
library(rmmc)
data <- rmmc:::data   # one row per firm and year, one column per market
markets <- c("m1", "m2", "m3", "m4")

# Firm-in-market count measure, one row per firm and market
mmc_count(data, "id", "year", "firm market", markets)

# The same in wide layout, one column per market
mmc_count(data, "id", "year", "firm market", markets, format = "wide")

# Results for some firms only; all firms still count as rivals
mmc_bgsm(data, "id", "year", "firm market", markets, focal = c("firm1", "firm2"))

# Measures based on sales take a second table with the same layout
mmc_feinberg(data, rmmc:::m, "id", "year", "market", markets)
```

Firms are paired only within the markets they share, so large data sets (thousands of firms and markets) run in seconds.
