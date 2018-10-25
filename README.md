## Decomposing an instrument/ portfolio into exchange rate and non exchange rate effects.

If you are someone with a stake in foreign positions, this package I wrote here may be a useful tool to help you understand the impact of foreign currency on your positions. For instance,

- If you are an investor, you may use it to analyze impact of exchange rate on your invesment positions.
- If you are in the treasury department, you may wish to analyze the impact of exchange rates on your bonds.
- If you are in the finance department, you could analyze the exchange rate impact on your foreign revenue.

To start using this package, you may first install the devtools package and execute the following command. install_github("jironghuang/RemoveExchangeRateEffects").

You may follow the example to better understand how this package works. 

In summary, what the example does below is to decompose 1 instrument position in SGD (column value) - from the perspective of someone staying in Singapore - into local static value (i.e if I keep the exchange rate constant at the start of the period) and the residual exchange rate impact. 

If you look at the value at the end of the period (Oct 2018), you would notice that the value in SGD fell from 331 to 261. From the perspective of a Singaporean local - through this package -  we can understand that the appreciation in USD negate the fall in value by 4 SGD.       

## Quick example in R codes

```
library(devtools)
install_github("jironghuang/RemoveExchangeRateEffects")
library(RemoveExchangeRateEffects)
sp_exch_rate_pair = "USDSGD=X"
ap_start_date <- as.Date("1950-01-01")
ap_end_date <- as.Date("2020-10-01")
np_mthly_yearly = "monthly"  #alternatively this could be "yearly""
data(instrument)
dp_dates_investment_value = instrument
o_exchRate_effect <- exchange_rate_decomposition(sp_exch_rate_pair, ap_start_date, ap_end_date, np_mthly_yearly, dp_dates_investment_value)
o_exchRate_effect$get_portfolio()
```

```
      value_in_sgd Adj_Close fgn_value local_static_value exch_rate_impact
Oct 2017 331.53   1.36010  243.7541           331.5300        0.0000000
Nov 2017 308.85   1.34670  229.3384           311.9231       -3.0731344
Dec 2017 311.35   1.33780  232.7328           316.5399       -5.1899425
Jan 2018 354.31   1.31168  270.1192           367.3892      -13.0791734
Feb 2018 343.06   1.32433  259.0442           352.3260       -9.2660108
Mar 2018 266.13   1.31090  203.0132           276.1183       -9.9882495
Apr 2018 293.90   1.32577  221.6825           301.5104       -7.6103599
May 2018 284.73   1.33850  212.7232           289.3248       -4.5948212
Jun 2018 342.95   1.36830  250.6395           340.8948        2.0552438
Jul 2018 298.14   1.36140  218.9952           297.8553        0.2846937
Aug 2018 301.66   1.36700  220.6730           300.1374        1.5226438
Sep 2018 264.77   1.36732  193.6416           263.3719        1.3980921
Oct 2018 260.95   1.38080  188.9847           257.0380        3.9119824

o_exchRate_effect$get_diff_portfolio_value()
[1] -3.911982
```
