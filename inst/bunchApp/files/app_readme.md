# bunchApp - ReadMe
Itai Trilnick, UC Berkeley, December 2016

## What does bunchApp do?
This app allows you to explore bunching analysis. The interactive setting lets you play with simulated earnings under different scenarios, and use different settings to estimate the elasticity of earnings w.r.t. the marginal tax rate. I hope this app is helpful for learning and understanding the bunching techniques. For a review on the bunching methodology, see:
Kleven, H J (2016). _"Bunching"_, Annual Review of Economics, 8(1).

## Parameters You Can Play With

### Simulation Parameters

An earning vector is simulated using the following parameters:

- **\# Observations**: Choose one. Larger population allows more precise estimates.
- **Kink Point**: Choose the kink (or notch) point: at what earning level does the Marginal Tax Rate changes, or a one time Tax (such as loss of benefits) is effective.
- **Marginal Tax Rate Before**: Marginal Tax Rate before kink.
- **Marginal Tax Rate After**: Marginal Tax Rate After kink.
- **Tax at Notch**: one time tax for surpassing the critical earning level.
- **Earning Elasticity w.r.t MTR**: Fixed for everyone.
- **Noise Standard Deviation**: Random disturbance with mean zero and SD of choice to the simulated earning vector. Can be set to zero.

### Estimation Parameters

- **Bin Witdh**: for the histogram.
- **Sum Zero Integral**: For a kink, this corrects the counter-factual distribution, recognizing that the extra bunching needs to be re-distributed on the counter-factual area to the right of the kink. Choosing "No" will set the counter-factual distribution using bin height only. In most cases, it will not make a big difference. For a notch, "Yes" means that the program seeks the notch size that keeps the total histogram area equal. Note that this will vary with your setting of excluded area! Choosing "No" sets the end of the notch at the right edge of the excluded area (see below).
- **Counter-Factual Range (bins around kink)**: Set the range for calculating the counter-factual earning distribution.
- **Excluded Range (bins around kink)**: Set the excluded area, where bunching or a notch are visible.


## Where is bunchApp?
The app lives [here](http://trilnick.shinyapps.io/bunchapp). If it gets too popular, it might go to sleep for the rest of the month. If you are an R user, please consider downloading the package (`install.packages("bunchr")`) and using it offline.

## What's under the hood?
`bunchApp` is part of `bunchr`, an R package for bunching analysis. Download it directly (`install.packages("bunchr")`) or from the [development repository](https://github.com/trilnick/bunchr). The app was developed with [Shiny](http://shiny.rstudio.com/).

## Who made this?
My name is Itai Trilnick. I'm a Ph.D candidate at the Agricultural and Resource Economics department at UC Berkeley. Contact me at *itai.trilnick at berkeley.edu*

## I found a bug / I have a great idea to improve bunchApp
Please [share it with me](http://github.com/trilnick/bunchr/issues). Thanks!
