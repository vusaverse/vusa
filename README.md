# vusa
  <!-- badges: start -->
  [![CodeFactor](https://www.codefactor.io/repository/github/vusaverse/vusa/badge)](https://www.codefactor.io/repository/github/vusaverse/vusa)
  [![R-CMD-check](https://github.com/vusaverse/vusa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vusaverse/vusa/actions/workflows/R-CMD-check.yaml)
  [![Coverage Status](https://coveralls.io/repos/github/vusaverse/vusa/badge.svg)](https://coveralls.io/github/vusaverse/vusa)
  <!-- badges: end -->

This is a collection of R functions designed to provide a variety of utilities. However, please note that due to the nature of the functions included in this package, it will not be available on CRAN. The functions contain hardcoded lines, system variables, and functions that may violate CRAN policies. Furthermore, the usage of `.Internals` is also prohibited by CRAN, which further restricts the availability of this package on CRAN.

## Installation

As this package is not intended for distribution via CRAN, you will need to install it directly from this GitHub repository. To do this, you can use the devtools package in R:

```
install.packages("devtools") # If you haven't installed devtools yet
devtools::install_github("vusaverse/vusa")
```

## Usage

After installing the package, you can load it into your R environment using the library() function:

```
library(vusa)
```

Once loaded, you can access the functions contained within the package. For detailed usage instructions and examples, please refer to the individual documentation for each function.

Please note that there may be some untranslated words from Dutch and that running R-cmd-check may fail. However, as a collection, you might find one or two functions that are interesting to you. Feel free to copy the source code to your own projects.

## Reference Page

We are currently working on organizing the reference page on the [reference](https://vusaverse.github.io/vusa/reference/index.html) page to make it easier for users to find relevant functions. 

## Contributing

Contributions to the VUSA package are welcome. If you have a function that you believe would be beneficial to others, feel free to submit a pull request. Please ensure that your function adheres to the guidelines and restrictions outlined above.
