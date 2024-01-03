# vusa
A package with a collection of R functions

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

## Contributing

Contributions to the VUSA package are welcome. If you have a function that you believe would be beneficial to others, feel free to submit a pull request. Please ensure that your function adheres to the guidelines and restrictions outlined above.
