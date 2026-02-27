# Plot available years for a given area

Plot available years for a given area

## Usage

``` r
plot_photos(x)
```

## Arguments

- x:

  Object of class `sf` or `sfc`

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
library(happign)
x <- get_apicarto_cadastre("29158")
photos <- plot_photos(x)
} # }
```
