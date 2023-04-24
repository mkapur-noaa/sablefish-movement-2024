# sablefish-movement
Northeast Pacific sablefish movement analysis

## Overview
This research compendium uses an R [targets](https://github.com/ropensci/targets)
workflow to quantify sablefish movement rates among geographic regions in the northeast Pacific.

## Dependencies
Most package dependencies can be installed from CRAN, but some need special installation:

```{r}
devtools::install_github("seananderson/ggsidekick")
devtools::install_github("luke-a-rogers/mmmstan")
```

## Use
The R targets workflow is found in the _targets.R file. To run the workflow, first load the targets library via

```{r}
library(targets)
```

then optionally inspect the node statuses and dependencies via

```{r}
tar_visnetwork()
```

and finally, update the outdated or errored nodes in the workflow via

```{r}
tar_make()
```

## Output
The targets workflow exports figures to the figs/ folder. All other nodes are
stored as R objects outside the working environment and can be accessed from the R console by calling the `tar_read()` function.
For example, 

```{r}
tar_read(year_start)
```

## Data
The data for the workflow are stored in the data/ folder. These data were assembled
using the corresponding R scripts in the data-raw/ folder, which load raw data stored outside this 
repository.
