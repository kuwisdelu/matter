# matter

Out-of-memory dense and sparse signal arrays

Memory-efficient file-based data structures for dense and sparse vectors, matrices, arrays, and lists with applications to nonuniformly sampled signals and spectra.

## Description

The *Matter* package provides flexible data structures for out-of-memory computing on dense and sparse arrays, with several features designed specifically for computing on nonuniform signals such as mass spectra and other spectral data.

*Matter 2* has been updated to provide a more robust C++ backend to out-of-memory `matter` objects, along with a completely new implementation of sparse arrays and new signal processing functions for nonuniform sparse signal data.

## Installation

### Bioconductor Release

*Matter* can be installed via the *BiocManager* package.

```{r install, eval=FALSE}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("matter")
```

The same function can be used to update *Matter* and other Bioconductor packages.

Once installed, *Matter* can be loaded with `library()`:

```{r library, eval=FALSE}
library(matter)
```

### Bioconductor Devel

The Bioconductor development version of *Matter* can also be installed via the *BiocManager* package.

```{r install, eval=FALSE}
BiocManager::install("matter", version="devel")
```

This version is unstable and should not be relied on for critical work. However, it is typically more stable than Github version.

### Github Devel

The most cutting edge version of *Matter* can be installed from Github via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::github_install("kuwisdelu/matter")
```

This version is unstable and only recommended for developers.

