# matter

Scientific computing for out-of-memory signals and arrays

Toolbox for out-of-memory scientific computing and data visualization, providing memory-efficient file-based data structures for dense and sparse vectors, matrices, and arrays with applications to nonuniformly sampled signals and images.

## Description

The *Matter* package provides flexible data structures for out-of-memory computing on dense and sparse arrays, with specialized features designed specifically for computing on nonuniform signals such as mass spectra and other spectral data, as well as hyperspectral images.

*Matter 2* has been updated to provide a more robust C++ backend to out-of-memory `matter` objects, along with a completely new implementation of sparse arrays. Tools for signal processing, dimension reduction, and data visualization are also provided.

## User Installation

### Bioconductor Release

*Matter* can be installed via the *BiocManager* package.

This is the **recommended** installation method.

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

### Github Release

*Matter* can also be installed via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/matter", ref=remotes::github_release())
```

Previous releases can be installed by specifying the exact version.

```{r library, eval=FALSE}
remotes::install_github("kuwisdelu/matter@v2.6.2")
```

## Developer Installation

### Bioconductor Devel

The Bioconductor development version of *matter* can also be installed via the *BiocManager* package.

```{r install, eval=FALSE}
BiocManager::install("matter", version="devel")
```

This version is **unstable** and should not be used for critical work. However, it is typically more stable than Github devel.

This version should *typically* pass `R CMD check` without errors.

### Github Devel

The most cutting edge version of *Matter* can be installed from Github via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/matter")
```

This version is **unstable** and only recommended for developers. It should not be used for critical work.


