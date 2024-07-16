# Impacts of post-Soviet land-use transformation on sediment dynamics in the Western Caucasus
This repository contains the main processing steps to explore sediment load temporal changes in the Kuban River basin (Western Caucasus) during the 1975-2020 period. It is meant to accompany a journal article (Tsyplenkov et al., *In review*). This study is part of the Russian Science Foundation project No. 19-17-00181: "Quantitative assessment of the slope sediment flux and its changes in the Holocene for the Caucasus mountain rivers".

For a deep introduction to the study, please refer to:
>Tsyplenkov A, Grachev A, Yermolaev O, Golosov V. Impacts of post-Soviet land-use transformation on sediment dynamics in the Western Caucasus. Science of the Total Environment, *In review*.

# Overview
We used the [{renv} package](https://rstudio.github.io/renv/articles/renv.html) to create a stable version-specific library of R packages. The work itself was done on an Ubuntu 22.04 WSL2 remote machine in R `4.4.1`. In order to replicate the analysis and the environment, the following steps are recommended:

1. Open the folder in a WSL remote desktop in VS Code.
2. Make sure that R `4.4.1` and `{renv}` packages are installed.
3. Run `renv::restore()` to install all the packages.
4. The folder `workflow` contains all necessary steps to reproduce the results. Follow inner folders in order they named to produce the results. Folder `R` contains additional functions that were written for analysis.