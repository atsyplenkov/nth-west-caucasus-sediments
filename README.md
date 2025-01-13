[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12816100.svg)](https://doi.org/10.5281/zenodo.12816100)

# Impacts of post-Soviet land-use transformation on sediment dynamics in the Western Caucasus
![](/figures/graphical-abstract/graphical_abstract_rivers.png)

This repository contains the main processing steps to explore sediment load temporal changes in the Kuban River basin (Western Caucasus) during the 1975-2020 period. It is meant to accompany a journal article (Tsyplenkov et al., *In print*). This study is part of the Russian Science Foundation project No. 19-17-00181: "Quantitative assessment of the slope sediment flux and its changes in the Holocene for the Caucasus mountain rivers".

For a deep introduction to the study, please refer to:
>Tsyplenkov A, Grachev A, Yermolaev O, Golosov V. **Impacts of post-Soviet land-use transformation on sediment dynamics in the Western Caucasus**. *Journal of Hydrology*, *In print*.

# Highlights
> - This study investigates sediment transport dynamics over the last five decades in the western Greater Caucasus, focusing on the Krasnodar Reservoir, using repeated bathymetric surveys and gauging station measurements.
> - The results indicate a progressive increase in erosion rates from 1975 to the period of 1993–2005, followed by a subsequent decline that persists to the present day.
> - The collapse of the Soviet Union and subsequent land-use changes, especially in arable and pastoral lands, were identified as primary drivers of sediment transport alteration, with significant shifts observed between 1995 and 2000.
> - The Top-Kriging modelling technique was effectively used to extend suspended sediment yield (SSY) records and predict SSY in ungauged basins, demonstrating good accuracy (NSE = 0.63) in cross-validation and comparison with independent reservoir sedimentation records

# Documentation

The repository is organized as follows:
```
├── analysis
├── data
├── figures
├── R
├── tables
└── workflow
```
Here's how this repo is organized:
- `analysis`: Where all the juicy results live, mosty for internal use
- `data`: Home to our raw data. However, some data is too big to be uploaded and can be shared upon request
- `figures`: You guessed it - all the visualizations
- `tables`: Where we keep our data tables nice and tidy
- `R`: The R scripts that make everything work internally
- `workflow`: Step-by-step instructions on how everything fits together

Some maps, as well as manual shoreline digitizing, were performed in QGIS 3.34 LTS version. The corresponding `.qgz` project files are uploaded. The QGIS projects are missing some heavy satellite imagery, as well as DEM. Files can be shared upon request.

# How to reproduce?

This project can be reproduced either directly on your machine or using Docker/Podman containers. We used the [{renv} package](https://rstudio.github.io/renv/articles/renv.html) to create a stable version-specific library of R packages. The work itself was done on Ubuntu 22.04 in R `4.4.1`.

## Option 1: Direct installation

1. Make sure that R `4.4.1` and `{renv}` packages are installed
2. Clone the repository and navigate to its directory
3. Run `renv::restore()` to install all required packages
4. The folder `workflow` contains all necessary steps to reproduce the results. Follow inner folders in order they are named to produce the results. Folder `R` contains additional functions that were written to support the analysis.

## Option 2: Using Docker/Podman

1. Clone the current repository:
```shell
git clone https://github.com/atsyplenkov/nth-west-caucasus-sediments.git
```

2. Navigate to the downloaded directory:
```shell
cd nth-west-caucasus-sediments
```

3. Build the container image:
```shell
podman build -t nth-west-caucasus-sediments-image -f Dockerfile
```
The Dockerfile is configured to install all the required packages and restore the `renv` environment.

4. Create and run a container with the mounted local directory:
```shell
podman run -d --name nth-west-caucasus-sediments \
  --mount "type=bind,src=$(pwd),target=/nth-west-caucasus-sediments" \
  -p 8080:80 nth-west-caucasus-sediments-image
```

This command mounts your current working directory (the cloned repository) to the `/nth-west-caucasus-sediments` directory inside the container, allowing you to access and modify files from both the host and container. 

5. Open the container:
```shell
podman exec -it nth-west-caucasus-sediments R
```

# Related reproducible research by our team
- Study describing suspended sediment spatial patterns in the Greater Caucasus mountains (Golosov & Tsyplenkov, [2021](https://github.com/atsyplenkov/caucasus-sediment-yield2021))
- Spatio-temporal assessment of suspended sediment changes during the Anthropocene in the adjacent Terek River basin (Tsyplenkov et al., [2021](https://github.com/atsyplenkov/sediment-caucasus-anthropocene))
