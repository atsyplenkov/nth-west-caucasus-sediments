FROM rocker/r-ver:4.4.1
LABEL maintainer="Anatoly Tsyplenkov <atsyplenkov@fastmail.com>"

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))"
RUN R -e "pak::pkg_install('renv@1.0.7')"
COPY renv.lock renv.lock
RUN R -e "renv::restore()"
COPY . /nth-west-caucasus-sediments

CMD ["R"]