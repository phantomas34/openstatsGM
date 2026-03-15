FROM --platform=linux/amd64 rocker/shiny-verse:4.4.2

RUN apt-get update && apt-get install -y \
    wget \
    ca-certificates \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    cmake \
    libnlopt-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && update-ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN echo 'options(timeout=3600, download.file.method="wget", download.file.extra="--tries=20 --waitretry=5 --retry-connrefused")' >> /usr/local/lib/R/etc/Rprofile.site

RUN mkdir -p /srv/shiny-server/openstats
WORKDIR /srv/shiny-server/openstats

RUN install2.r --error --skipinstalled -n 1 -r "https://cloud.r-project.org" \
    lazyeval

RUN R -e "options(timeout=3600, download.file.method='wget', download.file.extra='--tries=20 --waitretry=5 --retry-connrefused'); for (i in 1:8) { message(sprintf('S7 install attempt %d/8', i)); try(suppressWarnings(install.packages('S7', repos='https://cloud.r-project.org')), silent=TRUE); if (requireNamespace('S7', quietly=TRUE)) quit(save='no', status=0); Sys.sleep(8) }; stop('Failed to install S7 after retries')"

RUN install2.r --error --skipinstalled -n 1 -r "https://cloud.r-project.org" \
    ggplot2 \
    dplyr \
    tidyr \
    scales \
    magrittr \
    rlang \
    rsconnect

RUN R -e "options(timeout=3600, download.file.method='wget', download.file.extra='--tries=20 --waitretry=5 --retry-connrefused'); for (i in 1:8) { message(sprintf('crosstalk install attempt %d/8', i)); try(suppressWarnings(install.packages('crosstalk', repos='https://cloud.r-project.org')), silent=TRUE); if (requireNamespace('crosstalk', quietly=TRUE)) quit(save='no', status=0); Sys.sleep(8) }; stop('Failed to install crosstalk after retries')"

RUN R -e "options(timeout=3600, download.file.method='wget', download.file.extra='--tries=20 --waitretry=5 --retry-connrefused'); for (i in 1:8) { message(sprintf('DT install attempt %d/8', i)); try(suppressWarnings(install.packages('DT', repos='https://cloud.r-project.org')), silent=TRUE); if (requireNamespace('DT', quietly=TRUE)) quit(save='no', status=0); Sys.sleep(8) }; stop('Failed to install DT after retries')"

RUN install2.r --error --skipinstalled -n 1 -r "https://cloud.r-project.org" \
    bslib \
    thematic \
    rhandsontable \
    readxl \
    bsicons \
    shinyjs

RUN R -e "options(timeout=3600, download.file.method='wget', download.file.extra='--tries=20 --waitretry=5 --retry-connrefused'); for (i in 1:8) { message(sprintf('shinyWidgets install attempt %d/8', i)); try(suppressWarnings(install.packages('shinyWidgets', repos='https://cloud.r-project.org')), silent=TRUE); if (requireNamespace('shinyWidgets', quietly=TRUE)) quit(save='no', status=0); Sys.sleep(8) }; stop('Failed to install shinyWidgets after retries')"

RUN R -e "options(timeout=3600, download.file.method='wget', download.file.extra='--tries=20 --waitretry=5 --retry-connrefused'); for (i in 1:8) { message(sprintf('shinycssloaders install attempt %d/8', i)); try(suppressWarnings(install.packages('shinycssloaders', repos='https://cloud.r-project.org')), silent=TRUE); if (requireNamespace('shinycssloaders', quietly=TRUE)) quit(save='no', status=0); Sys.sleep(8) }; stop('Failed to install shinycssloaders after retries')"

RUN install2.r --error --skipinstalled -n 1 -r "https://cloud.r-project.org" \
    psych \
    car

COPY . /srv/shiny-server/openstats/

RUN rm -f .Rprofile renv.lock
RUN rm -rf renv/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/openstats', host = '0.0.0.0', port = 3838)"]