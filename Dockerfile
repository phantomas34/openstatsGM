# 1. Base Image
FROM --platform=linux/amd64 rocker/shiny-verse:4.4.2

# 2. System Libraries (Provides necessary shared objects for pre-compiled binaries)
RUN apt-get update && apt-get install -y \
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
    && rm -rf /var/lib/apt/lists/*

# 3. Setup App Directory
RUN mkdir -p /srv/shiny-server/openstats
WORKDIR /srv/shiny-server/openstats

# 4. Install Packages (The "Binary Speed" Method)
# Pointing to the '__linux__/noble/latest' repo grabs pre-compiled binaries.
# This prevents your Mac emulator from having to compile C++ code and crashing.
RUN install2.r --error --skipinstalled -n 1 -r "https://packagemanager.posit.co/cran/__linux__/noble/latest" \
    bslib \
    thematic \
    DT \
    rhandsontable \
    car \
    psych \
    readxl \
    shinyWidgets \
    bsicons \
    shinyjs \
    shinycssloaders

# 5. Copy App Files 
COPY . /srv/shiny-server/openstats/

# 6. Neutralize local environments
RUN rm -f .Rprofile renv.lock
RUN rm -rf renv/

# 7. Expose port and Run
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/openstats', host = '0.0.0.0', port = 3838)"]