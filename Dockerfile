# 1. Base Image (amd64 emulation required for rocker/shiny-verse on M1 Mac)
# Pin R to 4.4.2 to match renv.lock — :latest floats to R 4.6.1 and breaks base64enc/SETLENGTH.
FROM rocker/shiny-verse:4.4.2

# 2. System Libraries
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    --option Acquire::Retries=3 \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    cmake \
    curl \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup App Directory
RUN mkdir -p /srv/shiny-server/openstats
WORKDIR /srv/shiny-server/openstats

# 4. Install renv
RUN R --no-save --no-restore -e "install.packages('remotes', repos='https://cran.r-project.org', timeout=300); remotes::install_version('renv', '1.1.5', repos='https://cran.r-project.org', timeout=300)"

# 5. Copy renv configuration
COPY renv.lock ./
COPY renv/activate.R ./renv/
COPY .Rprofile ./

# 6. Restore Packages — CHANGED to the __linux__/noble binary repo (was cran/latest = source-only)
RUN R --no-save --no-restore -e "\
  options(timeout=1200, repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); \
  renv::restore(prompt = FALSE)"

# 7. Copy the rest of the Application Code
COPY . /srv/shiny-server/openstats/

# 8. Expose the port
EXPOSE 3838

# 9. Run the Application — CHANGED from a duplicate RUN renv::restore to the actual start command
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/openstats', host = '0.0.0.0', port = 3838)"]