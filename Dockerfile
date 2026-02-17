# 1. Base Image
FROM --platform=linux/amd64 rocker/shiny-verse:latest

# 2. System Libraries
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    cmake \
    curl \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup App Directory
RUN mkdir -p /srv/shiny-server/openstats
WORKDIR /srv/shiny-server/openstats

# 4. Install correct renv version (The Fix)
# We install 'remotes' and then use it to force-install renv 1.1.5.
# We do this BEFORE copying .Rprofile so the auto-loader doesn't crash us.
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org'); remotes::install_version('renv', '1.1.5', repos='https://cloud.r-project.org')"

# 5. Copy renv configuration
COPY renv.lock ./
COPY renv/activate.R ./renv/
COPY .Rprofile ./

# 6. Restore Packages
# Now that .Rprofile is copied, this will restore into the project library (renv/library).
# We use the Posit Package Manager for binary speed, but we set a high timeout just in case.
RUN R -e "options(timeout=300, repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); renv::restore(prompt = FALSE)"

# 7. Copy the rest of the Application Code
COPY . /srv/shiny-server/openstats/

# 8. Expose the port
EXPOSE 3838

# 9. Run the Application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/openstats', host = '0.0.0.0', port = 3838)"]