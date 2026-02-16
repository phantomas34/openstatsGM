# 1. Base Image: Start with a version of R that has Shiny and the Tidyverse pre-installed
# This saves huge amounts of build time.
FROM rocker/shiny-verse:latest

# 2. System Libraries: Install Linux dependencies required by your specific packages
# (devtools often needs these)
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Create App Directory
RUN mkdir -p /srv/shiny-server/openstats

# 4. Copy renv files first (Caching layer)
# This allows Docker to cache the library installation step if your code changes but libraries don't.
COPY renv.lock /srv/shiny-server/openstats/
COPY .Rprofile /srv/shiny-server/openstats/
COPY renv/activate.R /srv/shiny-server/openstats/renv/

# 5. Install R Packages using renv
WORKDIR /srv/shiny-server/openstats
# We restore the library. 
RUN R -e "renv::restore(prompt = FALSE)"

# 6. Copy the rest of the App Code
COPY . /srv/shiny-server/openstats/

# 7. Expose the port (Shiny runs on 3838 by default in this image)
EXPOSE 3838

# 8. Run the App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/openstats', host = '0.0.0.0', port = 3838)"]