FROM rocker/shiny:4.4.0

LABEL org.opencontainers.image.authors="julien.barde@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"
#connect this container (GHitHub package) to the repository
LABEL org.opencontainers.image.source https://github.com/firms-gta/darwin_core_viewer

# Update and upgrade the system with option -y to tells apt-get to assume the answer to all prompts is yes.
RUN apt update && apt upgrade -y

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    libssl-dev \
    libcurl4-openssl-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    gdal-bin \
    libgdal-dev \
    libxml2-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libnode-dev \
    zlib1g-dev \
    libsqlite3-dev \
    libjq-dev \
    git \
    make
    
    
       

## update system libraries
RUN apt update && apt upgrade -y && apt clean

#geospatial
# RUN /rocker_scripts/install_geospatial.sh

# Install R core package dependencies (we might specify the version of renv package)
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# FROM ghcr.io/firms-gta/darwin_core_viewer-cache AS base
# Set environment variables for renv cache, see doc https://docs.docker.com/build/cache/backends/
ARG RENV_PATHS_ROOT

# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

# Set the working directory
WORKDIR /root/darwin_core_viewer

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Set renv cache location: change default location of cache to project folder
# see documentation for Multi-stage builds => https://cran.r-project.org/web/packages/renv/vignettes/docker.html
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE=renv/.cache

# Restore renv packages
RUN R -e "renv::restore()"

#FROM ghcr.io/firms-gta/darwin_core_viewer-cache
# Copy the rest of the application code
COPY  . .

# Create directories for configuration
RUN mkdir -p /etc/darwin_core_viewer/

# Expose port 3838 for the Shiny app
EXPOSE 3838
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/darwin_core_viewer'"]