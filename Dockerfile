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



# Set the working directory
WORKDIR /root/darwin_core_viewer

# ARG defines a constructor argument called RENV_PATHS_ROOT. Its value is passed from the YAML file. An initial value is set up in case the YAML does not provide one
ARG RENV_PATHS_ROOT=/root/.cache/R/renv
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

# Set environment variables for renv cache
# Set renv cache location: change default location of cache to project folder
# see documentation for Multi-stage builds => https://cran.r-project.org/web/packages/renv/vignettes/docker.html
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}


# Echo the RENV_PATHS_ROOT for logging
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}"
RUN echo "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}"

# Define the build argument for the hash of renv.lock to stop cache if renv.lock has changed
ARG RENV_LOCK_HASH
RUN echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}"

# Make a directory in the container to create the renv cache directory
RUN mkdir -p ${RENV_PATHS_ROOT}

# Install renv package that records the packages used in the shiny app (we might specify the version of renv package)
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 

# Copy the rest of the application code
COPY . .

# Create directories for configuration
RUN mkdir -p /etc/darwin_core_viewer/

# Expose port 3838 for the Shiny app
EXPOSE 3838
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/darwin_core_viewer'"]