# Use rocker/shiny as the base image (includes R and Shiny Server)

#FROM rocker/shiny:latest
FROM rocker/shiny:4.4.3

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('shiny', 'dplyr', 'forcats', 'ggplot2', 'here', 'maps', 'sf'), repos='https://cran.rstudio.com/')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]

