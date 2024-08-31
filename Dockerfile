# Use the official R Shiny image as the base image
FROM rocker/shiny:latest

# Set the working directory inside the container
WORKDIR /srv/shiny-server/

# Copy the current directory contents into the container at /srv/shiny-server/
COPY . /srv/shiny-server/

# Install renv and restore R package dependencies
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Run the Shiny application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
