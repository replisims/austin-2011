FROM rocker/tidyverse:4.1.3
LABEL maintainer="Anna Lohmann <anna@lohmann-web.net>"


##### Install system libraries
ARG DEB_PKGS='libz-dev libssl-dev libxml2-dev libcurl4-openssl-dev curl python3 python3-pip libpython3-dev'
RUN apt-get -qy update && \
    apt-get -qy install --no-install-recommends $DEB_PKGS


WORKDIR /tmp
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
RUN unzip awscliv2.zip
RUN ./aws/install


##### Installl python packages
RUN ln -sv python3 /usr/bin/python

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt


##### Installl R packages
WORKDIR /app

# COPY packrat packrat
# COPY .Rprofile .

# RUN printf '\
#   install.packages("packrat") \n\
#   packrat::restore() \n\
# ' | R

# RUN printf '\
#   uninstall.packages("Rcpp") \n\
# ' | R


RUN printf '\
  install.packages(c("reticulate", "futile.logger")) \n\
' | R
  # install.packages(c("reticulate", "Rcpp", "tidyverse")) \n\
  # install.packages("Rcpp") \n\



# ##### Install our application
COPY R R
COPY cloud cloud
