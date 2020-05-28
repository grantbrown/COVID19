FROM ubuntu
MAINTAINER grant.brown73@gmail.com

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y && \
 apt-get upgrade -y && \
 apt-get install -y apt-utils && \
 apt-get upgrade -y && \
 apt-get install -y build-essential git && \
 apt-get install -y vim && \
 apt-get install -y libssl-dev && \
 apt-get install -y libcurl4-openssl-dev && \
 apt-get install -y curl

RUN apt-get install apt-transport-https software-properties-common -y

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' 

RUN apt-get install r-base -y
RUN apt-get install r-base-dev -y

RUN apt-get install pandoc -y

COPY ./Scripts/install.R /home/install.R
RUN eval "$(R CMD BATCH /home/install.R)"
COPY ./Scripts/installPkg.R /home/installPkg.R
RUN cd /home && Rscript installPkg.R readxl
RUN cd /home && Rscript installPkg.R openxlsx
RUN cd /home && Rscript installPkg.R optparse

RUN apt-get install git -y

COPY ./Project/ABSEIR /home/Analysis/ABSEIR

RUN cd /home/Analysis && R CMD INSTALL ./ABSEIR

COPY ./Project /home/Analysis

RUN cd /home/Analysis/Data && rm -rf ./COVID-19 && git clone https://github.com/CSSEGISandData/COVID-19.git COVID-19
RUN cd /home/Analysis/Data && rm -rf ./covid-19-data && git clone https://github.com/nytimes/covid-19-data.git covid-19-data

RUN apt-get install libxml2-dev -y
RUN cd /home && Rscript installPkg.R kableExtra


#RUN eval "cd  /home/Analysis && $(R CMD BATCH ./compile.R)"
#RUN /bin/bash


