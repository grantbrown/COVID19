#!/bin/bash
cd ../COVID-19 & \
	git pull origin master & \
	cd .. & \
	cd ./covid-19-data & \
	
cd /home/Analysis/Data/covid-19-data
git pull origin master
cd /home/Analysis/BatchAnalysis_V1
R CMD BATCH render.R 
wait
cp -r ./* ../artifacts
  
