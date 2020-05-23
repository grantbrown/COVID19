#!/bin/bash
cd home/Analysis/Data/COVID-19
git pull origin master
cd /home/Analysis/Data/covid-19-data
git pull origin master
cd /home/Analysis/BatchAnalysis_V1
R CMD BATCH render.R 
wait
cp -r ./* ../artifacts
  
