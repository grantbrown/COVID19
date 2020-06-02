echo "BUILDING IMAGE" && \
docker build -t covid1 ./ && \
echo "BUILD COMPLETE" && \
echo "STARTING CONTAINER" && \
docker run -v ${PWD}/artifacts:/home/Analysis/artifacts -t covid1 /home/Analysis/BatchAnalysis_V1/run.sh
wait
rm -f .RData
wait
echo "REBUILDING IMAGE" && \
docker build -t covid1 ./ && \
rm -f .RData
R CMD BATCH --no-save finalize_results.R
wait
docker run -v ${PWD}/:/home/basefolder -t covid1 /home/Analysis/GetLatestR0Figure/renderR0.sh
docker run -v ${PWD}/:/home/basefolder -t covid1 /home/Analysis/BatchAnalysis_V1/renderIdx.sh
wait
rm -f .RData
R CMD BATCH --no-save update_index.R


