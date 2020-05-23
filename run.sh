echo "BUILDING IMAGE" && \
docker build -t covid1 ./ && \
echo "BUILD COMPLETE" && \
echo "STARTING CONTAINER" && \
docker run -it covid1 /bin/bash
