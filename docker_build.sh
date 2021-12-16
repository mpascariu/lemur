# build docker image
docker build -t lemur /research/git/doug-leasure/lemur/

# run docker container
docker run -dp 3838:3838 lemur