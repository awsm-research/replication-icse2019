# Ubuntu 14.04

useradd awsmdocker \
&& mkdir /home/awsmdocker \
&& chown awsmdocker:awsmdocker /home/awsmdocker \
&& addgroup awsmdocker staff

sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get -y install r-base libcurl4-gnutls-dev libcurl4-openssl-dev libxml2-dev libssl-dev

R -e "install.packages('reshape', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('ggplot2', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('plyr', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('effsize', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('randomForest', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('doParallel', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('gridExtra', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('grid', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('Hmisc', repos = 'http://cran.rstudio.com/')"
R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/')"
R -e "devtools::install_github('awsm-research/Rnalytica')"
exit

docker commit -m "create an Rnalytica docker" 8bc03efd0f8a rnalytica
docker tag d01c03379897 awsmdocker/rnalytica:latest
docker push awsmdocker/rnalytica

