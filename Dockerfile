FROM swipl
LABEL MAINTAINER="Joaquim Carvalho joaquim@uc.pt"
LABEL pt.uc.joaquimrcarvalho.kleio.server.version=2019.A1

# We need git for the repository handling api using 
# Following based on https://github.com/SamuelDebruyn/docker-debian-git/blob/master/Dockerfile
# with note from https://serverfault.com/questions/618994/when-building-from-dockerfile-debian-ubuntu-package-install-debconf-noninteract

# setup workdir
RUN mkdir -p /root/work/
WORKDIR /root/work/

# install git and slim down image
RUN apt-get -y update && DEBIAN_FRONTEND=noninteractive apt-get -y install git && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man/?? /usr/share/man/??_*

# Install the kleio-server src
COPY ./src /usr/local/timelink/clio/src
WORKDIR /usr/local/timelink/clio/src
EXPOSE 8088 
EXPOSE 4000
CMD ["swipl", "-f" , "serverStart.pl","-g","run_server_forever"]