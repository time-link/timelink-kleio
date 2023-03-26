FROM swipl
LABEL MAINTAINER="Joaquim Carvalho joaquim@uc.pt"

# We need git for the repository handling api using 
# Following based on https://github.com/SamuelDebruyn/docker-debian-qgit/blob/master/Dockerfile
# with note from https://serverfault.com/questions/618994/when-building-from-dockerfile-debian-ubuntu-package-install-debconf-noninteract

# install git and slim down image
RUN apt-get -y update && DEBIAN_FRONTEND=noninteractive apt-get -y install git && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man/?? /usr/share/man/??_*

# Install the kleio-server src
COPY ./src /usr/local/timelink/clio/src
WORKDIR /usr/local/timelink/clio/src

CMD ["swipl", "-f" , "serverStart.pl","-g","run_server_forever"]
