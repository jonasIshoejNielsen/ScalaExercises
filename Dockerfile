# The original file created by Anders  Fischer-Nielsen
# Additions by Andrzej Wąsowski

FROM ubuntu:20.04

RUN apt-get update 
RUN apt-get -y install curl gnupg2
RUN DEBIAN_FRONTEND="noninteractive" apt-get -y install tzdata
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" >> /etc/apt/sources.list.d/sbt.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update 
RUN apt-get -y install git openjdk-11-jdk-headless neovim emacs nano mcedit jed joe sbt
RUN mkdir adpro

RUN echo 'echo \* ' >> ~/.bashrc
RUN echo 'echo \* Welcome to stable testing environment for adpro' >> ~/.bashrc
RUN echo 'echo \* Installed text editors: vim emacs nano mcedit jed joe' >> ~/.bashrc
RUN echo 'echo \* Run docker with: docker run -it --volume=PATH_TO_ADPRO_GIT_CHECKOUT:/adpro --workdir=/adpro adpro' >> ~/.bashrc
RUN echo 'echo \* Then the course material will be available in adpro/ and you can edit on the host machine' >> ~/.bashrc
RUN echo 'echo \* Type "sbt" to start working with sbt, then use test, compile, run' >> ~/.bashrc
RUN echo 'echo \* Type "sbt console" if you want to experiment quickly with Scala REPL' >> ~/.bashrc
RUN echo 'echo \* ' >> ~/.bashrc

ENTRYPOINT ["/bin/bash"]
