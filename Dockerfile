FROM openjdk:latest

RUN curl -sL https://deb.nodesource.com/setup_7.x | bash - \
        && apt-get install -y nodejs make gcc
