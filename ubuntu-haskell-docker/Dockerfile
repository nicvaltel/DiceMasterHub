FROM ubuntu:22.04


# COPY . /app
# WORKDIR /app

RUN apt-get update && apt-get -y install cabal-install hpack libghc-wai-websockets-dev libghc-zlib-dev  libghc-zlib-bindings-dev libpq-dev golang-go git  && cabal update && 

# RUN cd /app/Schwertbot && rm Schwertbot.cabal && hpack && cabal update && cabal build



# EXPOSE 8080  EXPOSE 8081
# EXPOSE 443
# EXPOSE 80
# EXPOSE 88
# EXPOSE 8443


# ENTRYPOINT [ "/bin/bash", "-l", "-c"]


# CMD ./Schwertbot-exe


# cd Schwertbot
# rm Schwertbot.cabal
# hpack  
# cabal update
# cabal install record-dot-preprocessor
# cabal build