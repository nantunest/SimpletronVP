FROM ubuntu:20.04
ENV DEBIAN_FRONTEND noninteractive

# Use "bash" as replacement for	"sh"
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

RUN apt-get update

# Apt packages installation
RUN apt-get install -y sudo
RUN apt-get install -y net-tools
RUN apt-get install -y iputils-ping
RUN apt-get install -y vim
RUN apt-get install -y --fix-missing clang
RUN apt-get install -y libsystemc-dev
RUN apt-get install -y build-essential
RUN apt-get install -y bsdmainutils
RUN apt-get install -y git
RUN apt-get install -y curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN apt-get install -y gnuplot

RUN apt-get install -y texlive-full
RUN apt-get install -y gtkwave
RUN apt-get install -y tmux

# Devel user configuration
RUN useradd -mNs /bin/bash -u 1000 -p 'devel' devel
RUN echo "devel:devel" | chpasswd
RUN usermod -aG sudo devel

USER devel

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.6.2.0

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo "source /home/devel/.ghcup/env" >> ~/.bashrc
ENV PATH="/home/devel/.ghcup/bin:$PATH"

RUN cabal update
RUN cabal install --lib forsyde-shallow
RUN cabal install homplexity
RUN ghcup install hls
