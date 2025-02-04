FROM centos:7.9.2009

ARG GHC_VERSION=9.4.8
ARG CABAL_VERSION=3.10.2.0

RUN   yum -y remove git \
  &&  yum -y install \
        https://repo.ius.io/ius-release-el7.rpm \
        https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
  &&  yum install -y \
        libsecret pcre2 sudo gcc gcc-c++ gmp gmp-devel make \
        ncurses xz perl zstd ncurses-devel zlib-devel \
        libsecret pcre2 git236.x86_64 sqlite-devel xz-devel \
        openssl-devel libtool automake wget curl unzip cppunit-devel

ENV LANG en_US.utf-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.utf-8
ENV LC_MESSAGES C

RUN mkdir -p /work

WORKDIR /work

RUN   wget --no-check-certificate https://github.com/openpbs/openpbs/releases/download/v19.1.3/pbspro_19.1.3.centos_7.zip \
  &&  unzip pbspro_19.1.3.centos_7.zip \
  &&  cd pbspro_19.1.3.centos_7 \
  &&  yum install -y ./pbspro-server-19.1.3-0.x86_64.rpm ./pbspro-server-19.1.3-0.x86_64.rpm

WORKDIR /root

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
  | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION="${GHC_VERSION}" \
    BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL_VERSION}" \
    sh
ENV PATH /root/.local/bin:/root/.ghcup/bin:${PATH}

WORKDIR /work

COPY ./drmaav2 /work/drmaa2

RUN cd drmaa2 && ./autogen.sh && ./configure \
&& make && make install \
&& mkdir -p /usr/local/lib && cp ./api/c-binding/drmaa2.h /usr/local/include

ENV LD_LIBRARY_PATH "/usr/local/lib:${LD_LIBRARY_PATH}"
ENV DYLD_LIBRARY_PATH "/usr/local/lib:${DYLD_LIBRARY_PATH}"

RUN /opt/pbs/libexec/pbs_postinstall
RUN sudo chmod 4755 /opt/pbs/sbin/pbs_iff /opt/pbs/sbin/pbs_rcp
RUN sed -i 's/buildkitsandbox/localhost/' /var/spool/pbs/mom_priv/config
RUN sed -i 's/buildkitsandbox/localhost/' /etc/pbs.conf

WORKDIR /root
RUN rm -rf /work
