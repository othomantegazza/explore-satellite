FROM rocker/r-ver:4.2.2

ENV OS_TYPE=aarch64
ENV RENV_VERSION=0.16.0

ARG CONDA_ENV=r-py-vscode
ENV CONDA_ENV=$CONDA_ENV

ARG PYTHON_VER=3.8
ENV PYTHON_VER=$PYTHON_VER

RUN mkdir project

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    jq \
    libxml2-dev \
    g++-11 \
    libz-dev \
    freetype2-demos \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    make \
    fontconfig \
    libfribidi-dev \
    libharfbuzz-dev \
    libfontconfig1-dev \
    locales \
    git \
    vim \
    libgit2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*


COPY requirements.txt requirements.txt

RUN apt update && apt-get install -y --no-install-recommends \
    software-properties-common 
    
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-${OS_TYPE}.sh -O ~/miniconda.sh \
    && /bin/bash ~/miniconda.sh -b -p /opt/conda \
    && export PATH=/opt/conda/bin:$PATH \
    && conda init bash \
    && conda install conda-build \
    && conda install pip \
    && pip install -r requirements.txt \
    && conda install -y -c conda-forge libstdcxx-ng

RUN . /root/.bashrc \
    && conda create -y --name $CONDA_ENV python=$PYTHON_VER

RUN echo "conda activate $CONDA_ENV" >> ~/.bashrc

# install RENV
RUN R --vanilla -s -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'));remotes::install_github('rstudio/renv@${RENV_VERSION}')"
