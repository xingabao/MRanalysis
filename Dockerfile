FROM rocker/shiny:4.5.1

MAINTAINER Abao Xing

# Install system dependencies
RUN apt-get update \
	&& apt-get install -y cmake libxml2-dev libssl-dev libgmp-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev git bzip2 libgsl-dev libglpk-dev libigraph-dev gcc-9 g++-9 \
	&& sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 90 --slave /usr/bin/g++ g++ /usr/bin/g++-9 \
	&& sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-13 130 --slave /usr/bin/g++ g++ /usr/bin/g++-13

# Install R packages
RUN Rscript -e 'install.packages(c("purrr", "tibble", "reactable", "ggplot2", "ggthemes", "ggdag", "ggpubr", "ggtext", "venn", "shinymeta", "esquisse", "openxlsx", "bs4Dash", "shinyalert", "shinyjs", "stringr", "spsComps", "colourpicker", "shinyvalidate", "shinyjqui", "sysfonts", "extrafont", "shinydashboard", "shinycssloaders", "formattable", "MendelianRandomization", "DT", "devtools", "BiocManager", "formatR", "coloc", "gdata", "logger", "Rfast", "Rmpfr", "pheatmap"))'

# Install R packages
RUN Rscript -e 'BiocManager::install(c("clusterProfiler", "org.Hs.eg.db", "ComplexHeatmap", "VariantAnnotation", "MungeSumstats"), update = FALSE, ask = FALSE)'

# Install R packages
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}
RUN Rscript -e 'remotes::install_version("RcppEigen", version = "0.3.3.9.3")' \
	&& wget -O /tmp/hyprcoloc-0.0.2.tar.gz https://github.com/jrs95/hyprcoloc/archive/refs/tags/v0.0.2.tar.gz \
	&& R CMD INSTALL /tmp/hyprcoloc-0.0.2.tar.gz \
	&& Rscript -e 'devtools::install_github("MRCIEU/CheckSumStats")' \
	&& Rscript -e 'devtools::install_github("qingyuanzhao/mr.raps")' \
	&& Rscript -e 'devtools::install_github("mrcieu/gwasglue")' \
	&& Rscript -e 'devtools::install_github("WSpiller/MVMR", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = FALSE)' \
	&& Rscript -e 'devtools::install_github("jrs95/geni.plots", build_vignettes = FALSE)' \
	&& Rscript -e 'devtools::install_github("boxiangliu/locuscomparer")' \
	&& Rscript -e 'remotes::install_version("RcppEigen", version = "0.3.3.9.3")' \
	&& Rscript -e 'remotes::install_github("xingabao/MRanalysisBase")' \
	&& Rscript -e 'remotes::install_github("xingabao/GWASkitR")'

# Install PWCoCo
RUN mkdir -p /tools && cd /tools \
	&& git clone https://github.com/jwr-git/pwcoco.git \
	&& cd pwcoco \
	&& mkdir build && cd build \
	&& cmake .. \
	&& make
	
# Install CAVIAR 
RUN cd /tools \
	&& git clone https://github.com/fhormoz/caviar.git \
	&& cd caviar/CAVIAR-C++ \
	&& sudo update-alternatives --set gcc /usr/bin/gcc-9 && make
	
# Install bcftools
RUN wget -O /tmp/bcftools-1.22.tar.bz2 https://github.com/samtools/bcftools/releases/download/1.22/bcftools-1.22.tar.bz2 \
	&& tar -xvjf /tmp/bcftools-1.22.tar.bz2 -C /tools \
	&& cd /tools/bcftools-1.22 \
	&& ./configure --prefix=/tools/bcftools-1.22 \
	&& sudo update-alternatives --set gcc /usr/bin/gcc-13 && make -j \
	&& make install

# Install plink
RUN wget -O /tmp/plink_linux_x86_64.zip https://s3.amazonaws.com/plink1-assets/dev/plink_linux_x86_64.zip \
	&& mkdir -p /tools/plink \
	&& unzip /tmp/plink_linux_x86_64.zip -d /tools/plink

# Install magma
RUN wget -O /tmp/magma_v1.10.zip https://vu.data.surfsara.nl/index.php/s/zkKbNeNOZAhFXZB/download \
	&& mkdir -p /tools/magma_v1.10 \
	&& unzip /tmp/magma_v1.10.zip -d /tools/magma_v1.10 \
	&& chmod 777 /tools/magma_v1.10/magma


# 中间文件删除
RUN rm -rf /tmp/* /srv/shiny-server* 

# Copy
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY analysis/ /srv/shiny-server/analysis/
COPY gallery/ /srv/shiny-server/gallery/
COPY plugins/ /srv/shiny-server/plugins/
COPY installation/ /srv/shiny-server/installation/
COPY www/ /srv/shiny-server/www/
COPY Contact.html index.html /srv/shiny-server/
COPY Contact.html index.html /srv/shiny-server/
RUN cp -r /usr/local/lib/R/site-library/MRanalysisBase/extdata /srv/shiny-server/XINGABAO && apt-get -y install libmysqlclient-dev

EXPOSE 8001

# 设置用户
ARG user=shiny
RUN echo "$user:XINGABAO" | chpasswd
WORKDIR /home/$user
USER $user

CMD ["/usr/bin/shiny-server"]