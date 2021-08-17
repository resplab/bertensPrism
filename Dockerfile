FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/bertens")'
RUN R -e 'remotes::install_github("resplab/bertensPrism")'
RUN echo "opencpu:opencpu" | chpasswd
