FROM demandshiny_base

# Instalação dos pacotes necessários
RUN apt-get update && apt-get install -y \
    cron \
    nano \
    tdsodbc \
    odbc-postgresql \
    libsqliteodbc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN apt-get update \
 && apt-get install --yes --no-install-recommends \
        apt-transport-https \
        curl \
        gnupg \
        unixodbc-dev \
 && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
 && curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
 && apt-get update \
 && ACCEPT_EULA=Y apt-get install --yes --no-install-recommends msodbcsql17 \
 && install2.r odbc \
 && apt-get clean

# Copia o arquivo de configuração do ODBC
COPY odbc.ini /etc/odbc.ini

# Copia o arquivo de bloqueio do renv e restaura o ambiente
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'

# Copia o pacote da aplicação e instala
COPY demandshiny_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz

# Expõe a porta 80 e inicia a aplicação
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(demandshiny);demandshiny::run_app()"
