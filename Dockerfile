FROM estat/r-dev
LABEL author="estat"
LABEL project="rbokeh"

WORKDIR /usr/src/

ARG GITHUB_PAT=${GITHUB_PAT}

COPY renv.lock .

RUN [ "R", "-e", "renv::restore(prompt=FALSE)" ]

COPY .lintr .
COPY DESCRIPTION .
COPY NAMESPACE .

COPY ./src/ ./src/
COPY ./inst/ ./inst/
COPY ./tests/ ./tests/
COPY ./R/ ./R/
