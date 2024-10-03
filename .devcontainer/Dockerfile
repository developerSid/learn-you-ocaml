FROM ocaml/opam:fedora-40-ocaml-5.2 

USER root

RUN dnf update -y &&\
    dnf remove -y docker \
        docker-client \
        docker-client-latest \
        docker-common \
        docker-latest \
        docker-latest-logrotate \
        docker-logrotate \
        docker-selinux \
        docker-engine-selinux \
        docker-engine && \
   dnf install -y neovim git
