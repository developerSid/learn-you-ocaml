FROM ocaml/opam:fedora-40-ocaml-5.2 

ARG DOCKER_GID=999
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
    docker-engine

RUN dnf install -y neovim git dnf-plugins-core zsh curl lsd direnv bat
RUN groupadd -g $DOCKER_GID docker || groupmod -g $DOCKER_GID docker

RUN dnf-3 config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo && \
    dnf install -y docker-ce-cli docker-compose-plugin && \
    chown -R opam:opam /home/opam && \
    usermod -aG docker opam


USER opam

RUN RUNZSH=no sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
RUN mkdir -p /home/opam/.config && \
    git clone https://github.com/developerSid/nvim.git /home/opam/.config/nvim && \
    git clone https://github.com/developerSid/dotfiles /home/opam/.dotfiles && \
    /home/opam/.dotfiles/install.sh && \
    opam install dune merlin ocamlformat ocaml-lsp-server && \
    echo "eval \$(opam env)" >> /home/opam/.localConfig

WORKDIR /learn-you-ocaml
