FROM phadej/ghc:7.10.2

ADD mkdocs /opt/project/
ADD LICENSE /opt/project/
ADD Setup.hs /opt/project/
ADD README.md /opt/project/
ADD gore-and-ash.cabal /opt/project/
ADD stack.yaml /opt/project/
ADD src /opt/project/src

WORKDIR /opt/project

ENTRYPOINT ["./mkdocs", "gore-and-ash", "1.2.0.0", "NCrashed"]
