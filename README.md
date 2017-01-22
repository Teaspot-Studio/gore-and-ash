gore-and-ash
============

Core package of game engine called Gore&Ash. The engine has following features:

- based on monadic FRP ([reflex](https://hackage.haskell.org/package/reflex))

- provides high-modularity and reusability. Actually the core can only compose modules that extends the engine capabilities.

- network API with user controlled reliability, see [gore-and-ash-network](https://github.com/Teaspot-Studio/gore-and-ash-network) module.

- synchronization EDSL that greately simplifies complexity of client-server programming, see [gore-and-ash-sync](https://github.com/Teaspot-Studio/gore-and-ash-sync) module.

- input module via SDL2 library, see [gore-and-ash-sdl](https://github.com/Teaspot-Studio/gore-and-ash-sdl) module.

For complete proof-of-concept, see [gore-and-ash-demo](https://github.com/Teaspot-Studio/gore-and-ash-demo) repo that contains implementation of simple game.

Making your own module
======================

You can generate backbone of core module with `stack`:

``` bash
stack new gore-and-ash-mymodule ./gore-and-ash-module.hsfiles \
  -p module-name:Test \
  -p module-docs-name:test \
  -p field-prefix:test \
  -p github-username:teaspot-studio \
  -p current-year:2017 \
  -p author-name:ncrashed \
  -p author-email:ncrashed@gmail.com \
  -p copyright:Anton Gushcha 2017 \
  --solver
```