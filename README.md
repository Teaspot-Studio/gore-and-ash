gore-and-ash
============

Core package of game engine called Gore&Ash. The engine has following features:

- based on arrowised FRP ([netwire](https://wiki.haskell.org/Netwire))

- provides high-modularity and reusability. Actually the core can only compose modules that extends engine capabilities.

- actor based style of programming, see [gore-and-ash-actor](https://github.com/Teaspot-Studio/gore-and-ash-actor) module.

- network API over UDP with user controlled reliability, see [gore-and-ash-network](https://github.com/Teaspot-Studio/gore-and-ash-network) module.

- synchronization EDSL that greately simplifies complexity of client-server programming, see [gore-and-ash-sync](https://github.com/Teaspot-Studio/gore-and-ash-sync) module.

- input module via SDL2 library, see [gore-and-ash-sdl](https://github.com/Teaspot-Studio/gore-and-ash-sdl) module.

For complete proof-of-concept, see [gore-and-ash-demo](https://github.com/Teaspot-Studio/gore-and-ash-demo) repo that contains implementation of simple game.

Making your own module
======================

You can generate backbone of core module with `stack`:

```
stack new gore-and-ash-testtemp ./gore-and-ash-module.hsfiles -p module-name:TestTemp -p module-name-lower:testtemp --solver
```