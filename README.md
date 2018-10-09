---
permalink: /about
---
# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://jnbooth.github.io/chaldeas/#)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
Install [Elm](https://guide.elm-lang.org/install.html). The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [Elm IDE](https://marketplace.visualstudio.com/items/sbrink.elm) extension. To build the project, run:

```sh
elm make src/Main.elm --output=js/chaldeas.js
```

For testing purposes, you'll need to set up a staging copy:

```sh
mkdir chaldeas
mkdir chaldeas/js
cp -r img/ chaldeas/img/
```

Once the staging copy is set up, run the test server with:

``sh
elm make src/Main.elm --output chaldeas/js/chaldeas.js && elm reactor
``

and access it at [http://localhost:8000/index.html](http://localhost:8000/index.html).

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.purs). Before adding a new skill effect, make sure it isn't already on the list.
