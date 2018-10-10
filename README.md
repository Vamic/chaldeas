# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://jnbooth.github.io/chaldeas/#)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
Install [Elm](https://guide.elm-lang.org/install.html). The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [Elm IDE](https://marketplace.visualstudio.com/items/sbrink.elm) extension. 

You will also need [npm](https://www.npmjs.com/get-npm) in order to install uglify-js:

```sh
sudo npm install uglify-js --global
```

To build the project, run:

```sh
elm make src/Main.elm --output=js/chaldeas.js --optimize && uglifyjs js/chaldeas.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=js/chaldeas.js
```

For testing purposes, you'll need to set up a staging copy:

```sh
mkdir chaldeas
mkdir chaldeas/js
cp -r img/ chaldeas/img/
```

Once the staging copy is set up, run the test server with:

```sh
elm make src/Main.elm --output chaldeas/js/chaldeas.js && elm reactor
```

and access it at [http://localhost:8000/index.html](http://localhost:8000/index.html).

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.elm). Before adding a new skill effect, make sure it isn't already on the list.
