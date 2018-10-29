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

Once you've done all that, build the site with  `scripts/build.sh`.

For development purposes, you'll need to set up a staging copy:

```sh
mkdir chaldeas
mkdir chaldeas/js
cp -r img/ chaldeas/img/
```

Once the staging copy is set up, run the development server with `scripts/run.sh` and access it at [http://localhost:8000/index.html](http://localhost:8000/index.html).

To test the database's information against [GrandOrder.Wiki](https://grandorder.wiki), run `scripts/test.sh`.

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.elm). Before adding a new skill effect, make sure it isn't already on the list.
