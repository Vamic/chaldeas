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

Once you've done all that, build the site with  [scripts/build.sh]. In the `publish` directory, you can run the site in developer mode with `elm reactor` and publish it with [scripts/publish.sh].

To test the database's information against [GrandOrder.Wiki](https://grandorder.wiki), run [scripts/test.sh].

To generate the sitemap, run [scripts/sitemap.sh], go to [http://localhost:8000/index.html], and copy and paste the content into [publish/sitemap.txt].

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.elm). Before adding a new skill effect, make sure it isn't already on the list.
