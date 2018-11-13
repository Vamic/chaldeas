# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://chaldeas.surge.sh)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
Install [Elm](https://guide.elm-lang.org/install.html). The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [Elm IDE](https://marketplace.visualstudio.com/items/sbrink.elm) extension. 

You will also need [npm](https://www.npmjs.com/get-npm) in order to install [Uglify](https://www.npmjs.com/package/uglify-js) and [Surge](https://www.npmjs.com/package/surge):

```sh
sudo npm install --global uglify-js surge
```

Once you've done all that, build the site with [scripts/build.sh](scripts/build.sh). To run a local development instance of the site, use [scripts/run.sh](scripts/run.sh) and access it at [http://localhost:8000/200.html](http://localhost:8000/200.html). To publish it to the [server](https://chaldeas.surge.sh), use [scripts/publish.sh](scripts/publish.sh). To generate the sitemap, run [scripts/sitemap.sh](scripts/sitemap.sh), go to [http://localhost:8000/index.html](http://localhost:8000/index.html), and copy and paste the content into [publish/sitemap.txt](publish/sitemap.txt).

To test the database's information against [GrandOrder.Wiki](https://grandorder.wiki), run [scripts/test.sh](scripts/test.sh) and go to [http://localhost:8000/index.html](http://localhost:8000/index.html). The page will only work if you temporarily disable Cross-Origin security policies, which is easiest to do with a browser extension like [this one for Firefox](https://addons.mozilla.org/en-US/firefox/addon/cors-everywhere/) or [this one for Chrome](https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi?hl=en),

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.elm). If you aren't sure what an effect means, check its text description in [Class.Show](src/Class/Show.elm). Before adding a new skill effect, make sure it isn't already on the list.
