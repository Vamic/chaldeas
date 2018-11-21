# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://chaldeas.surge.sh)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
Install [Elm](https://guide.elm-lang.org/install.html) and [npm](https://www.npmjs.com/get-npm). The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [Elm IDE](https://marketplace.visualstudio.com/items/sbrink.elm) extension. 

To run a local development instance of the site, use `npm run dev`. To generate the sitemap, run `npm run sitemap` and copy and paste the content from the browser window to [publish/sitemap.txt](publish/sitemap.txt). **Always regenerate the sitemap if you add a Craft Essence or Servant.**

Pushing your commits to the repository automatically uploads your latest build to the server. It takes [Travis CI](https://travis-ci.com/) a few minutes to deploy everything up through [Surge](surge.sh), so changes will not be reflected immediately.

To test the database's information against [GrandOrder.Wiki](https://grandorder.wiki), run `npm run tests`. The page will only work if you temporarily disable Cross-Origin security policies, which is easiest to do with a browser extension like [this one for Firefox](https://addons.mozilla.org/en-US/firefox/addon/cors-everywhere/) or [this one for Chrome](https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi?hl=en).

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.elm). If you aren't sure what an effect means, check its text description in [Class.Show](src/Class/Show.elm). Before adding a new skill effect, make sure it isn't already on the list.
