# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://jnbooth.github.io/chaldeas/#)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) extension. [Pulp](https://github.com/purescript-contrib/pulp#installation), PureScript's build tool, is also required. The reporting frontend [purescript-pulp](https://github.com/natefaubion/purescript-psa) is recommended. Once these prerequisites are installed, `./build.sh` will build CHALDEAS.

In 

~~~~ bower install ~~~~

followed by 

~~~~ pulp build --to js/chaldeas.js --no-check-main --modules Export` ~~~~

## Skill Effects
Skill effects are enumerated at the top of src/Database/Skill.purs. Before adding a new skill effect, make sure it isn't already on the list.
