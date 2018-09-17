# CHALDEAS
Bringing order to Fate/Grand Order

## Links
[CHALDEAS](https://jnbooth.github.io/chaldeas/#)

## License
BSD 3-Clause, see [LICENSE](https://github.com/jnbooth/chaldeas/blob/master/LICENSE).

## Getting Started
The recommended development environment is [Visual Studio Code](code.visualstudio.com) with the [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) extension. [NPM](https://www.npmjs.com/get-npm) is the package manager. Once NPM is installed, run

```sh
npm run build
```

Similarly, to run tests:

```sh
npm run test
```

## Skill Effects
Skill effects are enumerated at the top of [Database.Skill](src/Database/Skill.purs). Before adding a new skill effect, make sure it isn't already on the list.
