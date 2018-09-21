This folder contains unit testing for CHALDEAS. Specifically, it tests all properties against an external database (except Traits, since I have not yet found a reliable external database of EN-only Traits). At present, the database used is [GrandOrder.Wiki](grandorder.wiki).

When running tests, the following command line arguments are accepted:
`--ce <amount>` to limit the number of Craft Essences tested,
`--s <amount>` to limit the number of Servants tested. Since the test uses npm, these arguments can be passed through as e.g. `npm run test -- -- --ce 0 --s 15`.

Note: What happens in this folder stays in this folder. When CHALDEAS is compiled, nothing here should be included.
