This folder contains unit testing for CHALDEAS. Specifically, it tests all properties against an external database (except Traits, since I have not yet found a reliable external database of EN-only Traits). Currently, the database used is [GrandOrder.Wiki](grandorder.wiki). 

When running the tests, the following command line arguments are accepted:
`--ce <amount>` to limit the number of Craft Essences tested,
`--s <amount>` to limit the number of Servants tested.

Note: What happens in this folder stays in this folder. When CHALDEAS is compiled, nothing here should be included.
