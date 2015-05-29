To build and run the project:
```bash
git clone --recursive https://github.com/qwe2/try-agda
cd try-agda
cabal sandbox init
cabal install -j
#if the above fails: cabal install -j --reorder-goals
cabal run -- -p 9000
```
