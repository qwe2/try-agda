Need to manually unpack Agda because it has a dependency on cpphs that conflicts with some other package's.

To get the right Agda version:
 * `cabal unpack Agda`
 * modify `Agda.cabal` in the unpacked folder so that it allows version `1.19` of `cpphs`.

To build and run the project:
```bash
git clone https://github.com/qwe2/try-agda
cd try-agda
cabal sandbox init
cabal add-source ../path/to/unpacked/Agda-2.4.2.2     # or whichever version you unpacked
cabal install -j
cabal run -- -p 9000
```
