# Keyboard
## GHC version used
**ghc   9.8.2      base-4.19.1.0**

## To start the virtual keyboard using cabal
First pull the spell-checker submodule with:
```
git submodule update --init --recursive
```
You can then start the keyboard with:
```
cabal run keyboard
```

# Using the build environment
If you prefer to use a (podman) container to build the keyboard, you can do so with:
```
podman build . -f build-env/Containerfile -t ghc
podman run -it ghc
```
Once in the container, you can build the program with:
```
cabal build keyboard
```
