# bundleSRTReader

Grabs SRT paths from Witcher 3 bundles, including DLC. Writes them to a file and sorts the output. Skips the mods folder by default.

# Build and run

You need to download the Haskell Stack:
https://docs.haskellstack.org/en/stable/README/

Then simply run:
`stack build --ghc-options=-O2`
inside the project folder to build with optimizations.

`stack exec bundleSRTReader-exe arg1 arg2` will run the program, you need to supply CLI arguments:
* **arg1** - Game path (without "\" at the end)
* **arg2** - Output name
