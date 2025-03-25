Installation Instructions for macOS

Install the OCaml Graphics library by running this command:
```
opam install graphics
```
Install XQuartz using Homebrew by running this command:
```
brew install xquartz
```

After installing XQuartz, configure the virtual display by running:
```
export DISPLAY=:0
```

To build and exectue the program, run:

```
dune build
dune exec bin/main.exe
```
