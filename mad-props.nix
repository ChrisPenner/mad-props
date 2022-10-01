{ mkDerivation, base, containers, lens, lib, logict, MonadRandom
, mtl, psqueues, random, random-shuffle, raw-strings-qq
, transformers
}:
mkDerivation {
  pname = "mad-props";
  version = "0.2.1.0";
  sha256 = "0cz01majj5m0a9wszdzlz4arzf3xzppi835k2xrkd3hpzl2fxpiv";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens logict MonadRandom mtl psqueues random
    random-shuffle raw-strings-qq transformers
  ];
  executableHaskellDepends = [
    base containers lens logict MonadRandom mtl psqueues random
    random-shuffle raw-strings-qq transformers
  ];
  homepage = "https://github.com/ChrisPenner/mad-props#readme";
  description = "Monadic DSL for building constraint solvers using basic propagators";
  license = lib.licenses.bsd3;
  mainProgram = "sudoku-exe";
}
