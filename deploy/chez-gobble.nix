{ stdenv, chez, chez-hemlock, chez-srfi, chez-euler, chez-matchable }:

stdenv.mkDerivation {
  pname = "chez-gobble";
  version = "0.1";
  src = ./../cobble;

  buildInputs = [ chez chez-hemlock chez-srfi chez-matchable ];

  buildPhase = ''
    export CHEZSCHEMELIBDIRS=${chez-hemlock}/lib/csv9.5-site/
    export CHEZSCHEMELIBDIRS=$CHEZSCHEMELIBDIRS:${chez-euler}/lib/csv9.5-site/
    export CHEZSCHEMELIBDIRS=$CHEZSCHEMELIBDIRS:${chez-srfi}/lib/csv9.5-site/
    make prefix=$out CHEZ=${chez}/bin/scheme
  '';

  installPhase = ''
    mkdir -p $out/lib/csv9.5-site/
    mkdir -p $out/share
    cp *.so $out/lib/csv9.5-site/
    cp share/trie.fasl $out/share/
  '';

  meta = {
    description = "This package solves and generates boggle boards";
    homepage = https://github.com/jitwit/gobble/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
