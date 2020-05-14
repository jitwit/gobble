{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "ef32f8e181dcd45d6b4dc183f9724304ed4a9ac2";
    sha256 = "1rq5x1bcslxhgwfjz0rgws4j302yl5qkbmbwd08znpcxyl41vaya";
  };

  buildInputs = [ chez ];

  buildPhase = ''
    make prefix=$out CHEZ=${chez}/bin/scheme
  '';

    installPhase = ''
    make install prefix=$out CHEZ=${chez}/bin/scheme
  '';

  meta = {
    description = "This package contains implementations of tree structures and others";
    homepage = https://github.com/jitwit/chez-hemlock/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
