{ stdenv, fetchgit, chez, chez-hemlock }:

stdenv.mkDerivation {
  pname = "chez-euler";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/chez-euler.git";
    rev = "042bdf848338bc99f8bc62347d221013f3bd5f45";
    sha256 = "0mrq4xmrf5np2fzfvjjylj90gzzlbnn6wjdx5nmncjb3zni81xhm";
  };

  buildInputs = [ chez chez-hemlock ];

  buildPhase = ''
    export CHEZSCHEMELIBDIRS=${chez-hemlock}/lib/csv9.5-site/
    make prefix=$out chez=${chez}/bin/scheme
  '';

  installPhase = ''
    make install prefix=$out chez=${chez}/bin/scheme
  '';

  meta = {
    description = "Various numerical procedures written while solving project euler problems";
    homepage = https://github.com/jitwit/chez-euler/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
