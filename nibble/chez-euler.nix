{ stdenv, fetchgit, chez, chez-hemlock }:

stdenv.mkDerivation {
  pname = "chez-euler";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/chez-euler.git";
    rev = "d83c2ea6df28a28c8f2b430afeeb6c61e3e26c1a";
    sha256 = "113f7zz3z448r4xi1gjzd784xvadyz2h6zpy6wkl2mfcdk6iph8c";
  };

  buildInputs = [ chez chez-hemlock ];

  checkPhase = "make check";

  buildPhase = ''
    export CHEZSCHEMELIBDIRS=${chez-hemlock}/lib/csv9.5-site/
    make chez=${chez}/bin/scheme
  '';

  installPhase = "make install out=$out/lib/csv9.5-site";
  
  meta = {
    description = "Various numerical procedures written while solving project euler problems";
    homepage = https://github.com/jitwit/chez-euler/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
