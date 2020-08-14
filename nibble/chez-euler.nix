{ stdenv, fetchgit, chez, chez-hemlock }:

stdenv.mkDerivation {
  pname = "chez-euler";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/chez-euler.git";
    rev = "5a50517ceecdbf9c0f0e252c8b2e5d4dd59048ac";
    sha256 = "1g0kyb5sr7swm490h95r16kmgjjaz0n7ri6x97vjd40apfp63rjj";
  };

  buildInputs = [ chez chez-hemlock ];

  checkPhase = "make check";

  buildPhase = ''
    export CHEZSCHEMELIBDIRS=${chez-hemlock}/lib/csv-site/
    make chez=${chez}/bin/scheme
  '';

  installPhase = "make install out=$out/lib/csv-site";
  
  meta = {
    description = "Various numerical procedures written while solving project euler problems";
    homepage = https://github.com/jitwit/chez-euler/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
