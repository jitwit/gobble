{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "c7e69d2d6e3de2db899774a14c1773f7d33411d5";
    sha256 = "1rhkn2brvp7z6mymakiai3kv984znvw62q8ln7svqkpbc83dibv1";
  };

  buildInputs = [ chez ];

  buildPhase = "make chez=${chez}/bin/scheme";

  installPhase = "make install out=$out/lib/csv9.5-site";

  meta = {
    description = "Trees and other structures";
    homepage = https://github.com/jitwit/chez-hemlock/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.asl20;
  };

}
