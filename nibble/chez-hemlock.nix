{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "3e5447241cf62fc2437d81ffe9e7b8b29b445dd0";
    sha256 = "09hqhkgg1c6hfnf5h9d9iqgznwy76kldjmra6h5jvwlbfsw17i53";
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
