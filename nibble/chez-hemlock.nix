{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "5506a83ef53cd469449190862e82de0ee69b5a66";
    sha256 = "18im3a891136d8kf88i3br56q3d10c120c4b711pn0bc7izgc3w5";
  };

  buildInputs = [ chez ];

  buildPhase = "make chez=${chez}/bin/scheme";

  installPhase = "make install out=$out/lib/csv-site";

  meta = {
    description = "Trees and other structures";
    homepage = https://github.com/jitwit/chez-hemlock/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.asl20;
  };

}
