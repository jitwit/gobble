{ stdenv, fetchgit, chez }:

stdenv.mkDerivation {
  pname = "chez-hemlock";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/jitwit/hemlock.git";
    rev = "d26beffa520c7ae91c1f4d9c1aaa0b0708a2a41b";
    sha256 = "0mylkcvi3cdyw95vhn8is4df0p6a31wyc4v1g8a0lv8lwzpi042y";
  };

  buildInputs = [ chez ];

  buildPhase = "make prefix=$out CHEZ=${chez}/bin/scheme";

  installPhase = "mkdir -p $out/lib/csv9.5-site && cp -r *.so $out/lib/csv9.5-site";

  meta = {
    description = "Trees and other structures";
    homepage = https://github.com/jitwit/chez-hemlock/;
    maintainers = [ stdenv.lib.maintainers.jitwit ];
    license = stdenv.lib.licenses.gpl3;
  };

}
