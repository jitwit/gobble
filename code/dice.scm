(define DICE
  '("NAEAEG"
    "EGNWEH"
    "CSOAHP"
    "LVERDY"
    "TOATOW"
    "PKAFFS"
    "HRVTWE"
    "HQUMNI"
    "EITSSO"
    "RLTYET"
    "TITSYD"
    "LXEDIR"
    "TOIMCU"
    "BAOBOJ"
    "NLNHZR"
    "ENSIEU"))

(define (roll die)
  (string-ref die (random (string-length die))))
