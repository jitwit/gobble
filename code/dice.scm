(define *DICE*
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

(define size (isqrt (length *DICE*)))

(define (roll die)
  (string-ref die (random (string-length die))))
