;; Face family
(setq my/regular-face-family "deja vu serif")
(setq my/modeline-face-family my/regular-face-family)

;; Face height
(setq my/regular-face-height 110)
(setq my/modeline-face-height (ceiling (* 0.8 my/regular-face-height)))

;; Scaling for modeline alignment
;; 1080p ~1.29
;; 1440p ~1.22
(setq my/modeline-face-factor 1.29)
