# -*- mode: snippet; require-final-newline: nil -*-
# name: <child> type
# key: ch.t
# --
`(save-excursion (if (>= (forward-line -2) 0)
                      (if (c-next-line-empty-p)
                           (setq last-line-empty t)
                         (setq last-line-empty nil)))
  nil)`<child type="title${1:bar}">
$0
</child>`(if (and last-line-empty
                  (not (c-next-line-empty-p))) "\n")`