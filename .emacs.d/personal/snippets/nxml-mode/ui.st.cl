# -*- mode: snippet; require-final-newline: nil -*-
# name: <class .../>
# key: st.cl
# --
<class name="${1:$$(replace-regexp-in-string " " "-" yas-text)}"/>${0:$$(read-only-mode)}